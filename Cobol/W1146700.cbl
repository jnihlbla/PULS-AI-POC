000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1146700.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   13/01/09.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPPDATERAR INFORMATION FRÅN SI+ (KVITTERINGAR)                   
000900*        SKAPAR LARM FÖR AVVIKELSER                                       
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDC9                                       
001200*        PROGRAMMET UPPDATERAR WDK7                                       
001300*        PROGRAMMET UPPDATERAR WDR2                                       
001400*        PROGRAMMET UPPDATERAR WDR5                                       
001500*        PROGRAMMET UPPDATERAR WDR3                                       
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- KVITTO FRÅN SI+                                            
002900     SELECT W11465                     ASSIGN TO W11467D1.                
003000                                                                          
003100 DATA DIVISION.                                                           
003200 FILE SECTION.                                                            
003300                                                                          
003400 FD  W11465                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700*01  -COPY T335R309      -L.                                              
003800                                                                          
003900                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W1146700'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
004600 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004700                                                                          
004800 77  W-ANTAL-POSTER              PIC 9(7)    VALUE ZERO.                  
004900                                                                          
005000 77  W11465-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W11465                       VALUE 'J'.                   
005200                                                                          
005300*01  -COPY WWDCKONS                                                       
005400                                                                          
005500 01  ARB-AREOR.                                                           
005600                                                                          
005700     03  WS-IDPITEM              PIC X(20).                               
005800     03  FILLER  REDEFINES WS-IDPITEM.                                    
005900         05  FILLER              PIC X(11).                               
006000         05  WS-IDARTNR          PIC X(9).                                
006100     03  FILLER  REDEFINES WS-IDPITEM.                                    
006200         05  FILLER              PIC X(11).                               
006300         05  WS-IDARTNR-NUM      PIC 9(9).                                
006400                                                                          
006500     03  WS-IDHANDLR             PIC 9(4)    VALUE ZERO.                  
006600     03  WS-IDINK                PIC X(4)    VALUE SPACE.                 
006700     03  WS-W11465-IDUSER        PIC X(5)    VALUE SPACE.                 
006800                                                                          
006900     03  WS-LARM-RETUR-X         PIC 9(3)  VALUE 700.                     
007000     03  FILLER REDEFINES WS-LARM-RETUR-X.                                
007100         05  WS-LARM1            PIC 9.                                   
007200         05  WS-RETUR            PIC 9(2).                                
007300     03  WS-LARM-RETUR REDEFINES WS-LARM-RETUR-X  PIC 9(3).               
007400                                                                          
007500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007600 01  FILLER REDEFINES DAGENS-DATUM.                                       
007700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008000                                                                          
008100 01  NEXT-KLOCKSLAG.                                                      
008200     03  WS-NXT-KL                 PIC 9(6).                              
008300     03  FILLER                    REDEFINES WS-NXT-KL.                   
008400       05  WS-NXT-KL-TT            PIC 9(2).                              
008500       05  WS-NXT-KL-MM            PIC 9(2).                              
008600       05  WS-NXT-KL-SS            PIC 9(2).                              
008700                                                                          
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900*                                                                         
009000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009400                                                                          
009500*    --- PARAMETRAR TILL ABEND                                            
009600                                                                          
009700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010000                                                                          
010100 01  FELTEXT.                                                             
010200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010400                                                                          
010500*    --- PARAMETRAR TILL POSTSUM                                          
010600*                                                                         
010700*01  -COPY W0005   -PRE  POSTSUM-                                         
010800                                                                          
010900 01  W11465-AREA-START           PIC X(24)   VALUE                        
011000                                 'W11465-AREA-START '.                    
011100                                                                          
011200*01  AREA -COPY T335R309     -PRE W11465-                                 
011300                                                                          
011400                                                                          
011500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011600                                                                          
011700 01  CHKP-VAR.                                                            
011800 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
011900 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
012000 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
012100 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
012200 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
012300 03  CHKP-MAX                    PIC S9(3)   VALUE +500.                  
012400                                                                          
012500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012600 01  NYCKLAR-TILL-DLI.                                                    
012700     03  W-WDC901KY-X.                                                    
012800         05  W-IDDC-C901         PIC X(2)    VALUE SPACE.                 
012900         05  W-IDARTNR-C901      PIC S9(9)   VALUE ZERO COMP-3.           
013000     03  W-IDARTNR-X.                                                     
013100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013200     03  W-IDDC-X.                                                        
013300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013400     03  W-KDLARM-X.                                                      
013500         05  W-KDLARM            PIC S9(3)   COMP-3 VALUE ZERO.           
013600     03  W-KDSEGKEY-X.                                                    
013700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
013800     03  W-WDGX2231-X.                                                    
013900         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
014000         05  W-VALFRI-2231       PIC X(26)    VALUE LOW-VALUE.            
014100     03  W-WDGX2232-X.                                                    
014200         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
014300         05  W-LOW-VALUE-2232    PIC X(3)     VALUE LOW-VALUE.            
014400     03  W-WDGX2223-X.                                                    
014500         05  W-IDHTYP-2223       PIC X(4)     VALUE '2223'.               
014600         05  W-IDANSK-2223       PIC S9(3)    VALUE ZERO COMP-3.          
014700         05  W-LOW-VALUE         PIC X(24)    VALUE LOW-VALUE.            
014800     03  W-WDGXKEY-4579-X.                                                
014900          05 W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
015000          05 W-IDPGM             PIC X(8)    VALUE 'W1146700'.            
015100          05 FILLER              PIC X(18)   VALUE LOW-VALUE.             
015200     03  W-WDGX2263-X.                                                    
015300         05  W-IDHTYP-2263       PIC X(4)    VALUE '2263'.                
015400         05  W-FILLER            PIC X(26)   VALUE LOW-VALUE.             
015500     03  W-TISOP-X.                                                       
015600         05  W-TISOP-2264        PIC S9(5)           COMP-3.              
015700                                                                          
015800*    --- STATUS-KOD FRÅN IMS                                              
015900 01  STATUS-WS                   PIC XX.                                  
016000     88  SEGMENT-FINNS                       VALUE '  '.                  
016100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016300     88  IMS-EJ-OK                           VALUE 'XD'.                  
016400                                                                          
016500 01  GODK-STATUSKODER.                                                    
016600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016700                                                                          
016800 01  ALL-SSA.                                                             
016900     03 SSA1                     PIC X(128).                              
017000     03 SSA2                     PIC X(128).                              
017100     03 SSA3                     PIC X(128).                              
017200                                                                          
017300*    --- IMS FUNKTIONSKODER                                               
017400*01  -COPY W0003                                                          
017500                                                                          
017600                                                                          
017700*    ---  DLI INPUT-OUTPUT AREA                                           
017800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC901'.                      
017900 01  DLI-IO-WDC901.                                                       
018000*    03  -COPY WDC901                                                     
018100                                                                          
018200                                                                          
018300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
018400 01  DLI-IO-WDK701.                                                       
018500*    03  -COPY WDK701                                                     
018600                                                                          
018700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
018800 01  DLI-IO-WDK711.                                                       
018900*    03  -COPY WDK711                                                     
019000                                                                          
019100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
019200 01  DLI-IO-WDK722.                                                       
019300*    03  -COPY WDK722                                                     
019400                                                                          
019500                                                                          
019600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR220'.                      
019700 01  DLI-IO-WDR220.                                                       
019800*    03  -COPY WDGX2232                                                   
019900                                                                          
020000                                                                          
020100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2223'.                    
020200 01  DLI-IO-WDGX2223.                                                     
020300*    03  -COPY WDGX2223                                                   
020400                                                                          
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2224'.                    
020600 01  DLI-IO-WDGX2224.                                                     
020700*    03  -COPY WDGX2224                                                   
020800                                                                          
020900                                                                          
021000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
021100 01  DLI-IO-WDGX4580.                                                     
021200*    03  -COPY WDGX4580                                                   
021300                                                                          
021400                                                                          
021500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
021600 01  DLI-IO-WDK601.                                                       
021700*    03  -COPY WDK601                                                     
021800                                                                          
021900                                                                          
022000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2264'.                    
022100 01  DLI-IO-WDGX2264.                                                     
022200*    03  -COPY WDGX2264                                                   
022300                                                                          
022400                                                                          
022500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2266'.                    
022600 01  DLI-IO-WDGX2266.                                                     
022700*    03  -COPY WDGX2266                                                   
022800                                                                          
022900                                                                          
023000 LINKAGE SECTION.                                                         
023100                                                                          
023200*01  -COPY W0009  -PRE MSG-                                               
023300                                                                          
023400*01  -COPY W0008  -PRE WDC9-                                              
023500     05  FILLER                  PIC X.                                   
023600                                                                          
023700*01  -COPY W0008  -PRE WDK7-                                              
023800     05  FILLER                  PIC X.                                   
023900                                                                          
024000*01  -COPY W0008  -PRE WDR2-                                              
024100     05  FILLER                  PIC X.                                   
024200                                                                          
024300*01  -COPY W0008  -PRE WDR5-                                              
024400     05  FILLER                  PIC X.                                   
024500                                                                          
024600*01  -COPY W0008  -PRE 4579-                                              
024700     05  FILLER                  PIC X.                                   
024800                                                                          
024900*01  -COPY W0008  -PRE WDK6-                                              
025000     05  FILLER                  PIC X.                                   
025100                                                                          
025200*01  -COPY W0008  -PRE WDR2-2-                                            
025300     05  FILLER                  PIC X.                                   
025400                                                                          
025500                                                                          
025600 PROCEDURE DIVISION  USING MSG-PCB WDC9-PCB WDK7-PCB                      
025700                           WDR2-PCB WDR5-PCB 4579-PCB                     
025800                           WDK6-PCB WDR2-2-PCB.                           
025900 MAIN SECTION.                                                            
026000     ENTRY 'DLITCBL' USING MSG-PCB WDC9-PCB WDK7-PCB                      
026100                           WDR2-PCB WDR5-PCB 4579-PCB                     
026200                           WDK6-PCB WDR2-2-PCB.                           
026300                                                                          
026400     PERFORM A-INIT                                                       
026500                                                                          
026600     PERFORM S01-LAES-W11465                                              
026700     PERFORM UNTIL END-OF-W11465                                          
026800                                                                          
026900        MOVE W11465-IDPITEM TO WS-IDPITEM                                 
027000        INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                
027100        MOVE WS-IDARTNR-NUM TO W-IDARTNR                                  
027200                                                                          
027300        MOVE SPACE  TO WS-W11465-IDUSER                                   
027400        IF W11465-CDTYPE-REQ = 'NP'                                       
027500           MOVE W11465-NP-IDUSER  TO WS-W11465-IDUSER                     
027600        END-IF                                                            
027700                                                                          
027800        IF W11465-CDTYPE-REQ = 'CC'                                       
027900           MOVE W11465-CC-IDUSER  TO WS-W11465-IDUSER                     
028000        END-IF                                                            
028100                                                                          
028200        EVALUATE WS-W11465-IDUSER                                         
028300           WHEN 'CHN07'                                                   
028400             MOVE WC-NDC-CN-71 TO W-IDDC                                  
028500           WHEN 'CHN04'                                                   
028600             MOVE WC-NDC-CN-72 TO W-IDDC                                  
028700           WHEN 'AEFZT'                                                   
028800             MOVE WC-NDC-CN-73 TO W-IDDC                                  
028900           WHEN 'AEQD2'                                                   
029000             MOVE WC-NDC-CN-74 TO W-IDDC                                  
029100           WHEN 'CUGZQ'                                                   
029200*--          DC41                                                         
029300             MOVE WC-NDC-US-RU TO W-IDDC                                  
029400           WHEN 'CUGZR'                                                   
029500*--          DC43                                                         
029600             MOVE WC-NDC-US-LA TO W-IDDC                                  
029700           WHEN 'AEHRD'                                                   
029800*--          DC44                                                         
029900             MOVE WC-NDC-US-SE TO W-IDDC                                  
030000           WHEN 'AEKS1'                                                   
030100*--          DC45                                                         
030200             MOVE WC-NDC-US-CH TO W-IDDC                                  
030300           WHEN 'AELGT'                                                   
030400*--          DC46                                                         
030500             MOVE WC-NDC-US-JA TO W-IDDC                                  
030600           WHEN 'AE9X5'                                                   
030700*--          DC47                                                         
030800             MOVE WC-NDC-US-DA TO W-IDDC                                  
030900        END-EVALUATE                                                      
031000                                                                          
031100        EVALUATE W11465-RETURN-CODE                                       
031200                                                                          
031300        WHEN '  '                                                         
031400             PERFORM B-UPPDATERA-K722                                     
031500             PERFORM C-UPPDATERA-C901                                     
031600                                                                          
031700        WHEN '00'                                                         
031800             PERFORM B-UPPDATERA-K722                                     
031900             PERFORM C-UPPDATERA-C901                                     
032000                                                                          
032100        WHEN '02'                                                         
032200             PERFORM B-UPPDATERA-K722                                     
032300             PERFORM C-UPPDATERA-C901                                     
032400                                                                          
032500        WHEN '03'                                                         
032600             PERFORM B-UPPDATERA-K722                                     
032700             PERFORM C-UPPDATERA-C901                                     
032800                                                                          
032900        WHEN '04'                                                         
033000             PERFORM B-UPPDATERA-K722                                     
033100             PERFORM C-UPPDATERA-C901                                     
033200                                                                          
033300        WHEN '05'                                                         
033400             PERFORM E-SKAPA-LARM-712                                     
033500                                                                          
033600        WHEN '08'                                                         
033700             PERFORM C-UPPDATERA-C901                                     
033800             PERFORM D-SKAPA-LARM-708                                     
033900                                                                          
034000        WHEN '10'                                                         
034100             PERFORM D-SKAPA-LARM-708                                     
034200                                                                          
034300        WHEN '12'                                                         
034400             PERFORM E-SKAPA-LARM-712                                     
034500                                                                          
034600        WHEN '20'                                                         
034700             PERFORM C-UPPDATERA-C901                                     
034800             PERFORM F-SKAPA-LARM-720                                     
034900        END-EVALUATE                                                      
035000                                                                          
035100        IF CHKP-ANT > CHKP-MAX                                            
035200           PERFORM X-TAG-CHECKPOINT                                       
035300        END-IF                                                            
035400        PERFORM S01-LAES-W11465                                           
035500     END-PERFORM                                                          
035600                                                                          
035700                                                                          
035800     PERFORM Z-FINIT                                                      
035900                                                                          
036000     MOVE ZERO TO RETURN-CODE                                             
036100     GOBACK                                                               
036200     .                                                                    
036300                                                                          
036400                                                                          
036500 A-INIT SECTION.                                                          
036600     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
036700                                                                          
036800     OPEN INPUT  W11465                                                   
036900                                                                          
037000     ACCEPT DAGENS-DATUM  FROM DATE                                       
037100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037200                                                                          
037300     PERFORM IMS-RESTART                                                  
037400                                                                          
037500     PERFORM IMS-GHU-RESTART                                              
037600     IF 4580-KVPOST > ZERO                                                
037700        MOVE ZERO TO W-ANTAL-POSTER                                       
037800        PERFORM UNTIL W-ANTAL-POSTER = 4580-KVPOST                        
037900           PERFORM S01-LAES-W11465                                        
038000           ADD 1  TO W-ANTAL-POSTER                                       
038100        END-PERFORM                                                       
038200     END-IF                                                               
038300     .                                                                    
038400                                                                          
038500                                                                          
038600 B-UPPDATERA-K722 SECTION.                                                
038700     MOVE 'B-UPPDATERA-K722' TO CURRENT-SECTION                           
038800                                                                          
038900**   FIXA IDINK / KONTROLLERA                                             
039000                                                                          
039100     IF W11465-RETURN-IDHANDLR > SPACE                                    
039200        MOVE W11465-RETURN-IDHANDLR  TO WS-IDHANDLR                       
039300     ELSE                                                                 
039400        MOVE W11465-IDHANDLR         TO WS-IDHANDLR                       
039500     END-IF                                                               
039600                                                                          
039700     IF WS-IDHANDLR (2:1) > ZERO                                          
039800        MOVE WS-IDHANDLR (2:3)       TO WS-IDINK                          
039900     ELSE                                                                 
040000        IF WS-IDHANDLR (3:1) > ZERO                                       
040100           MOVE WS-IDHANDLR (3:2)    TO WS-IDINK                          
040200        ELSE                                                              
040300           IF WS-IDHANDLR (4:1) > ZERO                                    
040400              MOVE WS-IDHANDLR (4:1) TO WS-IDINK                          
040500           ELSE                                                           
040600              MOVE SPACE             TO WS-IDINK                          
040700           END-IF                                                         
040800        END-IF                                                            
040900     END-IF                                                               
041000                                                                          
041100     PERFORM IMS-GHU-WDK722                                               
041200     IF SEGMENT-FINNS AND XLAG-IDINK NOT = WS-IDINK                       
041300                      AND WS-IDINK > SPACE                                
041400        MOVE WS-IDINK    TO XLAG-IDINK                                    
041500        PERFORM IMS-REPL-WDK722                                           
041600     END-IF                                                               
041700     .                                                                    
041800                                                                          
041900                                                                          
042000 C-UPPDATERA-C901 SECTION.                                                
042100     MOVE 'C-UPPDATERA-C901' TO CURRENT-SECTION                           
042200                                                                          
042300     MOVE W-IDARTNR TO W-IDARTNR-C901                                     
042400     MOVE W-IDDC    TO W-IDDC-C901                                        
042500     PERFORM IMS-GHU-WDC901                                               
042600     IF SEGMENT-FINNS                                                     
042700        MOVE DAGENS-DATUM TO KART-TIMOTSI                                 
042800        IF W11465-RETURN-CODE = '03' OR '08' OR '20'                      
042900           MOVE '1'       TO KART-KDANSKQ                                 
043000        END-IF                                                            
043100        PERFORM IMS-REPL-WDC901                                           
043200                                                                          
043300        PERFORM CA-ISRT-WDGX2264-2266                                     
043400     END-IF                                                               
043500     .                                                                    
043600                                                                          
043700                                                                          
043800 CA-ISRT-WDGX2264-2266 SECTION.                                           
043900     MOVE 'CA-ISRT-WDGX2264-2266' TO CURRENT-SECTION                      
044000                                                                          
044100     PERFORM IMS-GU-WDK601                                                
044200     IF SEGMENT-FINNS                                                     
044300        MOVE ART-TISOP          TO 2264-TISOP                             
044400                                   W-TISOP-2264                           
044500        PERFORM IMS-ISRT-WDGX2264                                         
044600                                                                          
044700        MOVE KART-IDARTNR       TO 2266-IDARTNR                           
044800        MOVE KART-IDDC          TO 2266-IDDC                              
044900        MOVE KART-TIMOTSI       TO 2266-TIMOTSI                           
045000        PERFORM IMS-ISRT-WDGX2266                                         
045100     END-IF                                                               
045200     .                                                                    
045300                                                                          
045400 D-SKAPA-LARM-708 SECTION.                                                
045500     MOVE 'D-SKAPA-LARM-708' TO CURRENT-SECTION                           
045600                                                                          
045700     PERFORM IMS-GU-WDK722                                                
045800     IF SEGMENT-SAKNAS                                                    
045900       MOVE ZERO                TO XLAG-IDANSK                            
046000     END-IF                                                               
046100                                                                          
046200     MOVE 708                   TO 2224-KDLARM                            
046300     MOVE W11465-RETURN-ARRIVED TO 2224-TIREGDAT                          
046400     PERFORM S010-CREATE-GENERELLT-LARM                                   
046500                                                                          
046600     MOVE 'WDR5  '      TO POSTSUM-FDNAMN                                 
046700     MOVE 'ALERT   '    TO POSTSUM-DDNAMN2                                
046800     MOVE '708'         TO POSTSUM-TRANSTYP                               
046900     CALL POSTSUM    USING POSTSUM-PARM                                   
047000     .                                                                    
047100                                                                          
047200                                                                          
047300 E-SKAPA-LARM-712 SECTION.                                                
047400     MOVE 'E-SKAPA-LARM-712' TO CURRENT-SECTION                           
047500                                                                          
047600     PERFORM IMS-GU-WDK722                                                
047700     IF SEGMENT-SAKNAS                                                    
047800       MOVE ZERO                TO XLAG-IDANSK                            
047900     END-IF                                                               
048000                                                                          
048100     MOVE 712                   TO 2224-KDLARM                            
048200     MOVE W11465-RETURN-ARRIVED TO 2224-TIREGDAT                          
048300     PERFORM S010-CREATE-GENERELLT-LARM                                   
048400                                                                          
048500     MOVE 'WDR5  '      TO POSTSUM-FDNAMN                                 
048600     MOVE 'ALERT   '    TO POSTSUM-DDNAMN2                                
048700     MOVE '712'         TO POSTSUM-TRANSTYP                               
048800     CALL POSTSUM    USING POSTSUM-PARM                                   
048900     .                                                                    
049000                                                                          
049100                                                                          
049200 F-SKAPA-LARM-720 SECTION.                                                
049300     MOVE 'F-SKAPA-LARM-720' TO CURRENT-SECTION                           
049400                                                                          
049500     PERFORM IMS-GU-WDK722                                                
049600     IF SEGMENT-SAKNAS                                                    
049700       MOVE ZERO                TO XLAG-IDANSK                            
049800     END-IF                                                               
049900                                                                          
050000     MOVE 720                   TO 2224-KDLARM                            
050100     MOVE W11465-RETURN-DELETE-DATE TO 2224-TIREGDAT                      
050200     PERFORM S010-CREATE-GENERELLT-LARM                                   
050300                                                                          
050400     MOVE 'WDR5  '      TO POSTSUM-FDNAMN                                 
050500     MOVE 'ALERT   '    TO POSTSUM-DDNAMN2                                
050600     MOVE '720'         TO POSTSUM-TRANSTYP                               
050700     CALL POSTSUM    USING POSTSUM-PARM                                   
050800     .                                                                    
050900                                                                          
051000                                                                          
051100 Z-FINIT SECTION.                                                         
051200     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
051300                                                                          
051400     CLOSE W11465                                                         
051500                                                                          
051600     MOVE 'S' TO POSTSUM-OPKOD                                            
051700     CALL POSTSUM USING POSTSUM-PARM                                      
051800                                                                          
051900     PERFORM IMS-GHU-RESTART                                              
052000     MOVE ZERO       TO 4580-KVPOST                                       
052100     ACCEPT 4580-TIUPPDAT FROM DATE                                       
052200     ACCEPT 4580-TIUPPTID FROM TIME                                       
052300     PERFORM IMS-REPL-RESTART                                             
052400     .                                                                    
052500                                                                          
052600 S01-LAES-W11465  SECTION.                                                
052700     MOVE 'S01-LAES-W11465 ' TO CURRENT-SECTION                           
052800                                                                          
052900     READ W11465 INTO W11465-AREA                                         
053000     AT END                                                               
053100        MOVE HIGH-VALUE TO W11465-AREA                                    
053200        SET END-OF-W11465 TO TRUE                                         
053300                                                                          
053400     NOT AT END                                                           
053500        MOVE 'W11465'    TO POSTSUM-FDNAMN                                
053600        MOVE 'W11467D1'  TO POSTSUM-DDNAMN2                               
053700        MOVE W11465-IDRT TO POSTSUM-TRANSTYP                              
053800        CALL POSTSUM  USING POSTSUM-PARM                                  
053900                                                                          
054000        ADD 1 TO W-ANTAL-POSTER                                           
054100     END-READ                                                             
054200     .                                                                    
054300                                                                          
054400                                                                          
054500 S010-CREATE-GENERELLT-LARM SECTION.                                      
054600     MOVE 'S010-CREATE-GENERELLT-LARM' TO CURRENT-SECTION                 
054700                                                                          
054800     MOVE SPACE                   TO 2223-WDGX2223                        
054900     MOVE '2223'                  TO 2223-IDHTYP                          
055000     MOVE XLAG-IDANSK             TO W-IDANSK-2232                        
055100     PERFORM IMS-GU-WDR220                                                
055200     IF SEGMENT-FINNS                                                     
055300       MOVE 2232-IDANSK-LARM      TO W-IDANSK-2223                        
055400     ELSE                                                                 
055500       MOVE ZERO                  TO W-IDANSK-2223                        
055600     END-IF                                                               
055700     MOVE W-IDANSK-2223           TO 2223-IDANSK                          
055800     MOVE LOW-VALUE               TO 2223-LOW-VALUE                       
055900                                                                          
056000     PERFORM IMS-GU-WDGX2223                                              
056100     IF SEGMENT-SAKNAS                                                    
056200        PERFORM IMS-ISRT-WDGX2223                                         
056300        PERFORM IMS-GU-WDGX2223                                           
056400     END-IF                                                               
056500                                                                          
056600     MOVE 2224-KDLARM             TO W-KDLARM                             
056700     PERFORM IMS-GHNP-WDGX2224                                            
056800     IF SEGMENT-SAKNAS                                                    
056900        PERFORM S010A-REDIGERA-GENERELLT-LARM                             
057000        PERFORM IMS-ISRT-WDGX2224                                         
057100        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
057200          MOVE 2224-TISENBEK-KL   TO WS-NXT-KL                            
057300          PERFORM S010B-NXT-SEKUND                                        
057400          MOVE WS-NXT-KL          TO 2224-TISENBEK-KL                     
057500          PERFORM IMS-ISRT-WDGX2224                                       
057600        END-PERFORM                                                       
057700     ELSE                                                                 
057800        MOVE DAGENS-DATUM         TO 2224-TIREGDAT                        
057900        PERFORM IMS-REPL-WDGX2224                                         
058000     END-IF                                                               
058100     .                                                                    
058200                                                                          
058300 S010A-REDIGERA-GENERELLT-LARM SECTION.                                   
058400     MOVE 'S010-GEN-LARM   '  TO CURRENT-SECTION                          
058500                                                                          
058600     MOVE FUNCTION CURRENT-DATE(3:6)                                      
058700                              TO 2224-TISENBEK-DAG                        
058800     MOVE FUNCTION CURRENT-DATE(11:6)                                     
058900                              TO 2224-TISENBEK-KL                         
059000     MOVE W-IDARTNR           TO 2224-IDARTNR                             
059100     MOVE W-IDDC              TO 2224-IDDC                                
059200     MOVE JA                  TO 2224-FLNYLARM                            
059300     MOVE ZERO                TO 2224-IDDISTR                             
059400                                 2224-IDKUNDNR                            
059500     MOVE '0000000   '        TO 2224-IDKUNDRF                            
059600     MOVE 1                   TO 2224-IDLOPNR                             
059700                                                                          
059800     IF 2224-TIREGDAT NOT > ZERO                                          
059900        MOVE DAGENS-DATUM     TO 2224-TIREGDAT                            
060000     END-IF                                                               
060100     MOVE SPACE               TO 2224-IDTRANS                             
060200                                 2224-KDMFSFOR                            
060300     MOVE ZERO                TO 2224-IDKR                                
060400                                                                          
060500     PERFORM IMS-GU-WDK711                                                
060600     IF SEGMENT-FINNS                                                     
060700       MOVE SLAG-IDLEVNR      TO 2224-IDLEVNR                             
060800     ELSE                                                                 
060900       MOVE SPACE             TO 2224-IDLEVNR                             
061000     END-IF                                                               
061100     .                                                                    
061200                                                                          
061300 S010B-NXT-SEKUND              SECTION.                                   
061400     MOVE 'S010A-NXT-SEKUND'  TO CURRENT-SECTION                          
061500*                                                                         
061600*    RÄKNAR UPP TILL NÄSTA SEKUND.                                        
061700*    GÅR ALDRIG ÖVER DYGNS-GRÄNS.                                         
061800*    NÄSTA SEKUND EFTER 23.59.59 GER 00.00.00 INOM SAMMA DYGN.            
061900*                                                                         
062000     ADD 1                   TO WS-NXT-KL-SS                              
062100     IF  WS-NXT-KL-SS > 59                                                
062200       MOVE ZERO             TO WS-NXT-KL-SS                              
062300       ADD 1                 TO WS-NXT-KL-MM                              
062400       IF  WS-NXT-KL-MM > 59                                              
062500         MOVE ZERO           TO WS-NXT-KL-MM                              
062600         ADD 1               TO WS-NXT-KL-TT                              
062700         IF  WS-NXT-KL-TT > 23                                            
062800           MOVE ZERO         TO WS-NXT-KL-TT                              
062900         END-IF                                                           
063000       END-IF                                                             
063100     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400                                                                          
063500                                                                          
063600 X-TAG-CHECKPOINT   SECTION.                                              
063700                                                                          
063800     PERFORM IMS-GHU-RESTART                                              
063900     MOVE W-ANTAL-POSTER TO 4580-KVPOST                                   
064000     ACCEPT 4580-TIUPPDAT FROM DATE                                       
064100     ACCEPT 4580-TIUPPTID FROM TIME                                       
064200     PERFORM IMS-REPL-RESTART                                             
064300                                                                          
064400     PERFORM IMS-CHECKPOINT                                               
064500     MOVE ZERO TO CHKP-ANT                                                
064600     .                                                                    
064700                                                                          
064800                                                                          
064900* --- IMS SEKTIONER ---                                                   
065000                                                                          
065100                                                                          
065200 IMS-RESTART SECTION.                                                     
065300     MOVE 'IMS-RESTART     ' TO CURRENT-IMS-SECTION                       
065400                                                                          
065500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
065600     MOVE '  ' TO GODK-STATUSKODER                                        
065700     CALL CBLTDLI USING XRST MSG-PCB                                      
065800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
065900                        CHKP-AREA-LENGTH CHKP-AREA                        
066000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
066100     PERFORM IMS-STATUSKONTROLL                                           
066200     .                                                                    
066300                                                                          
066400                                                                          
066500 IMS-CHECKPOINT SECTION.                                                  
066600     MOVE 'IMS-CHECKPOINT  ' TO CURRENT-IMS-SECTION                       
066700                                                                          
066800     MOVE SPACE  TO CHKP-MSG-IO-AREA                                      
066900     MOVE '  XD' TO GODK-STATUSKODER                                      
067000     CALL CBLTDLI USING CHKP MSG-PCB                                      
067100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
067200                        CHKP-AREA-LENGTH CHKP-AREA                        
067300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067400     PERFORM IMS-STATUSKONTROLL                                           
067500                                                                          
067600     IF IMS-EJ-OK                                                         
067700       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
067800       DISPLAY FELTEXT                                                    
067900       CALL FELLOG                                                        
068000     END-IF                                                               
068100     .                                                                    
068200                                                                          
068300                                                                          
068400                                                                          
068500 IMS-GHU-WDC901 SECTION.                                                  
068600     MOVE 'IMS-GHU-WDC901  '  TO CURRENT-IMS-SECTION                      
068700                                                                          
068800     MOVE SPACE               TO ALL-SSA                                  
068900     STRING 'WDC901  (WDC901KY =' W-WDC901KY-X ')'                        
069000          DELIMITED BY SIZE INTO SSA1                                     
069100     MOVE '  GE' TO GODK-STATUSKODER                                      
069200     CALL CBLTDLI USING GHU WDC9-PCB DLI-IO-WDC901 SSA1                   
069300     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
069400     PERFORM IMS-STATUSKONTROLL                                           
069500     .                                                                    
069600                                                                          
069700                                                                          
069800 IMS-REPL-WDC901 SECTION.                                                 
069900     MOVE 'IMS-REPL-WDC901 '  TO CURRENT-IMS-SECTION                      
070000                                                                          
070100     MOVE SPACE               TO ALL-SSA                                  
070200     MOVE '  '             TO GODK-STATUSKODER                            
070300     CALL CBLTDLI USING REPL WDC9-PCB DLI-IO-WDC901                       
070400     MOVE WDC9-STATUS-CODE TO STATUS-WS                                   
070500     PERFORM IMS-STATUSKONTROLL                                           
070600     ADD +2 TO CHKP-ANT                                                   
070700     .                                                                    
070800                                                                          
070900                                                                          
071000 IMS-GHU-WDK722 SECTION.                                                  
071100     MOVE 'IMS-GHU-WDK722  '  TO CURRENT-IMS-SECTION                      
071200                                                                          
071300     MOVE SPACE               TO ALL-SSA                                  
071400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
071500          DELIMITED BY SIZE INTO SSA1                                     
071600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
071700          DELIMITED BY SIZE INTO SSA2                                     
071800     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
071900          DELIMITED BY SIZE INTO SSA3                                     
072000     MOVE '  GE'              TO GODK-STATUSKODER                         
072100     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
072200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
072300     PERFORM IMS-STATUSKONTROLL                                           
072400     .                                                                    
072500                                                                          
072600                                                                          
072700 IMS-REPL-WDK722 SECTION.                                                 
072800     MOVE 'IMS-REPL-WDK722 '  TO CURRENT-IMS-SECTION                      
072900                                                                          
073000     MOVE SPACE               TO ALL-SSA                                  
073100     MOVE '  '             TO GODK-STATUSKODER                            
073200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
073300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
073400     PERFORM IMS-STATUSKONTROLL                                           
073500     ADD +1 TO CHKP-ANT                                                   
073600     .                                                                    
073700                                                                          
073800                                                                          
073900 IMS-GU-WDK722 SECTION.                                                   
074000     MOVE 'IMS-GU-WDK722  '  TO CURRENT-IMS-SECTION                       
074100                                                                          
074200     MOVE SPACE               TO ALL-SSA                                  
074300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
074400          DELIMITED BY SIZE INTO SSA1                                     
074500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
074600          DELIMITED BY SIZE INTO SSA2                                     
074700     STRING 'WDK722  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
074800          DELIMITED BY SIZE INTO SSA3                                     
074900     MOVE '  GE'              TO GODK-STATUSKODER                         
075000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
075100     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
075200     PERFORM IMS-STATUSKONTROLL                                           
075300     .                                                                    
075400                                                                          
075500                                                                          
075600 IMS-GU-WDK711 SECTION.                                                   
075700     MOVE 'IMS-GU-WDK711 '  TO CURRENT-IMS-SECTION                        
075800                                                                          
075900     MOVE SPACE               TO ALL-SSA                                  
076000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
076100          DELIMITED BY SIZE INTO SSA1                                     
076200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
076300          DELIMITED BY SIZE INTO SSA2                                     
076400     MOVE '  GE'              TO GODK-STATUSKODER                         
076500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711  SSA1 SSA2              
076600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
076700     PERFORM IMS-STATUSKONTROLL                                           
076800                                                                          
076900     .                                                                    
077000                                                                          
077100 IMS-GU-WDR220 SECTION.                                                   
077200     MOVE 'IMS-GU-WDR220   '  TO CURRENT-IMS-SECTION                      
077300                                                                          
077400     MOVE SPACE               TO ALL-SSA                                  
077500     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
077600          DELIMITED BY SIZE INTO SSA1                                     
077700     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
077800          DELIMITED BY SIZE INTO SSA2                                     
077900     MOVE '  GE'              TO GODK-STATUSKODER                         
078000     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDR220 SSA1 SSA2               
078100     MOVE WDR2-STATUS-CODE    TO STATUS-WS                                
078200     PERFORM IMS-STATUSKONTROLL                                           
078300     .                                                                    
078400                                                                          
078500 IMS-GU-WDGX2223 SECTION.                                                 
078600     MOVE 'IMS-GU-WDGX2223   '  TO CURRENT-IMS-SECTION                    
078700                                                                          
078800     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
078900          DELIMITED BY SIZE INTO SSA1                                     
079000     MOVE '  GE'              TO GODK-STATUSKODER                         
079100     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX2223 SSA1                  
079200     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
079300     PERFORM IMS-STATUSKONTROLL                                           
079400     .                                                                    
079500                                                                          
079600 IMS-GHNP-WDGX2224 SECTION.                                               
079700     MOVE 'IMS-GHNP-WDGX2224  '  TO CURRENT-IMS-SECTION                   
079800                                                                          
079900     STRING 'WDR550  (IDARTNR  =' W-IDARTNR-X                             
080000                    '&IDDC     =' W-IDDC-X                                
080100                    '&KDLARM   =' W-KDLARM-X ')'                          
080200          DELIMITED BY SIZE INTO SSA1                                     
080300     MOVE '  GE'           TO GODK-STATUSKODER                            
080400     CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-WDGX2224 SSA1                
080500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
080600     PERFORM IMS-STATUSKONTROLL                                           
080700     .                                                                    
080800                                                                          
080900                                                                          
081000 IMS-ISRT-WDGX2223 SECTION.                                               
081100     MOVE 'IMS-ISRT-WDGX2223' TO CURRENT-IMS-SECTION                      
081200                                                                          
081300     MOVE SPACE               TO ALL-SSA                                  
081400     MOVE 'WDR501 '        TO SSA1                                        
081500     MOVE '  II'           TO GODK-STATUSKODER                            
081600     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2223 SSA1                
081700     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
081800     PERFORM IMS-STATUSKONTROLL                                           
081900     ADD +1 TO CHKP-ANT                                                   
082000     .                                                                    
082100                                                                          
082200                                                                          
082300 IMS-ISRT-WDGX2224 SECTION.                                               
082400     MOVE 'IMS-ISRT-WDGX2224' TO CURRENT-IMS-SECTION                      
082500                                                                          
082600     MOVE SPACE               TO ALL-SSA                                  
082700     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
082800          DELIMITED BY SIZE INTO SSA1                                     
082900     MOVE 'WDR550 '           TO SSA2                                     
083000     MOVE '  II'              TO GODK-STATUSKODER                         
083100     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDGX2224 SSA1 SSA2           
083200     MOVE WDR5-STATUS-CODE    TO STATUS-WS                                
083300     PERFORM IMS-STATUSKONTROLL                                           
083400     ADD +1 TO CHKP-ANT                                                   
083500     .                                                                    
083600                                                                          
083700 IMS-GU-WDK601 SECTION.                                                   
083800     MOVE 'IMS-GU-WDK601      '  TO CURRENT-IMS-SECTION                   
083900                                                                          
084000     MOVE SPACE               TO ALL-SSA                                  
084100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
084200          DELIMITED BY SIZE INTO SSA1                                     
084300     MOVE '  GE'           TO GODK-STATUSKODER                            
084400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
084500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
084600     PERFORM IMS-STATUSKONTROLL                                           
084700     .                                                                    
084800 IMS-ISRT-WDGX2264 SECTION.                                               
084900     MOVE 'IMS-ISRT-WDGX2264  '  TO CURRENT-IMS-SECTION                   
085000                                                                          
085100     MOVE SPACE               TO ALL-SSA                                  
085200     STRING 'WDR201  (WDGXKEY  =' W-WDGX2263-X ')'                        
085300          DELIMITED BY SIZE INTO SSA1                                     
085400     MOVE   'WDGX2264'        TO SSA2                                     
085500     MOVE '  II'              TO GODK-STATUSKODER                         
085600     CALL CBLTDLI USING ISRT WDR2-2-PCB DLI-IO-WDGX2264 SSA1 SSA2         
085700     MOVE WDR2-2-STATUS-CODE    TO STATUS-WS                              
085800     PERFORM IMS-STATUSKONTROLL                                           
085900     .                                                                    
086000                                                                          
086100 IMS-ISRT-WDGX2266 SECTION.                                               
086200     MOVE 'IMS-ISRT-WDGX2266  '  TO CURRENT-IMS-SECTION                   
086300                                                                          
086400     MOVE SPACE               TO ALL-SSA                                  
086500     STRING 'WDR201  (WDGXKEY  =' W-WDGX2263-X ')'                        
086600          DELIMITED BY SIZE INTO SSA1                                     
086700     STRING 'WDGX2264(TISOP    =' W-TISOP-X ')'                           
086800          DELIMITED BY SIZE INTO SSA2                                     
086900     MOVE   'WDGX2266'        TO SSA3                                     
087000     MOVE '  II'              TO GODK-STATUSKODER                         
087100     CALL CBLTDLI USING ISRT WDR2-2-PCB DLI-IO-WDGX2266 SSA1              
087200                                                        SSA2              
087300                                                        SSA3              
087400     MOVE WDR2-2-STATUS-CODE    TO STATUS-WS                              
087500     PERFORM IMS-STATUSKONTROLL                                           
087600     .                                                                    
087700                                                                          
087800 IMS-REPL-WDGX2224 SECTION.                                               
087900     MOVE 'IMS-REPL-WDGX2224  '  TO CURRENT-IMS-SECTION                   
088000                                                                          
088100     MOVE '  '             TO GODK-STATUSKODER                            
088200     CALL CBLTDLI USING REPL WDR5-PCB DLI-IO-WDGX2224                     
088300     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
088400     PERFORM IMS-STATUSKONTROLL                                           
088500     .                                                                    
088600                                                                          
088700 IMS-GHU-RESTART  SECTION.                                                
088800     MOVE 'IMS-GHU-RESTART '  TO CURRENT-IMS-SECTION                      
088900                                                                          
089000     MOVE SPACE          TO ALL-SSA                                       
089100     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
089200          DELIMITED BY SIZE INTO SSA1                                     
089300     MOVE 'WDR470   '    TO SSA2                                          
089400     MOVE '    '         TO GODK-STATUSKODER                              
089500     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
089600     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
089700     PERFORM IMS-STATUSKONTROLL                                           
089800     .                                                                    
089900                                                                          
090000                                                                          
090100 IMS-REPL-RESTART SECTION.                                                
090200     MOVE 'IMS-REPL-RESTART'  TO CURRENT-IMS-SECTION                      
090300                                                                          
090400     MOVE '  '             TO GODK-STATUSKODER                            
090500     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
090600     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
090700     PERFORM IMS-STATUSKONTROLL                                           
090800     .                                                                    
090900                                                                          
091000                                                                          
091100 IMS-STATUSKONTROLL SECTION.                                              
091200                                                                          
091300     SET STATUS-IX TO 1                                                   
091400     SEARCH GODK-STATUS                                                   
091500       AT END                                                             
091600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
091700           DELIMITED BY SIZE INTO FELTEXT                                 
091800         DISPLAY FELTEXT                                                  
091900         CALL FELLOG                                                      
092000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
092100         CONTINUE                                                         
092200     END-SEARCH                                                           
092300     .                                                                    

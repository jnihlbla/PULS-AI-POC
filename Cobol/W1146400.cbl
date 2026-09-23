000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1146400.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   05/12/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKAPAR LARM                                                      
001000*        PGA KVITTERINGAR FRÅN SI+                                        
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK6                                       
001300*        PROGRAMMET LÄSER      WDR2                                       
001400*        PROGRAMMET UPPDATERAR WDR5                                       
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- RETURKOD = 08 10 12 20 FRÅN SI+                            
002500     SELECT W11461                     ASSIGN TO W11464D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W11461                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01  -COPY T335R309      -L.                                              
003600     SKIP3                                                                
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W1146400'.            
004100 01  CHKP-VAR.                                                            
004200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004700     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000     SKIP2                                                                
005100 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005400                                                                          
005500 77  W11461-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W11461                       VALUE 'J'.                   
005700     EJECT                                                                
005800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES DAGENS-DATUM.                                       
006000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006300     SKIP3                                                                
006400 01  ARBAREOR.                                                            
006500     03  WS-LARM-RETUR-X         PIC 9(3)  VALUE 700.                     
006600     03  FILLER REDEFINES WS-LARM-RETUR-X.                                
006700         05  WS-LARM1            PIC 9.                                   
006800         05  WS-RETUR            PIC X(2).                                
006900     03  WS-LARM-RETUR REDEFINES WS-LARM-RETUR-X  PIC 9(3).               
007000                                                                          
007100     03  WS-IDPITEM              PIC X(20).                               
007200     03  FILLER  REDEFINES WS-IDPITEM.                                    
007300         05  FILLER              PIC X(11).                               
007400         05  WS-IDARTNR          PIC X(9).                                
007500     03  FILLER  REDEFINES WS-IDPITEM.                                    
007600         05  FILLER              PIC X(11).                               
007700         05  WS-IDARTNR-NUM      PIC 9(9).                                
007800*                                                                         
007900*01  -COPY WWDCKONS                                                       
008000                                                                          
008100     EJECT                                                                
008200 01  DYNAMISKA-SUBPROGRAM.                                                
008300*                                                                         
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL POSTSUM                                          
009000*                                                                         
009100*01  -COPY W0005   -PRE  POSTSUM-                                         
009200     EJECT                                                                
009300*01  -COPY WDATAREA                                                       
009400     EJECT                                                                
009500 01  IN-AREA-START               PIC X(24)   VALUE                        
009600                                             'IN-AREA-START'.             
009700     SKIP2                                                                
009800                                                                          
009900*01  AREA -COPY T335R309     -PRE IN-                                     
010000*                                                                         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010300     SKIP3                                                                
010400 01  NYCKLAR-TILL-DLI.                                                    
010500     03  W-IDARTNR-X.                                                     
010600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010700     03  W-KDSEGKEY-X.                                                    
010800         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
010900                                                                          
011000     03  W-WDGX2223-X.                                                    
011100         05  W-IDHTYP-2223       PIC X(4)     VALUE '2223'.               
011200         05  W-IDANSK-2223       PIC S9(3)    VALUE ZERO COMP-3.          
011300         05  W-VALFRI-2225       PIC X(24)    VALUE LOW-VALUE.            
011400                                                                          
011500     03  W-WDGX2224-X.                                                    
011600         05  W-TISENBEK-DAG-2224 PIC S9(7)    VALUE ZERO COMP-3.          
011700         05  W-TISENBEK-KL-2224  PIC S9(7)    VALUE ZERO COMP-3.          
011800         05  W-KDLARM-2224       PIC S9(3)    VALUE ZERO COMP-3.          
011900                                                                          
012000     03  W-WDGX2231-X.                                                    
012100         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
012200         05  W-VALFRI-2231       PIC X(26)    VALUE LOW-VALUE.            
012300                                                                          
012400     03  W-WDGX2232-X.                                                    
012500         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
012600         05  W-LOW-VALUE-2232    PIC X(3)     VALUE LOW-VALUE.            
012700                                                                          
012800     SKIP2                                                                
012900*    --- STATUS-KOD FRÅN IMS                                              
013000 01  STATUS-WS                   PIC XX.                                  
013100     88  SEGMENT-FINNS                       VALUE '  '.                  
013200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013500     88  IMS-EJ-OK                           VALUE 'XD'.                  
013600     SKIP2                                                                
013700 01  GODK-STATUSKODER.                                                    
013800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900     SKIP3                                                                
014000 01  SSA1                        PIC X(64).                               
014100 01  SSA2                        PIC X(64).                               
014200     EJECT                                                                
014300*    --- IMS FUNKTIONSKODER                                               
014400*01  -COPY W0003                                                          
014500     EJECT                                                                
014600*    ---  DLI INPUT-OUTPUT AREA                                           
014700                                                                          
014800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
014900 01  DLI-IO-WDK601.                                                       
015000*    03  -COPY WDK601                                                     
015100     SKIP3                                                                
015200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
015300 01  DLI-IO-WDK611.                                                       
015400*    03  -COPY WDK611                                                     
015500     EJECT                                                                
015600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
015700 01  DLI-IO-WDGX01.                                                       
015800*    03  -COPY WDGX01                                                     
015900     SKIP3                                                                
016000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR220'.                      
016100 01  DLI-IO-WDR220.                                                       
016200*    03  -COPY WDGX2232   -PRE WDR220-                                    
016300     EJECT                                                                
016400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR501'.                      
016500 01  DLI-IO-WDR501.                                                       
016600*    03  -COPY WDGX2223   -PRE WDR501-                                    
016700     SKIP3                                                                
016800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR550'.                      
016900 01  DLI-IO-WDR550.                                                       
017000*    03  -COPY WDGX2224   -PRE WDR550-                                    
017200                                                                          
017300     EJECT                                                                
017400 LINKAGE SECTION.                                                         
017500                                                                          
017600*01  -COPY W0009   -PRE MSG-                                              
017700                                                                          
017800*01  -COPY W0008  -PRE WDK6-                                              
017900     05  FILLER                  PIC X.                                   
018000                                                                          
018100*01  -COPY W0008  -PRE WDR2-                                              
018200     05  FILLER                  PIC X.                                   
018300                                                                          
018400*01  -COPY W0008  -PRE WDR5-                                              
018500     05  FILLER                  PIC X.                                   
018600     EJECT                                                                
018700 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDR2-PCB                      
018800     WDR5-PCB.                                                            
018900 MAIN SECTION.                                                            
019000     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDR2-PCB                      
019100     WDR5-PCB.                                                            
019200                                                                          
019300*---- FLYTTA SUBPROGRAM CALL TILL RÄTT STÄLLE --                          
019400*    MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
019500*    MOVE AAMMDD   TO DAT-I-TIDATUM                                       
019600                                                                          
019700*    CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
019800*                    DAT-O-TIDATUM DAT-KDSVAR                             
019900                                                                          
020000*    IF DAT-KDSVAR-OK                                                     
020100*      ......                                                             
020200*    ELSE                                                                 
020300*      .........                                                          
020400*    END-IF                                                               
020500*------------------------                                                 
020600     SKIP2                                                                
020700     PERFORM A-INIT                                                       
020800     PERFORM S01-LAES-W11461                                              
020900     PERFORM UNTIL END-OF-W11461                                          
021000       IF CHKP-ANT > CHKP-MAX                                             
021100         PERFORM X-TAG-CHECKPOINT                                         
021200       END-IF                                                             
021300*      FIXA IDARTNR                                                       
021400       MOVE IN-IDPITEM TO WS-IDPITEM                                      
021500       INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                 
021600       MOVE WS-IDARTNR-NUM TO W-IDARTNR                                   
021700                                                                          
021800       PERFORM IMS-GET-WDK611                                             
021900       IF SEGMENT-FINNS                                                   
022000          PERFORM B-SKAPA-LARM                                            
022100       ELSE                                                               
022200          DISPLAY ' ARTNR FINNS EJ I PULS ' WS-IDARTNR                    
022300       END-IF                                                             
022400                                                                          
022500       PERFORM S01-LAES-W11461                                            
022600     END-PERFORM                                                          
022700                                                                          
022800                                                                          
022900     PERFORM Z-FINIT                                                      
023000                                                                          
023100     MOVE ZERO TO RETURN-CODE                                             
023200     GOBACK                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 A-INIT SECTION.                                                          
023600     SKIP2                                                                
023700                                                                          
023800     PERFORM IMS-RESTART                                                  
023900                                                                          
024000     ACCEPT DAGENS-DATUM  FROM DATE                                       
024100                                                                          
024200     OPEN INPUT W11461                                                    
024300                                                                          
024400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024500     .                                                                    
024600     EJECT                                                                
024700 B-SKAPA-LARM SECTION.                                                    
024800     SKIP2                                                                
024900     MOVE CLAG-IDANSK TO W-IDANSK-2232                                    
025000     PERFORM IMS-GET-WDR220                                               
025100     IF SEGMENT-FINNS                                                     
025200       MOVE WDR220-2232-IDANSK-LARM TO W-IDANSK-2223                      
025300     ELSE                                                                 
025400       MOVE ZERO TO W-IDANSK-2223                                         
025500     END-IF                                                               
025600     MOVE '2223'              TO WDR501-2223-IDHTYP                       
025700     MOVE W-IDANSK-2223       TO WDR501-2223-IDANSK                       
025800     MOVE LOW-VALUE           TO WDR501-2223-LOW-VALUE                    
025900     PERFORM IMS-ISRT-WDR501                                              
026000     PERFORM IMS-GET-WDR501                                               
026100     MOVE FUNCTION CURRENT-DATE(3:6)                                      
026200                              TO WDR550-2224-TISENBEK-DAG                 
026300     MOVE FUNCTION CURRENT-DATE(11:6)                                     
026400                              TO WDR550-2224-TISENBEK-KL                  
026500     MOVE W-IDARTNR           TO WDR550-2224-IDARTNR                      
026510     MOVE WC-CDC-SE           TO WDR550-2224-IDDC                         
026600     MOVE JA                  TO WDR550-2224-FLNYLARM                     
026700     MOVE ZERO                TO WDR550-2224-IDDISTR                      
026800                                 WDR550-2224-IDKUNDNR                     
026900     MOVE '0000000   '        TO WDR550-2224-IDKUNDRF                     
027000     MOVE 1                   TO WDR550-2224-IDLOPNR                      
027100     IF IN-RETURN-CODE = '20'                                             
027200       MOVE IN-RETURN-DELETE-DATE TO WDR550-2224-TIREGDAT                 
027300     ELSE                                                                 
027400       MOVE IN-RETURN-ARRIVED TO WDR550-2224-TIREGDAT                     
027500     END-IF                                                               
027600     IF WDR550-2224-TIREGDAT NOT > ZERO                                   
027700        MOVE DAGENS-DATUM     TO WDR550-2224-TIREGDAT                     
027800     END-IF                                                               
028000     MOVE SPACE               TO WDR550-2224-IDTRANS                      
028100                                 WDR550-2224-KDMFSFOR                     
028200     MOVE ZERO                TO WDR550-2224-IDKR                         
028300     IF IN-RETURN-CODE = '05'                                             
028301        MOVE '12'             TO WS-RETUR                                 
028310     ELSE                                                                 
028311        MOVE IN-RETURN-CODE   TO WS-RETUR                                 
028330     END-IF                                                               
028400     MOVE WS-LARM-RETUR       TO WDR550-2224-KDLARM                       
028410     MOVE SPACE               TO WDR550-2224-IDLEVNR                      
028500                                                                          
028600     PERFORM IMS-ISRT-WDR550                                              
028700                                                                          
028800     .                                                                    
028900     EJECT                                                                
029000 Z-FINIT SECTION.                                                         
029100                                                                          
029200                                                                          
029300     CLOSE W11461                                                         
029400                                                                          
029500                                                                          
029600     MOVE 'S' TO POSTSUM-OPKOD                                            
029700     CALL POSTSUM USING POSTSUM-PARM                                      
029800     .                                                                    
029900     EJECT                                                                
030000 S01-LAES-W11461  SECTION.                                                
030100     SKIP2                                                                
030200     READ W11461 INTO IN-AREA                                             
030300     AT END                                                               
030400        SET END-OF-W11461 TO TRUE                                         
030500                                                                          
030600     NOT AT END                                                           
030700        MOVE 'W11461'        TO POSTSUM-FDNAMN                            
030800        MOVE 'W11464D1'      TO POSTSUM-DDNAMN2                           
030900        MOVE IN-RETURN-CODE  TO POSTSUM-TRANSTYP                          
031000        CALL POSTSUM      USING POSTSUM-PARM                              
031100                                                                          
031200     END-READ                                                             
031300     .                                                                    
031400     EJECT                                                                
031500 X-TAG-CHECKPOINT   SECTION.                                              
031600                                                                          
031700     PERFORM IMS-CHECKPOINT                                               
031800     MOVE ZERO TO CHKP-ANT                                                
031900     .                                                                    
032000     EJECT                                                                
032100* --- IMS SEKTIONER ---                                                   
032200                                                                          
032300 IMS-GET-WDK611 SECTION.                                                  
032400                                                                          
032500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
032600          DELIMITED BY SIZE INTO SSA1                                     
032700     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
032800          DELIMITED BY SIZE INTO SSA2                                     
032900     MOVE '  GE' TO GODK-STATUSKODER                                      
033000     CALL CBLTDLI USING GU  WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
033100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
033200     PERFORM IMS-STATUSKONTROLL                                           
033300     .                                                                    
033400     EJECT                                                                
033500 IMS-GET-WDR220 SECTION.                                                  
033600                                                                          
033700     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
033800          DELIMITED BY SIZE INTO SSA1                                     
033900     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
034000          DELIMITED BY SIZE INTO SSA2                                     
034100     MOVE '  GE' TO GODK-STATUSKODER                                      
034200     CALL CBLTDLI USING GU  WDR2-PCB DLI-IO-WDR220 SSA1 SSA2              
034300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
034400     PERFORM IMS-STATUSKONTROLL                                           
034500     .                                                                    
034600     EJECT                                                                
034700 IMS-GET-WDR501 SECTION.                                                  
034800                                                                          
034900     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
035000          DELIMITED BY SIZE INTO SSA1                                     
035100     MOVE '  GE' TO GODK-STATUSKODER                                      
035200     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-WDR501 SSA1                   
035300     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
035400     PERFORM IMS-STATUSKONTROLL                                           
035500     .                                                                    
035600     SKIP3                                                                
035700 IMS-ISRT-WDR501 SECTION.                                                 
035800                                                                          
035900     MOVE 'WDR501   ' TO SSA1                                             
036000     MOVE '  II' TO GODK-STATUSKODER                                      
036100     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDR501 SSA1                  
036200     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
036300     PERFORM IMS-STATUSKONTROLL                                           
036400     ADD +1 TO CHKP-ANT                                                   
036500     .                                                                    
036600     EJECT                                                                
036700*IMS-GET-WDR550 SECTION.                                                  
036800*                                                                         
036900*    STRING 'WDR550  (WDGXKEY  =' W-WDGXKEY-X ')'                         
037000*         DELIMITED BY SIZE INTO SSA1                                     
037100*    MOVE '  GE' TO GODK-STATUSKODER                                      
037200*    CALL CBLTDLI USING GHNP WDR5-PCB DLI-IO-WDR550 SSA1                  
037300*    MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
037400*    PERFORM IMS-STATUSKONTROLL                                           
037500*    .                                                                    
037600     SKIP3                                                                
037700 IMS-ISRT-WDR550 SECTION.                                                 
037800                                                                          
037900     MOVE 'WDR550   ' TO SSA1                                             
038000     MOVE '  II' TO GODK-STATUSKODER                                      
038100     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDR550 SSA1                  
038200     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
038300     PERFORM IMS-STATUSKONTROLL                                           
038400     ADD +1 TO CHKP-ANT                                                   
038500     .                                                                    
038600     EJECT                                                                
038700 IMS-RESTART SECTION.                                                     
038800     SKIP2                                                                
038900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
039000     MOVE '  ' TO GODK-STATUSKODER                                        
039100     CALL CBLTDLI USING XRST MSG-PCB                                      
039200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
039300                        CHKP-AREA-LENGTH CHKP-AREA                        
039400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039500     PERFORM IMS-STATUSKONTROLL                                           
039600     .                                                                    
039700     SKIP3                                                                
039800 IMS-CHECKPOINT SECTION.                                                  
039900     SKIP2                                                                
040000     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
040100     MOVE '  XD' TO GODK-STATUSKODER                                      
040200     CALL CBLTDLI USING CHKP MSG-PCB                                      
040300                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
040400                        CHKP-AREA-LENGTH CHKP-AREA                        
040500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040600     PERFORM IMS-STATUSKONTROLL                                           
040700                                                                          
040800     IF IMS-EJ-OK                                                         
040900       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
041000       DISPLAY FELTEXT                                                    
041100       CALL FELLOG                                                        
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 IMS-STATUSKONTROLL SECTION.                                              
041600     SKIP2                                                                
041700     SET STATUS-IX TO 1                                                   
041800     SEARCH GODK-STATUS                                                   
041900       AT END                                                             
042000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
042100           DELIMITED BY SIZE INTO FELTEXT                                 
042200         DISPLAY FELTEXT                                                  
042300         CALL FELLOG                                                      
042400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
042500         CONTINUE                                                         
042600     END-SEARCH                                                           
042700     .                                                                    

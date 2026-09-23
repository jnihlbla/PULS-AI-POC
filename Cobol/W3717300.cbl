000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3717300.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   00/04/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PGM UPPDATERAR LARM-KÖ ENLIGT NEDAN:                             
001000*        1 UPPLÄGG AV NYA LARM                                            
001100*        2 BORTTAG AV INAKTUELLA LARM                                     
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDR5                                       
001400*                                                                         
001500                                                                          
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800                                                                          
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200*          --- SUMMERADE LARMPOSTER                                       
002300     SELECT W37172                     ASSIGN TO W37173D1.                
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 FILE SECTION.                                                            
002800 FD  W37172                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100*01  POST -COPY W37172 -PRE  IN-  -L.                                     
003200     EJECT                                                                
003300                                                                          
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(8)    VALUE 'W3717300'.            
003800 01  CHKP-VAR.                                                            
003900     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004000     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004100     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004200     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004300     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004400     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700*                                                                         
004800*01  -COPY WWDCKONS                                                       
004900                                                                          
005000 01  FELTEXT.                                                             
005100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005300                                                                          
005400 77  W37172-EOF-SW               PIC X       VALUE 'N'.                   
005500     88  END-OF-W37172                       VALUE 'J'.                   
005600                                                                          
005700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES DAGENS-DATUM.                                       
005900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006200     EJECT                                                                
006300                                                                          
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500*                                                                         
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006900     EJECT                                                                
007000                                                                          
007100*    --- PARAMETRAR TILL POSTSUM                                          
007200*                                                                         
007300*01  -COPY W0005   -PRE  POSTSUM-                                         
007400     EJECT                                                                
007500                                                                          
007600 01  IN-AREA-START               PIC X(24)   VALUE                        
007700                                             'IN-AREA-START'.             
007800*01  -COPY W37172    -PRE IN-                                             
007900     EJECT                                                                
008000                                                                          
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200 01  NYCKLAR-TILL-DLI.                                                    
008300     03  W-WDGXKEY-2223-X.                                                
008400         05  W-IDHTYP-2223       PIC X(4)    VALUE '2223'.                
008500         05  W-IDANSK-2223       PIC S9(3)   COMP-3 VALUE ZERO.           
008600         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
008700     03  W-WDGXKEY-2224-X.                                                
008800         05  W-TISENBEK-DAG-2224 PIC S9(7)   COMP-3 VALUE ZERO.           
008900         05  W-TISENBEK-KL-2224  PIC S9(7)   COMP-3 VALUE ZERO.           
009000         05  W-KDLARM-2224       PIC S9(3)   COMP-3 VALUE 400.            
009100     03  W-IDARTNR-2224-X.                                                
009200         05  W-IDARTNR-2224      PIC S9(9)   COMP-3 VALUE ZERO.           
009310     03  W-IDDC-X.                                                        
009320         05  W-IDDC          PIC X(2)   VALUE SPACE.                      
009400*    --- STATUS-KOD FRÅN IMS                                              
009500 01  STATUS-WS                   PIC XX.                                  
009600     88  SEGMENT-FINNS                       VALUE '  '.                  
009700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009800     88  IMS-EJ-OK                           VALUE 'XD'.                  
009900                                                                          
010000 01  GODK-STATUSKODER.                                                    
010100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200                                                                          
010300 01  SSA1                        PIC X(64).                               
010400 01  SSA2                        PIC X(96).                               
010500     EJECT                                                                
010600                                                                          
010700*    --- IMS FUNKTIONSKODER                                               
010800*01  -COPY W0003                                                          
010900     EJECT                                                                
011000                                                                          
011100*    ---  DLI INPUT-OUTPUT AREA                                           
011200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2223'.                    
011300 01  DLI-IO-WDGX2223.                                                     
011400*    03  -COPY WDGX2223                                                   
011500     EJECT                                                                
011600                                                                          
011700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2224'.                    
011800 01  DLI-IO-WDGX2224.                                                     
011900*    03  -COPY WDGX2224                                                   
012100     EJECT                                                                
012200                                                                          
012300 LINKAGE SECTION.                                                         
012400*01  -COPY W0009   -PRE MSG-                                              
012500                                                                          
012600*01  -COPY W0008  -PRE 2223-                                              
012700     05  FILLER                  PIC X.                                   
012800     EJECT                                                                
012900                                                                          
013000 PROCEDURE DIVISION  USING MSG-PCB 2223-PCB.                              
013100 MAIN SECTION.                                                            
013200     ENTRY 'DLITCBL' USING MSG-PCB 2223-PCB.                              
013300                                                                          
013400     PERFORM A-INITIATE                                                   
013500                                                                          
013600     PERFORM B-EXECUTE                                                    
013700                                                                          
013800     PERFORM Z-FINISH                                                     
013900                                                                          
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400                                                                          
014500 A-INITIATE SECTION.                                                      
014600     PERFORM IMS-RESTART                                                  
014700                                                                          
014800     OPEN INPUT W37172                                                    
014900                                                                          
015000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400 B-EXECUTE SECTION.                                                       
015500     PERFORM S01-READ-W37172                                              
015600                                                                          
015700     PERFORM UNTIL END-OF-W37172                                          
015800       MOVE IN-IDANSK           TO W-IDANSK-2223                          
015900       IF IN-KDBEH = 'D'                                                  
016000*  BORTTAG AV INAKTUELLT LARM                                             
016100         PERFORM IMS-GHU-WDGX2223                                         
016200         IF SEGMENT-FINNS                                                 
016300           MOVE IN-IDARTNR      TO W-IDARTNR-2224                         
016310           MOVE WC-CDC-SE       TO W-IDDC                                 
016400           PERFORM IMS-GHNP-WDGX2224                                      
016500           IF SEGMENT-FINNS                                               
016600             PERFORM IMS-DLET-WDGX2224                                    
016700             ADD +1             TO CHKP-ANT                               
016800           END-IF                                                         
016900         END-IF                                                           
017000       ELSE                                                               
017100                                                                          
017200*  UPPLÄGG AV NYTT LARM ALT. UPDATERING AV BEFINTLIGT LARM                
017300         PERFORM IMS-GHU-WDGX2223                                         
017400         IF SEGMENT-SAKNAS                                                
017500           MOVE '2223'          TO 2223-IDHTYP                            
017600           MOVE IN-IDANSK       TO 2223-IDANSK                            
017700           MOVE LOW-VALUE       TO 2223-LOW-VALUE                         
017800           PERFORM IMS-ISRT-WDGX2223                                      
017900           ADD +1               TO CHKP-ANT                               
018000         END-IF                                                           
018100                                                                          
018200         PERFORM IMS-GHU-WDGX2223                                         
018300         MOVE IN-IDARTNR        TO W-IDARTNR-2224                         
018310         MOVE WC-CDC-SE         TO W-IDDC                                 
018400         PERFORM IMS-GHNP-WDGX2224                                        
018500         IF SEGMENT-SAKNAS                                                
018600           PERFORM IMS-GHU-WDGX2223                                       
018700           PERFORM BA-BUILD-WDGX2224                                      
018800           PERFORM IMS-ISRT-WDGX2224                                      
018900         ELSE                                                             
019000           MOVE NEJ             TO 2224-FLNYLARM                          
019100           MOVE IN-TIREGDAT     TO 2224-TIREGDAT                          
019200           PERFORM IMS-REPL-WDGX2224                                      
019300         END-IF                                                           
019400         ADD +1                 TO CHKP-ANT                               
019500       END-IF                                                             
019600                                                                          
019700       IF CHKP-ANT > CHKP-MAX                                             
019800         PERFORM X-CHECKPOINT                                             
019900       END-IF                                                             
020000       PERFORM S01-READ-W37172                                            
020100     END-PERFORM                                                          
020200     .                                                                    
020300     EJECT                                                                
020400                                                                          
020500 BA-BUILD-WDGX2224 SECTION.                                               
020600      MOVE FUNCTION CURRENT-DATE(3:6)    TO  2224-TISENBEK-DAG            
020700      MOVE FUNCTION CURRENT-DATE(11:6)   TO  2224-TISENBEK-KL             
020800     MOVE W-KDLARM-2224       TO 2224-KDLARM                              
020900     MOVE W-IDARTNR-2224      TO 2224-IDARTNR                             
020910     MOVE WC-CDC-SE           TO 2224-IDDC                                
021000     MOVE JA                  TO 2224-FLNYLARM                            
021100     MOVE ZERO                TO 2224-IDDISTR                             
021200                                 2224-IDKUNDNR                            
021300     MOVE '0000000   '        TO 2224-IDKUNDRF                            
021400     MOVE 1                   TO 2224-IDLOPNR                             
021500     MOVE IN-TIREGDAT         TO 2224-TIREGDAT                            
021700     MOVE SPACE               TO 2224-IDTRANS                             
021800                                 2224-KDMFSFOR                            
021900     MOVE ZERO                TO 2224-IDKR                                
021910     MOVE SPACE               TO 2224-IDLEVNR                             
022000     .                                                                    
022100     EJECT                                                                
022200                                                                          
022300 Z-FINISH SECTION.                                                        
022400     CLOSE W37172                                                         
022500                                                                          
022600     MOVE 'S' TO POSTSUM-OPKOD                                            
022700     CALL POSTSUM USING POSTSUM-PARM                                      
022800     .                                                                    
022900     EJECT                                                                
023000                                                                          
023100 S01-READ-W37172  SECTION.                                                
023200     READ W37172 INTO IN-W37172                                           
023300     AT END                                                               
023400        MOVE HIGH-VALUE   TO IN-W37172                                    
023500        SET END-OF-W37172 TO TRUE                                         
023600                                                                          
023700     NOT AT END                                                           
023800        MOVE 'W37172'     TO POSTSUM-FDNAMN                               
023900        MOVE 'W37173D1'   TO POSTSUM-DDNAMN2                              
024000        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
024100        CALL POSTSUM USING POSTSUM-PARM                                   
024200                                                                          
024300     END-READ                                                             
024400     .                                                                    
024500     EJECT                                                                
024600                                                                          
024700 X-CHECKPOINT   SECTION.                                                  
024800     PERFORM IMS-CHECKPOINT                                               
024900     MOVE ZERO TO CHKP-ANT                                                
025000     .                                                                    
025100     EJECT                                                                
025200                                                                          
025300* --- IMS SEKTIONER ---                                                   
025400 IMS-GHU-WDGX2223 SECTION.                                                
025500     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
025600          DELIMITED BY SIZE INTO SSA1                                     
025700     MOVE '  GE'           TO GODK-STATUSKODER                            
025800     CALL CBLTDLI USING GHU 2223-PCB DLI-IO-WDGX2223 SSA1                 
025900     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
026000     PERFORM IMS-STATUSCONTROL                                            
026100     .                                                                    
026200                                                                          
026300 IMS-ISRT-WDGX2223 SECTION.                                               
026400     MOVE 'WDR501   '      TO SSA1                                        
026500     MOVE '  II'           TO GODK-STATUSKODER                            
026600     CALL CBLTDLI USING ISRT 2223-PCB DLI-IO-WDGX2223 SSA1                
026700     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
026800     PERFORM IMS-STATUSCONTROL                                            
026900     .                                                                    
027000                                                                          
027100 IMS-GHNP-WDGX2224 SECTION.                                               
027200     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2223-X ')'                    
027300          DELIMITED BY SIZE INTO SSA1                                     
027400     STRING 'WDR550  (IDARTNR  =' W-IDARTNR-2224-X                        
027410                    '&IDDC     =' W-IDDC-X ')'                            
027500          DELIMITED BY SIZE INTO SSA2                                     
027600     MOVE '  GE'           TO GODK-STATUSKODER                            
027700     CALL CBLTDLI USING GHNP 2223-PCB DLI-IO-WDGX2224 SSA1 SSA2           
027800     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
027900     PERFORM IMS-STATUSCONTROL                                            
028000     .                                                                    
028100                                                                          
028200 IMS-ISRT-WDGX2224 SECTION.                                               
028300     MOVE 'WDR550   '      TO SSA1                                        
028400     MOVE '  II'           TO GODK-STATUSKODER                            
028500     CALL CBLTDLI USING ISRT 2223-PCB DLI-IO-WDGX2224 SSA1                
028600     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
028700     PERFORM IMS-STATUSCONTROL                                            
028800     .                                                                    
028900                                                                          
029000 IMS-REPL-WDGX2224 SECTION.                                               
029100     MOVE '  '             TO GODK-STATUSKODER                            
029200     CALL CBLTDLI USING REPL 2223-PCB DLI-IO-WDGX2224                     
029300     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
029400     PERFORM IMS-STATUSCONTROL                                            
029500     .                                                                    
029600                                                                          
029700 IMS-DLET-WDGX2224 SECTION.                                               
029800     MOVE '  '             TO GODK-STATUSKODER                            
029900     CALL CBLTDLI USING DLET 2223-PCB DLI-IO-WDGX2224                     
030000     MOVE 2223-STATUS-CODE TO STATUS-WS                                   
030100     PERFORM IMS-STATUSCONTROL                                            
030200     .                                                                    
030300     EJECT                                                                
030400                                                                          
030500 IMS-RESTART SECTION.                                                     
030600     MOVE SPACE           TO CHKP-MSG-IO-AREA                             
030700     MOVE '  '            TO GODK-STATUSKODER                             
030800     CALL CBLTDLI USING XRST MSG-PCB                                      
030900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
031000                        CHKP-AREA-LENGTH CHKP-AREA                        
031100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031200     PERFORM IMS-STATUSCONTROL                                            
031300     .                                                                    
031400                                                                          
031500 IMS-CHECKPOINT SECTION.                                                  
031600     MOVE SPACE           TO CHKP-MSG-IO-AREA                             
031700     MOVE '  XD'          TO GODK-STATUSKODER                             
031800     CALL CBLTDLI USING CHKP MSG-PCB                                      
031900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
032000                        CHKP-AREA-LENGTH CHKP-AREA                        
032100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
032200     PERFORM IMS-STATUSCONTROL                                            
032300                                                                          
032400     IF IMS-EJ-OK                                                         
032500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
032600       DISPLAY FELTEXT                                                    
032700       CALL FELLOG                                                        
032800     END-IF                                                               
032900     .                                                                    
033000     EJECT                                                                
033100                                                                          
033200 IMS-STATUSCONTROL SECTION.                                               
033300     SET STATUS-IX TO 1                                                   
033400     SEARCH GODK-STATUS                                                   
033500       AT END                                                             
033600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033700           DELIMITED BY SIZE INTO FELTEXT                                 
033800         DISPLAY FELTEXT                                                  
033900         CALL FELLOG                                                      
034000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034100         CONTINUE                                                         
034200     END-SEARCH                                                           
034300     .                                                                    

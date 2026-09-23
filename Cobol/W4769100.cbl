000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4769100.                                                
000400 AUTHOR.         STINA MOGREN.                                            
000500 DATE-WRITTEN.   05/07/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        REFILLING OF DATABASE.                                           
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WLINLC (WDL6)                              
001200*        PROGRAMMET UPPDATERAR WL6301 (WDR5)                              
001300*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
001400*        PROGRAMMET UPPDATERAR WLLOGA (WDL9)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000*    PROGRAMMET ÄR EN KOPIA AV W4752A                                     
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- FAKTURAINFO                                                
003000     SELECT W47664                     ASSIGN TO W47691D1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W47664                                                               
003700     RECORDING       V                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000 01  IN-FAKT-HUVUD.                                                       
004100*03  -COPY W4766401     -L.                                               
004200     SKIP2                                                                
004300 01  IN-FAKT-KOLLI.                                                       
004400*03  -COPY W4766402     -L.                                               
004500                                                                          
004600 01  IN-FAKT-RAD.                                                         
004700*03  -COPY W4766403     -L.                                               
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W4769100'.            
005200 01  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005300 01  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005400 01  FELTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005700                                                                          
005800 01  CHKP-VAR.                                                            
005900 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
006000 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
006100 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
006200 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
006300 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
006400 03  CHKP-MAX                    PIC S9(3)   VALUE +10.                   
006500 77  MAX-TAB-IX                  PIC S9(4)   VALUE +14 COMP SYNC.         
006600 77  JA                          PIC X       VALUE 'J'.                   
006700 77  NEJ                         PIC X       VALUE 'N'.                   
006800 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
006900 77  WS-KDVALISO-HUV             PIC X(3)    VALUE 'SEK'.                 
007000     SKIP2                                                                
007100*01  -COPY WWDCKONS                                                       
007200 77  MOT-WS-IDDC                 PIC X(2)    VALUE SPACE.                 
007300                                                                          
007400 01  W-IDDCTEXT-MSGI.                                                     
007500     03  FILLER              PIC X(5)   VALUE 'WIDDC'.                    
007600     03  W-IDDC-MSGI         PIC X(2).                                    
007700     EJECT                                                                
007800 01  POST-ANT                    PIC S9(7)   VALUE ZERO COMP-3.           
007900                                                                          
008000 01  FILLER          PIC X(16)   VALUE 'SPAR-FAELT START'.                
008100 01  SPAR-ANTAL-KOLLI            PIC S9(7)   VALUE ZERO  COMP-3.          
008200 01  SPAR-ANTAL-RADER            PIC S9(7)   VALUE ZERO  COMP-3.          
008300 01  SPAR-IDKUNDNR               PIC S9(7)   VALUE ZERO  COMP-3.          
008400 01  SPAR-IDLBBET                PIC X(12)   VALUE SPACE.                 
008500 01  SPAR-IDFAKT                 PIC S9(7)   VALUE ZERO  COMP-3.          
008600 01  SPAR-IDKOLLI                PIC S9(5)   VALUE ZERO  COMP-3.          
008700 01  SPAR-KDKOLLI                PIC X(8)    VALUE SPACE.                 
008800 01  SPAR-IDKUNDRF               PIC X(10)   VALUE SPACE.                 
008900 01  SPAR-IDDISTR                PIC S9(5)   VALUE ZERO  COMP-3.          
009000 01  SPAR-KDFRAKT                PIC S9(3)   VALUE ZERO  COMP-3.          
009100 01  SPAR-KDVALISO               PIC X(3)    VALUE SPACE.                 
009200 01  SPAR-TIFAKT                 PIC S9(7)   VALUE ZERO  COMP-3.          
009300 01  SPAR-DABERANK               PIC  9(8)   VALUE ZERO.                  
009400 01  SPAR-PRKURS              PIC S9(6)V9(5) VALUE ZERO  COMP-3.          
009500 01  SPAR-IDSHIPM                PIC 9(7)    VALUE ZERO.                  
009600                                                                          
009700 77  RKOD-ABEND-UTAN-DUMP       PIC S9(4)   VALUE +16 COMP SYNC.          
009800 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +1000 COMP SYNC.        
009900                                                                          
010000 01  WS-ARBETSAREA.                                                       
010100*    DATE + TIME  FÖR SKAPANDE AV INLEVERANSNUMMER                        
010200     03  WS-TIAAAAMMDDTTMMSSTH   PIC 9(16)  VALUE ZERO.                   
010300     03  FILLER REDEFINES WS-TIAAAAMMDDTTMMSSTH.                          
010400         05  WS-TISEKEL          PIC 9(2).                                
010500         05  WS-TIAAMMDD-DATE    PIC 9(6).                                
010600         05  WS-TTMMSSTH-TIME    PIC 9(8).                                
010700     03  WS-DAINLEV              PIC 9(16)  VALUE ZERO.                   
010800 01  WS-TAB-IDDC                 PIC X(2)   VALUE SPACE.                  
010900                                                                          
011000 01 DB2-LASNING.                                                          
011100     03 FILLER                   PIC X(16)   VALUE                        
011200                                             'WS-DB2-SEKTION'.            
011300     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
011400                                                                          
011500                                                                          
011600 01 NYCKLAR-TP4TRAN.                                                      
011700     03 W-TP4TRAN-IDDISTR        PIC S9(5)   COMP-3 VALUE ZERO.           
011800                                                                          
011900 01  TEST-IDDISTR                PIC 9(5)    VALUE ZERO COMP-3.           
012000*01  FILLER  -COPY WWDIST35    -RED  TEST-IDDISTR.                        
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)  VALUE 'REFILLTABDC'.          
012300*   -COPY WWDIST57                                                        
012400     EJECT                                                                
012500                                                                          
012600 77  W47664-EOF-SW               PIC X       VALUE 'N'.                   
012700     88  END-OF-W47664                       VALUE 'J'.                   
012800     EJECT                                                                
012900 01  WS-DAGENS-DATUM             PIC 9(8).                                
013000 01  WS-TID                      PIC 9(9).                                
013100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
013200 01  FILLER REDEFINES DAGENS-DATUM.                                       
013300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013600     SKIP3                                                                
013700 01  WS-AAMMDD                   PIC 9(6).                                
013800 01  FILLER REDEFINES WS-AAMMDD.                                          
013900     03  WS-AA               PIC 9(2).                                    
014000     03  FILLER              PIC 9(4).                                    
014100     EJECT                                                                
014200 01  WS-KVAKS-PAV            PIC 9(7)    VALUE ZERO.                      
014300     EJECT                                                                
014400 01  DYNAMISKA-SUBPROGRAM.                                                
014500*                                                                         
014600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
014900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015000     03  W218ETA                 PIC X(8)    VALUE 'W218ETA '.            
015100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
015200     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
015300     EJECT                                                                
015400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015500*01 -COPY WMSGINIT                                                        
015600     EJECT                                                                
015700*    --- PARAMETRAR TILL POSTSUM                                          
015800*01  -COPY W0005     -PRE  POSTSUM-                                       
015900     EJECT                                                                
016000*    --- PARAMETRAR TILL W510CURR                                         
016100*01  -COPY W510CURR                                                       
016200     EJECT                                                                
016300*    --- PARAMETRAR TILL SUBPROGRAM W218ETA                               
016400 01  FILLER                     PIC X(16) VALUE 'W218LETA START'.         
016500*01  -COPY W218LETA  -PRE ETA-                                            
016600 01  FILLER                     PIC X(12) VALUE 'DUMMY ARTC'.             
016700 01  ETA-ARTC-PCB               PIC X(1).                                 
016800 01  FILLER                     PIC X(12) VALUE 'DUMMY ARTS'.             
016900 01  ETA-ARTS-PCB               PIC X(1).                                 
017000 01  FILLER                     PIC X(12) VALUE 'DUMMY INLC'.             
017100 01  ETA-INLC-PCB               PIC X(1).                                 
017200 01  FILLER                     PIC X(12) VALUE 'DUMMY LEVA'.             
017300 01  ETA-LEVA-PCB               PIC X(1).                                 
017400     EJECT                                                                
017500 01  IN-AREA-START               PIC X(24)   VALUE                        
017600                                             'IN-AREA-START'.             
017700     SKIP2                                                                
017800 01  IN-AREA.                                                             
017900     03  POSTAREA -COPY W4766401 -PRE FAKT-.                              
018000     EJECT                                                                
018100     03  AREA     -COPY W4766402 -PRE FAKT- -RED FAKT-POSTAREA.           
018200     EJECT                                                                
018300     03  AREA     -COPY W4766403 -PRE FAKT- -RED FAKT-POSTAREA.           
018400     EJECT                                                                
018500*                                                                         
018600     EJECT                                                                
018700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018800     SKIP3                                                                
018900 01  NYCKLAR-TILL-DLI.                                                    
019000                                                                          
019100     03  W-IDARTNR-X.                                                     
019200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019300                                                                          
019400     03  W-IDDC-X.                                                        
019500         05  W-IDDC-WDK7         PIC X(2)    VALUE SPACE.                 
019600                                                                          
019700     03  W-DAINLEV-X.                                                     
019800         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
019900                                                                          
020000     03  W-WDGXKEY-6301.                                                  
020100         05  FILLER              PIC X(4)    VALUE '6301'.                
020200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
020300         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
020400                                                                          
020500     03  W-WDGXKEY-6302.                                                  
020600         05  W-DABERANK          PIC  9(8).                               
020700         05  W-IDFAKT            PIC S9(7)   VALUE ZERO COMP-3.           
020800                                                                          
020900     03  W-WDGXKEY-X.                                                     
021000         05  W-IDHTYP            PIC X(4)    VALUE '4751'.                
021100         05  NYCKEL-VALFRI       PIC X(26)   VALUE LOW-VALUE.             
021200                                                                          
021300     03  W-IDDC-B6-X.                                                     
021400         05 W-IDDC-B6            PIC X(2).                                
021500                                                                          
021600     03  W-IDDC-B6-MOT-X.                                                 
021700         05 W-IDDC-B6-MOT        PIC X(2).                                
021800                                                                          
021900     03  W-IDDC-B6-SEND-X.                                                
022000         05 W-IDDC-B6-SEND       PIC X(2).                                
022100                                                                          
022200     SKIP2                                                                
022300*    --- STATUS-KOD FRÅN IMS                                              
022400 01  STATUS-WS                   PIC XX.                                  
022500     88  SEGMENT-FINNS                       VALUE '  '.                  
022600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
022900     88  IMS-EJ-OK                           VALUE 'XD'.                  
023000     SKIP2                                                                
023100 01  GODK-STATUSKODER.                                                    
023200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023300     SKIP3                                                                
023400 01  SSA1                        PIC X(64).                               
023500 01  SSA2                        PIC X(64).                               
023600     EJECT                                                                
023700*                            DB2 FUNKTIONSKODER                           
023800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
023900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
024000                                                                          
024100 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
024200 01  DB2-WS.                                                              
024300     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
024400         88  CURSOR-OK                       VALUE 000.                   
024500         88  RADER-FINNS                     VALUE 000.                   
024600         88  RADER-SAKNAS                    VALUE 100.                   
024700         88  ATKOMST-FEL                     VALUE 904.                   
024800     03  GODK-SQLCODEKODER.                                               
024900         05  GODK-SQLCODE OCCURS 5                                        
025000             INDEXED BY SQLCODE-IX PIC 9(3).                              
025100 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
025200     EJECT                                                                
025300*    --- IMS FUNKTIONSKODER                                               
025400*01  -COPY W0003                                                          
025500     EJECT                                                                
025600*    ---  DLI INPUT-OUTPUT AREA                                           
025700 01  FILLER                   PIC X(16)   VALUE 'WDL901       '.          
025800*01  WLLOGA01  -COPY WDL901                                               
025900     EJECT                                                                
026000 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDL601'.          
026100 01  DLI-IO-WDL601.                                                       
026200*    03  -COPY WDL601  -PRE INLC-                                         
026300     EJECT                                                                
026400 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDL611'.          
026500 01  DLI-IO-WDL611.                                                       
026600*    03  -COPY WDL611  -PRE INLC-                                         
026700     EJECT                                                                
026800 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-6301'.            
026900 01  DLI-IO-AREA-6301.                                                    
027000*    03  -COPY WDGX6301                                                   
027100     EJECT                                                                
027200 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-6302'.            
027300 01  DLI-IO-AREA-6302.                                                    
027400*    03  -COPY WDGX6302                                                   
027500     EJECT                                                                
027600 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDK701'.          
027700 01  DLI-IO-WDK701.                                                       
027800*    03  -COPY WDK701  -PRE ARTS-                                         
027900     EJECT                                                                
028000 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDK711'.          
028100 01  DLI-IO-WDK711.                                                       
028200*    03  -COPY WDK711  -PRE ARTS-                                         
028300     EJECT                                                                
028400 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-WDK611'.          
028500 01  DLI-IO-WDK611.                                                       
028600*    03  -COPY WDK611                                                     
028700     EJECT                                                                
028800 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-4751'.            
028900 01  DLI-IO-AREA-4751.                                                    
029000*    03  -COPY WDGX4751                                                   
029100     EJECT                                                                
029200 01  FILLER                   PIC X(16) VALUE 'WDB601 AREA'.              
029300 01   DLI-IO-AREA-B601.                                                   
029400*     03  -COPY WDB601                                                    
029500                                                                          
029600 01  FILLER                   PIC X(16) VALUE 'WDB601 MOT-AREA'.          
029700 01   DLI-IO-AREA-B601-MOT.                                               
029800*     03  -COPY WDB601  -PRE MOT-                                         
029900                                                                          
030000 01  FILLER                   PIC X(16) VALUE 'WDB601 SEND-AREA'.         
030100 01   DLI-IO-AREA-B601-SEND.                                              
030200*     03  -COPY WDB601  -PRE SEND-                                        
030300                                                                          
030400     EJECT                                                                
030500 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
030600                                                                          
030700*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
030800     EJECT                                                                
030900     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
031000     EJECT                                                                
031100 LINKAGE SECTION.                                                         
031200                                                                          
031300*01  -COPY W0009  -PRE MSG-                                               
031400     EJECT                                                                
031500*01  -COPY W0009  -PRE WDP7-                                              
031600                                                                          
031700*01  -COPY W0008  -PRE WDL9-                                              
031800     05  FILLER                  PIC X.                                   
031900     EJECT                                                                
032000*01  -COPY W0008  -PRE WDL6-                                              
032100     05  FILLER                  PIC X.                                   
032200     EJECT                                                                
032300*01  -COPY W0008  -PRE 6301-                                              
032400     05  FILLER                  PIC X.                                   
032500     EJECT                                                                
032600*01  -COPY W0008  -PRE WDK7-                                              
032700     05  FILLER                  PIC X.                                   
032800     EJECT                                                                
032900*01  -COPY W0008  -PRE 4751-                                              
033000     05  FILLER                  PIC X.                                   
033100     EJECT                                                                
033200*01  -COPY W0008  -PRE WDG2-                                              
033300     05  FILLER                  PIC X.                                   
033400     EJECT                                                                
033500*01  -COPY W0008  -PRE WDB6-                                              
033600     05  FILLER                  PIC X.                                   
033700     EJECT                                                                
033800*01  -COPY W0008  -PRE WDK6-                                              
033900     05  FILLER                  PIC X.                                   
034000     EJECT                                                                
034100 01  ETA-WDB6-PCB                PIC X.                                   
034200     EJECT                                                                
034300                                                                          
034400 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB                               
034500                                   WDL6-PCB 6301-PCB WDK7-PCB             
034600                                   4751-PCB WDG2-PCB WDL9-PCB             
034700                                   WDB6-PCB WDK6-PCB                      
034800                                   ETA-WDB6-PCB.                          
034900 MAIN SECTION.                                                            
035000     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB                               
035100                                   WDL6-PCB 6301-PCB WDK7-PCB             
035200                                   4751-PCB WDG2-PCB WDL9-PCB             
035300                                   WDB6-PCB WDK6-PCB                      
035400                                   ETA-WDB6-PCB.                          
035500                                                                          
035600     PERFORM A-INIT                                                       
035700                                                                          
035800     IF NOT END-OF-W47664                                                 
035900                                                                          
036000       PERFORM UNTIL END-OF-W47664                                        
036100                                                                          
036200         PERFORM B-BEHANDLA-INFIL                                         
036300                                                                          
036400         PERFORM S01-LAES-W47664                                          
036500       END-PERFORM                                                        
036600                                                                          
036700       MOVE SPAR-IDDISTR       TO TEST-IDDISTR                            
036800       PERFORM S10-TEST-AV-IDDISTR                                        
036900                                                                          
037000*      IF DIST35-JP-NDC-RETURNS                                           
037100*        MOVE '6A' TO MOT-WS-IDDC                                         
037200*      END-IF                                                             
037300                                                                          
037400       MOVE MOT-WS-IDDC        TO W-IDDC                                  
037500                                                                          
037600       PERFORM IMS-GHU-6301                                               
037700       IF SEGMENT-SAKNAS                                                  
037800         MOVE '6301'           TO 6301-IDHTYP                             
037900         MOVE MOT-WS-IDDC      TO 6301-IDDC                               
038000         PERFORM IMS-ISRT-6301                                            
038100         ADD +1 TO CHKP-ANT                                               
038200       END-IF                                                             
038300                                                                          
038400       MOVE SPAR-ANTAL-KOLLI   TO 6302-KVKOLLI-FAKT                       
038500       MOVE SPAR-IDKUNDNR      TO 6302-IDKUNDNR                           
038600       MOVE SPAR-IDKUNDRF      TO 6302-IDKUNDRF                           
038700       MOVE SPAR-ANTAL-RADER   TO 6302-KVRADER-FAKT                       
038800                                                                          
038900******************************************************************        
039000*                                                                         
039100*  KOLLA OM KVALITETSTRANSFER (GER SVARET RADER-FINNS)                    
039200*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
039300*                                                                         
039400******************************************************************        
039500                                                                          
039600       IF DIST35-RETUR AND NOT DIST35-CDC-NL-RETUR                        
039700         IF DIST35-RETUR-Q                                                
039800           MOVE 'QUALITY'      TO 6302-IDLBBET                            
039900         ELSE                                                             
040000           MOVE SPAR-IDLBBET   TO 6302-IDLBBET                            
040100         END-IF                                                           
040200       ELSE                                                               
040300         MOVE SPAR-IDDISTR     TO W-TP4TRAN-IDDISTR                       
040400                                                                          
040500         PERFORM DB2-SELECT-TP4TRAN                                       
040600                                                                          
040700         IF  RADER-FINNS                                                  
040800         AND TP4TRAN-KDARBTYP = 'QUAL'                                    
040900           MOVE 'QUALITY'      TO 6302-IDLBBET                            
041000         ELSE                                                             
041100           MOVE SPAR-IDLBBET   TO 6302-IDLBBET                            
041200         END-IF                                                           
041300       END-IF                                                             
041400                                                                          
041500       PERFORM IMS-ISRT-6302                                              
041600                                                                          
041700     END-IF                                                               
041800                                                                          
041900     PERFORM Z-FINIT                                                      
042000                                                                          
042100     MOVE ZERO TO RETURN-CODE                                             
042200     GOBACK                                                               
042300     .                                                                    
042400     EJECT                                                                
042500 A-INIT SECTION.                                                          
042600     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
042700     SKIP2                                                                
042800                                                                          
042900     OPEN INPUT W47664                                                    
043000     MOVE +0   TO POST-ANT                                                
043100                                                                          
043200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
043300                                                                          
043400     MOVE ZERO TO SPAR-IDFAKT                                             
043500     MOVE ZERO TO SPAR-IDSHIPM                                            
043600                                                                          
043700     PERFORM IMS-RESTART                                                  
043800     PERFORM IMS-LAS-ATERSTART                                            
043900**                                                                        
044000     IF SEGMENT-SAKNAS                                                    
044100        MOVE ZERO       TO 4751-WDGX4751-CTX                              
044200        MOVE '1'        TO 4751-KDSEGKEY                                  
044300        MOVE SPACES     TO 4751-IDKUNDRF                                  
044400                           4751-FILLERX5                                  
044500        PERFORM IMS-ISRT-ATERSTART                                        
044600     END-IF                                                               
044700**  + GE I LÄSNING                                                        
044800                                                                          
044900     IF 4751-KVPOST > +0                                                  
045000       PERFORM AA-LAES-FRAM-TILL-CHKPOINT                                 
045100     ELSE                                                                 
045200       PERFORM S01-LAES-W47664                                            
045300       IF NOT END-OF-W47664                                               
045400         MOVE FAKT-001-IDFAKT    TO SPAR-IDFAKT                           
045500         MOVE FAKT-001-IDSHIPM   TO SPAR-IDSHIPM                          
045600       END-IF                                                             
045700     END-IF                                                               
045800                                                                          
045900     MOVE FUNCTION CURRENT-DATE (3:2) TO W-DATE-AAMM(1:2)                 
046000     MOVE FUNCTION CURRENT-DATE (5:2) TO W-DATE-AAMM(3:2)                 
046100     .                                                                    
046200     EJECT                                                                
046300                                                                          
046400 AA-LAES-FRAM-TILL-CHKPOINT SECTION.                                      
046500     MOVE 'AA-LAES-FRAM    ' TO CURRENT-SECTION                           
046600                                                                          
046700     PERFORM S01-LAES-W47664                                              
046800     ADD +1 TO POST-ANT                                                   
046900                                                                          
047000     PERFORM UNTIL END-OF-W47664    OR                                    
047100                     POST-ANT = 4751-KVPOST                               
047200                                                                          
047300         IF FAKT-001-IDPTYP = '001'                                       
047400           MOVE FAKT-001-IDFAKT    TO SPAR-IDFAKT                         
047500                                      6302-IDFAKT                         
047600                                      W-IDFAKT                            
047700           MOVE FAKT-001-IDSHIPM   TO SPAR-IDSHIPM                        
047800                                      6302-IDSHIPM                        
047900                                                                          
048000           MOVE FAKT-001-IDDISTR   TO TEST-IDDISTR                        
048100           PERFORM S10-TEST-AV-IDDISTR                                    
048200                                                                          
048300           IF DCS-SDC AND NOT DCS-CHINA                                   
048400             MOVE FAKT-001-TIFAKT TO 6302-DABERANK                        
048500                                      W-DABERANK                          
048600                                      SPAR-DABERANK                       
048700             IF FAKT-001-TIFAKT NOT = ZERO                                
048800               IF FAKT-001-TIFAKT < 500000                                
048900                 MOVE 20          TO 6302-DABERANK (1:2)                  
049000               ELSE                                                       
049100                 IF FAKT-001-TIFAKT < 999999                              
049200                   MOVE 19        TO 6302-DABERANK (1:2)                  
049300                 ELSE                                                     
049400                   MOVE 99999999  TO 6302-DABERANK                        
049500                 END-IF                                                   
049600               END-IF                                                     
049700             END-IF                                                       
049800           ELSE                                                           
049900             IF DIST35-RETUR                                              
050000            AND NOT (DIST35-CDC-NL-RETUR OR DIST35-JP-NDC-RETURNS)        
050100               MOVE ZERO            TO 6302-DABERANK                      
050200                                       W-DABERANK                         
050300                                       SPAR-DABERANK                      
050400             ELSE                                                         
050500               PERFORM S11-BERAKNA-ETA                                    
050600               IF ETA-SVAR-OK = SPACE OR JA                               
050700                 MOVE ETA-TIAAMMDD-SVAR  TO 6302-DABERANK                 
050800                                            W-DABERANK                    
050900                                            SPAR-DABERANK                 
051000                 IF ETA-TIAAMMDD-SVAR NOT = ZERO                          
051100                   IF ETA-TIAAMMDD-SVAR < 500000                          
051200                     MOVE 20        TO 6302-DABERANK (1:2)                
051300                   ELSE                                                   
051400                     IF ETA-TIAAMMDD-SVAR < 999999                        
051500                       MOVE 19      TO 6302-DABERANK (1:2)                
051600                     ELSE                                                 
051700                       MOVE 99999999 TO 6302-DABERANK                     
051800                     END-IF                                               
051900                   END-IF                                                 
052000                 END-IF                                                   
052100               ELSE                                                       
052200                 MOVE ' FEL I SUBPGM W218ETA   ' TO FELTEXT               
052300                 CALL  ABEND USING RKOD-ABEND-MED-DUMP                    
052400               END-IF                                                     
052500             END-IF                                                       
052600           END-IF                                                         
052700                                                                          
052800           MOVE FAKT-001-IDDC      TO 6302-IDDC-SEND                      
052900           MOVE FAKT-001-IDDC-LEV  TO 6302-IDDC-LEV                       
053000           MOVE FAKT-001-IDDISTR   TO 6302-IDDISTR                        
053100                                      SPAR-IDDISTR                        
053200           MOVE SPACE              TO 6302-KDTRPSTA                       
053300                                      6302-ADINLOMR                       
053400                                      6302-IDBOKN                         
053500                                      6302-BETRPFIR                       
053600                                      6302-IDUSER-MANETA                  
053700           MOVE ZERO               TO 6302-KVKOLLI-MOT                    
053800                                      6302-KVRADER-MOT                    
053900                                      6302-KVRADER-PRIO                   
054000                                      6302-DABERANK-DISCH                 
054100                                      6302-DABERANK-PROP                  
054200                                      6302-TILST-CALLP44                  
054300                                      6302-TILST-PUSHEVNT                 
054400                                      6302-IDSUBSCR                       
054500                                      6302-IDCONTNR                       
054600                                      6302-DABERANK-LIFDEPPL              
054700                                      6302-DABERANK-LIFDEPAC              
054800                                      6302-DABERANK-PODDEPPL              
054900                                      6302-DABERANK-PODDEPAC              
055000                                      6302-DABERANK-DLVDELPL              
055100                                      6302-DABERANK-DLVDELAC              
055200                                      6302-DABERANK-PODDISPL              
055300                                      6302-DABERANK-PODDISAC              
055400                                      6302-DABERANK-PODARRPL              
055500                                      6302-DABERANK-LIFARRAC              
055600           MOVE NEJ                TO 6302-FLMANETA                       
055700           MOVE FAKT-001-TIFAKT    TO 6302-TIFAKT                         
055800                                      SPAR-TIFAKT                         
055900                                                                          
056000           MOVE FAKT-001-IDLEVNR   TO 6302-IDLEVNR                        
056100           MOVE FAKT-001-KDFRAKT   TO SPAR-KDFRAKT                        
056200           MOVE FAKT-001-KDVALISO  TO SPAR-KDVALISO                       
056300           MOVE FAKT-001-PRKURS    TO SPAR-PRKURS                         
056400         ELSE                                                             
056500           IF FAKT-001-IDPTYP = '002'                                     
056600             ADD +1 TO SPAR-ANTAL-KOLLI                                   
056700           ELSE                                                           
056800             IF FAKT-001-IDPTYP = '003'                                   
056900               ADD +1 TO SPAR-ANTAL-RADER                                 
057000             END-IF                                                       
057100           END-IF                                                         
057200                                                                          
057300         END-IF                                                           
057400                                                                          
057500         ADD +1          TO POST-ANT                                      
057600         PERFORM S01-LAES-W47664                                          
057700                                                                          
057800     END-PERFORM                                                          
057900                                                                          
058000     IF FAKT-001-IDPTYP       = '002'                                     
058100       IF FAKT-002-IDFAKT       = 4751-IDFAKT AND                         
058200          FAKT-002-IDKOLLI      = 4751-IDKOLLI AND                        
058300          FAKT-002-IDKUNDRF     = 4751-IDKUNDRF AND                       
058400          FAKT-002-IDDISTR      = 4751-IDDISTR                            
058500          CONTINUE                                                        
058600       ELSE                                                               
058700         MOVE ' FEL POST VID ÅTERSTART ' TO FELTEXT                       
058800         CALL  ABEND USING RKOD-ABEND-UTAN-DUMP                           
058900       END-IF                                                             
059000     ELSE                                                                 
059100       MOVE ' FEL POST VID ÅTERSTART ' TO FELTEXT                         
059200       CALL    ABEND USING RKOD-ABEND-UTAN-DUMP                           
059300     END-IF                                                               
059400                                                                          
059500     IF END-OF-W47664                                                     
059600        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
059700                      TO FELTEXT                                          
059800        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
059900     END-IF                                                               
060000     .                                                                    
060100     EJECT                                                                
060200                                                                          
060300 B-BEHANDLA-INFIL SECTION.                                                
060400     MOVE 'B-BEHANDLA-INFIL' TO CURRENT-SECTION                           
060500     SKIP2                                                                
060600     ADD +1 TO POST-ANT                                                   
060700                                                                          
060800     EVALUATE FAKT-001-IDPTYP                                             
060900                                                                          
061000                                                                          
061100       WHEN '001'                                                         
061200         PERFORM BA-FAKTURA-HUV                                           
061300                                                                          
061400       WHEN '002'                                                         
061500         PERFORM BB-KOLLI                                                 
061600                                                                          
061700         IF CHKP-ANT > CHKP-MAX                                           
061800           PERFORM X-TAG-CHECKPOINT                                       
061900         END-IF                                                           
062000                                                                          
062100       WHEN '003'                                                         
062200         PERFORM BC-RAD                                                   
062300                                                                          
062400       WHEN OTHER                                                         
062500         PERFORM S99-FEL-POSTTYP                                          
062600                                                                          
062700     END-EVALUATE                                                         
062800     .                                                                    
062900     EJECT                                                                
063000                                                                          
063100 BA-FAKTURA-HUV SECTION.                                                  
063200     MOVE 'BA-FAKTURA-HUV  ' TO CURRENT-SECTION                           
063300     SKIP2                                                                
063400     IF FAKT-001-IDFAKT NOT = SPAR-IDFAKT                                 
063500                                                                          
063600       MOVE SPAR-IDDISTR      TO TEST-IDDISTR                             
063700       PERFORM S10-TEST-AV-IDDISTR                                        
063800                                                                          
063900       MOVE MOT-WS-IDDC       TO W-IDDC                                   
064000                                                                          
064100       PERFORM IMS-GHU-6301                                               
064200       IF SEGMENT-SAKNAS                                                  
064300         MOVE '6301'          TO 6301-IDHTYP                              
064400         MOVE MOT-WS-IDDC     TO 6301-IDDC                                
064500                                                                          
064600         PERFORM IMS-ISRT-6301                                            
064700         ADD +1 TO CHKP-ANT                                               
064800       END-IF                                                             
064900                                                                          
065000       MOVE SPAR-ANTAL-KOLLI  TO 6302-KVKOLLI-FAKT                        
065100       MOVE SPAR-ANTAL-RADER  TO 6302-KVRADER-FAKT                        
065200       MOVE SPAR-IDKUNDRF     TO 6302-IDKUNDRF                            
065300       MOVE SPAR-IDKUNDNR     TO 6302-IDKUNDNR                            
065400                                                                          
065500******************************************************************        
065600*                                                                         
065700*  KOLLA OM KVALITETSTRANSFER (GER SVARET RADER-FINNS)                    
065800*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
065900*                                                                         
066000******************************************************************        
066100                                                                          
066200       IF DIST35-RETUR AND NOT DIST35-CDC-NL-RETUR                        
066300         IF DIST35-RETUR-Q                                                
066400           MOVE 'QUALITY'      TO 6302-IDLBBET                            
066500         ELSE                                                             
066600           MOVE SPAR-IDLBBET   TO 6302-IDLBBET                            
066700         END-IF                                                           
066800       ELSE                                                               
066900         MOVE SPAR-IDDISTR      TO W-TP4TRAN-IDDISTR                      
067000                                                                          
067100         PERFORM DB2-SELECT-TP4TRAN                                       
067200                                                                          
067300         IF  RADER-FINNS                                                  
067400         AND TP4TRAN-KDARBTYP = 'QUAL'                                    
067500           MOVE 'QUALITY'      TO 6302-IDLBBET                            
067600         ELSE                                                             
067700           MOVE SPAR-IDLBBET   TO 6302-IDLBBET                            
067800         END-IF                                                           
067900       END-IF                                                             
068000                                                                          
068100       PERFORM IMS-ISRT-6302                                              
068200                                                                          
068300       ADD +1 TO CHKP-ANT                                                 
068400       MOVE ZERO        TO SPAR-ANTAL-KOLLI                               
068500       MOVE ZERO        TO SPAR-ANTAL-RADER                               
068600       MOVE SPACE       TO SPAR-IDLBBET                                   
068700     END-IF                                                               
068800                                                                          
068900     MOVE FAKT-001-IDFAKT    TO SPAR-IDFAKT                               
069000                                6302-IDFAKT                               
069100                                W-IDFAKT                                  
069200     MOVE FAKT-001-IDSHIPM   TO SPAR-IDSHIPM                              
069300                                6302-IDSHIPM                              
069400     MOVE FAKT-001-IDDC      TO 6302-IDDC-SEND                            
069500     MOVE FAKT-001-IDDC-LEV  TO 6302-IDDC-LEV                             
069600     MOVE FAKT-001-IDDISTR   TO 6302-IDDISTR                              
069700                                SPAR-IDDISTR                              
069800     MOVE SPACE              TO 6302-KDTRPSTA                             
069900                                6302-ADINLOMR                             
070000                                6302-IDBOKN                               
070100                                6302-BETRPFIR                             
070200                                6302-IDUSER-MANETA                        
070300     MOVE ZERO               TO 6302-KVKOLLI-MOT                          
070400                                6302-KVRADER-MOT                          
070500                                6302-KVRADER-PRIO                         
070600                                6302-DABERANK-DISCH                       
070700                                6302-DABERANK-PROP                        
070800                                6302-TILST-CALLP44                        
070900                                6302-TILST-PUSHEVNT                       
071000                                6302-IDSUBSCR                             
071100                                6302-IDCONTNR                             
071200                                6302-DABERANK-LIFDEPPL                    
071300                                6302-DABERANK-LIFDEPAC                    
071400                                6302-DABERANK-PODDEPPL                    
071500                                6302-DABERANK-PODDEPAC                    
071600                                6302-DABERANK-DLVDELPL                    
071700                                6302-DABERANK-DLVDELAC                    
071800                                6302-DABERANK-PODDISPL                    
071900                                6302-DABERANK-PODDISAC                    
072000                                6302-DABERANK-PODARRPL                    
072100                                6302-DABERANK-LIFARRAC                    
072200     MOVE NEJ                TO 6302-FLMANETA                             
072300     MOVE FAKT-001-TIFAKT    TO 6302-TIFAKT                               
072400                                                                          
072500     MOVE FAKT-001-IDLEVNR   TO 6302-IDLEVNR                              
072600     MOVE FAKT-001-KDFRAKT   TO SPAR-KDFRAKT                              
072700     MOVE FAKT-001-KDVALISO  TO SPAR-KDVALISO                             
072800     MOVE FAKT-001-PRKURS    TO SPAR-PRKURS                               
072900     MOVE SPAR-IDDISTR       TO TEST-IDDISTR                              
073000     PERFORM S10-TEST-AV-IDDISTR                                          
073100                                                                          
073200     IF DCS-SDC AND NOT DCS-CHINA                                         
073300       MOVE FAKT-001-TIFAKT  TO 6302-DABERANK                             
073400                                W-DABERANK                                
073500                                SPAR-DABERANK                             
073600       IF FAKT-001-TIFAKT NOT = ZERO                                      
073700         IF FAKT-001-TIFAKT < 500000                                      
073800           MOVE 20           TO 6302-DABERANK (1:2)                       
073900         ELSE                                                             
074000           IF FAKT-001-TIFAKT < 999999                                    
074100             MOVE 19         TO 6302-DABERANK (1:2)                       
074200           ELSE                                                           
074300             MOVE 99999999   TO 6302-DABERANK                             
074400           END-IF                                                         
074500         END-IF                                                           
074600       END-IF                                                             
074700     ELSE                                                                 
074800       IF DIST35-RETUR                                                    
074900       AND NOT (DIST35-CDC-NL-RETUR OR DIST35-JP-NDC-RETURNS)             
075000         MOVE ZERO             TO 6302-DABERANK                           
075100                                  W-DABERANK                              
075200                                  SPAR-DABERANK                           
075300       ELSE                                                               
075400         PERFORM S11-BERAKNA-ETA                                          
075500         IF ETA-SVAR-OK = SPACE OR JA                                     
075600           MOVE ETA-TIAAMMDD-SVAR  TO 6302-DABERANK                       
075700                                      W-DABERANK                          
075800                                      SPAR-DABERANK                       
075900           IF ETA-TIAAMMDD-SVAR NOT = ZERO                                
076000             IF ETA-TIAAMMDD-SVAR < 500000                                
076100               MOVE 20         TO 6302-DABERANK (1:2)                     
076200             ELSE                                                         
076300               IF ETA-TIAAMMDD-SVAR < 999999                              
076400                 MOVE 19       TO 6302-DABERANK (1:2)                     
076500               ELSE                                                       
076600                 MOVE 99999999 TO 6302-DABERANK                           
076700               END-IF                                                     
076800             END-IF                                                       
076900           END-IF                                                         
077000         ELSE                                                             
077100           MOVE ' FEL I SUBPGM W218ETA   ' TO FELTEXT                     
077200           CALL  ABEND USING RKOD-ABEND-MED-DUMP                          
077300         END-IF                                                           
077400       END-IF                                                             
077500     END-IF                                                               
077600                                                                          
077700     .                                                                    
077800     EJECT                                                                
077900 BB-KOLLI SECTION.                                                        
078000     MOVE 'BB-KOLLI        ' TO CURRENT-SECTION                           
078100     SKIP2                                                                
078200     MOVE FAKT-002-IDKUNDNR  TO SPAR-IDKUNDNR                             
078300     MOVE FAKT-002-IDLBBET   TO SPAR-IDLBBET                              
078400     MOVE FAKT-002-IDKUNDRF  TO SPAR-IDKUNDRF                             
078500     MOVE FAKT-002-IDKOLLI   TO SPAR-IDKOLLI                              
078600     MOVE FAKT-002-KDKOLLI   TO SPAR-KDKOLLI                              
078700     ADD +1                  TO SPAR-ANTAL-KOLLI                          
078800     .                                                                    
078900     EJECT                                                                
079000 BC-RAD SECTION.                                                          
079100     MOVE 'BC-RAD          ' TO CURRENT-SECTION                           
079200     SKIP2                                                                
079300     MOVE FAKT-003-IDARTNR   TO INLC-ART-IDARTNR                          
079400                                W-IDARTNR                                 
079500     ADD +1                  TO SPAR-ANTAL-RADER                          
079600                                                                          
079700     MOVE FAKT-003-IDDISTR   TO TEST-IDDISTR                              
079800     PERFORM S10-TEST-AV-IDDISTR                                          
079900                                                                          
080000     PERFORM BCB-UPPDATERA-WDK7                                           
080100                                                                          
080200     PERFORM IMS-GHU-WDL601                                               
080300                                                                          
080400     IF SEGMENT-SAKNAS                                                    
080500       PERFORM IMS-ISRT-WDL601                                            
080600       ADD +1 TO CHKP-ANT                                                 
080700     END-IF                                                               
080800                                                                          
080900     PERFORM BCA-SKAPA-INLEVERANS                                         
081000                                                                          
081100     IF DIST35-NONVCC-CDC-REFILL                                          
081200     OR (DIST35-RETUR                                                     
081300        AND NOT (DIST35-CDC-NL-RETUR OR DIST35-JP-NDC-RETURNS))           
081400        MOVE CLAG-ADLAGOMR      TO INLC-INL-ADLAGOMR                      
081500        MOVE CLAG-ADGANG        TO INLC-INL-ADGANG                        
081600        MOVE CLAG-ADPLATS       TO INLC-INL-ADPLATS                       
081700     ELSE                                                                 
081800        MOVE ARTS-SLAG-ADLAGOMR TO INLC-INL-ADLAGOMR                      
081900        MOVE ARTS-SLAG-ADGANG   TO INLC-INL-ADGANG                        
082000        MOVE ARTS-SLAG-ADPLATS  TO INLC-INL-ADPLATS                       
082100     END-IF                                                               
082200                                                                          
082300                                                                          
082400     MOVE MOT-WS-IDDC        TO INLC-INL-IDDC                             
082500     MOVE FAKT-003-IDDC-LEV  TO INLC-INL-IDDC-LEV                         
082600     MOVE FAKT-003-IDFAKT    TO INLC-INL-IDFAKT                           
082700                                                                          
082800     IF DIST35-NONVCC-NONVCC-REFILL  OR                                   
082900        DIST35-NONVCC-NONVCC-TRANSFER                                     
083000       MOVE FAKT-003-IDDC    TO W-IDDC-B6-SEND                            
083100       PERFORM IMS-GU-WDB601-SEND                                         
083200       MOVE SEND-DCS-IDLEVNR-DC                                           
083300                             TO INLC-INL-IDLEVNR                          
083400     ELSE                                                                 
083500       IF FAKT-003-IDDC NOT = SEND-DCS-IDDC                               
083600         MOVE FAKT-003-IDDC  TO W-IDDC-B6-SEND                            
083700         PERFORM IMS-GU-WDB601-SEND                                       
083800       END-IF                                                             
083900       IF SEND-DCS-DDC                                                    
084000         MOVE WC-CDC-SE      TO W-IDDC-B6                                 
084100       ELSE                                                               
084200         MOVE FAKT-003-IDDC  TO W-IDDC-B6                                 
084300       END-IF                                                             
084400       IF W-IDDC-B6 NOT = DCS-IDDC                                        
084500          PERFORM IMS-GU-WDB601                                           
084600       END-IF                                                             
084700       MOVE DCS-IDLEVNR-DC   TO INLC-INL-IDLEVNR                          
084800     END-IF                                                               
084900                                                                          
085000     MOVE FAKT-003-IDDISTR   TO INLC-INL-IDDISTR                          
085100     MOVE SPAR-IDKUNDNR      TO INLC-INL-IDKUNDNR                         
085200     MOVE SPAR-IDKUNDRF      TO INLC-INL-IDKUNDRF                         
085300     MOVE SPAR-IDKOLLI       TO INLC-INL-IDKOLLI                          
085400     MOVE 'R30'              TO INLC-INL-IDPTYP                           
085500     MOVE SPAR-KDFRAKT       TO INLC-INL-KDFRAKT                          
085600     MOVE SPAR-KDKOLLI       TO INLC-INL-KDKOLLI                          
085700     MOVE SPAR-KDVALISO      TO INLC-INL-KDVALISO                         
085800     MOVE SPAR-DABERANK(3:6) TO INLC-INL-TIBERANK                         
085900     MOVE FAKT-003-KVLEVART  TO INLC-INL-KVAVIS                           
086000     IF DIST35-NONVCC-NONVCC-REFILL   OR                                  
086010        DIST35-NONVCC-NONVCC-TRANSFER                                     
086100       MOVE FAKT-003-PRARTNTO                                             
086200                             TO INLC-INL-PRARTNTO                         
086300       MOVE SPAR-PRKURS      TO INLC-INL-PRKURS                           
086400     ELSE                                                                 
086500       IF SEND-DCS-NDC-NA OR DCS-LAND-NON-VCC-OWNED                       
086600*      IF SEND-DCS-NDC-NA OR DCS-CHINA OR                                 
086700*        DCS-INDIA OR DCS-KOREA                                           
086800         MOVE FAKT-003-PRAVCOST TO INLC-INL-PRARTNTO                      
086900         PERFORM BCC-BERAKNA-KURS                                         
087000       ELSE                                                               
087100         IF SEND-DCS-NDC-PF                                               
087200           MOVE FAKT-003-PRARTNTO                                         
087300                             TO INLC-INL-PRARTNTO                         
087400           PERFORM BCC-BERAKNA-KURS                                       
087500         ELSE                                                             
087600           MOVE FAKT-003-PRARTNTO                                         
087700                             TO INLC-INL-PRARTNTO                         
087800           MOVE SPAR-PRKURS  TO INLC-INL-PRKURS                           
087900         END-IF                                                           
088000       END-IF                                                             
088100     END-IF                                                               
088200                                                                          
088300     MOVE NEJ                TO INLC-INL-FLSKAKOL                         
088400                                INLC-INL-FLMAKUL                          
088500                                INLC-INL-FLPRIO                           
088600                                INLC-INL-FLTULLST                         
088700                                                                          
088800     IF DIST35-RETUR                                                      
088900        MOVE +8              TO INLC-INL-KDRT                             
089000     ELSE                                                                 
089100        MOVE ZERO            TO INLC-INL-KDRT                             
089200     END-IF                                                               
089300     MOVE ZERO               TO INLC-INL-KVANTMOT                         
089400                                INLC-INL-TIINLMOT                         
089500                                INLC-INL-TIINLINL                         
089600                                INLC-INL-KVART-SKROT                      
089700                                INLC-INL-IDLOPNRM                         
089800                                INLC-INL-TIINLMTI                         
089900                                INLC-INL-TIINLITI                         
090000                                INLC-INL-KVTULRET                         
090100                                INLC-INL-KVRETUR                          
090200                                INLC-INL-KDAVVANT                         
090300                                INLC-INL-TIAVIDAT                         
090400                                INLC-INL-IDKONTO                          
090500     MOVE SPACE              TO INLC-INL-IDUSER-003                       
090600                                INLC-INL-ADINLOMR                         
090700                                INLC-INL-IDKST                            
090800                                INLC-INL-IDANALYS                         
090900                                                                          
091000     PERFORM IMS-ISRT-WDL611                                              
091100                                                                          
091200     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
091300       IF SEGMENT-FINNS-REDAN                                             
091400         SUBTRACT 1 FROM INLC-INL-DAINLEV                                 
091500         SUBTRACT 1 FROM W-DAINLEV                                        
091600         PERFORM IMS-ISRT-WDL611                                          
091700       END-IF                                                             
091800     END-PERFORM                                                          
091900                                                                          
092000     ADD +1 TO CHKP-ANT                                                   
092100     .                                                                    
092200     EJECT                                                                
092300 BCA-SKAPA-INLEVERANS SECTION.                                            
092400     MOVE 'BCA-SKAPA-INLEV ' TO CURRENT-SECTION                           
092500     SKIP2                                                                
092600     ACCEPT WS-TIAAMMDD-DATE FROM DATE                                    
092700     ACCEPT WS-TTMMSSTH-TIME FROM TIME                                    
092800     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
092900                                                                          
093000     COMPUTE WS-DAINLEV = 9999999999999999                                
093100                        - WS-TIAAAAMMDDTTMMSSTH                           
093200                                                                          
093300     MOVE WS-DAINLEV TO INLC-INL-DAINLEV                                  
093400                        W-DAINLEV                                         
093500     .                                                                    
093600     EJECT                                                                
093700 BCB-UPPDATERA-WDK7 SECTION.                                              
093800     MOVE 'BCB-UPPD-WDK7   ' TO CURRENT-SECTION                           
093900                                                                          
094000**********  HÄR UPPDATERAS REFILL PÅ DC-LAGER                             
094100                                                                          
094200     IF DIST35-NONVCC-CDC-REFILL                                          
094300     OR (DIST35-RETUR                                                     
094400        AND NOT (DIST35-CDC-NL-RETUR OR DIST35-JP-NDC-RETURNS))           
094500        PERFORM IMS-GHU-WDK611                                            
094600        MOVE CLAG-KVAKS-PAV TO WS-KVAKS-PAV                               
094700                                                                          
094800        COMPUTE CLAG-KVAKS-PAV = CLAG-KVAKS-PAV +                         
094900                                 FAKT-003-KVLEVART                        
095000        COMPUTE CLAG-KVBEART   = CLAG-KVBEART -                           
095100                                 FAKT-003-KVLEVART                        
095200                                                                          
095300        PERFORM IMS-REPL-WDK611                                           
095400     ELSE                                                                 
095500        MOVE MOT-WS-IDDC       TO W-IDDC-WDK7                             
095600        PERFORM IMS-LAES-WDK711-SLAGSEG                                   
095700        MOVE ARTS-SLAG-KVAKS-PAV TO WS-KVAKS-PAV                          
095800                                                                          
095900        COMPUTE ARTS-SLAG-KVAKS-PAV = ARTS-SLAG-KVAKS-PAV +               
096000                                      FAKT-003-KVLEVART                   
096100        MOVE FAKT-003-IDDISTR  TO TEST-IDDISTR                            
096200        IF DIST35-REFILL-NA-JAP                                           
096300          CONTINUE                                                        
096400        ELSE                                                              
096500          COMPUTE ARTS-SLAG-KVBEART = ARTS-SLAG-KVBEART -                 
096600                                      FAKT-003-KVLEVART                   
096700        END-IF                                                            
096800                                                                          
096900        PERFORM IMS-REPL-WDK711-SLAGSEG                                   
097000     END-IF                                                               
097100     PERFORM BCBA-BERAKNA-SALDOLOGG-DATA                                  
097200                                                                          
097300     ADD +1 TO CHKP-ANT                                                   
097400     .                                                                    
097500     EJECT                                                                
097600 BCBA-BERAKNA-SALDOLOGG-DATA SECTION.                                     
097700     MOVE 'BCBA-BERAKNA    ' TO CURRENT-SECTION                           
097800     PERFORM S12-FLYTTA-SALDOLOGG-DATA                                    
097900*    ---KOLLAR SALDOFÖRÄNDRINGAR PÅ WDK711 OCH                            
098000*    ---LOGGAR DESSA PÅ WDL9                                              
098100     IF DIST35-NONVCC-CDC-REFILL                                          
098200     OR (DIST35-RETUR                                                     
098300        AND NOT (DIST35-CDC-NL-RETUR OR DIST35-JP-NDC-RETURNS))           
098400       IF WS-KVAKS-PAV > CLAG-KVAKS-PAV                                   
098500         MOVE '-' TO LOGG-IDTECKEN-KVAKS-PAV                              
098600       ELSE                                                               
098700          IF WS-KVAKS-PAV < CLAG-KVAKS-PAV                                
098800             MOVE '+' TO LOGG-IDTECKEN-KVAKS-PAV                          
098900          ELSE                                                            
099000             MOVE ' ' TO LOGG-IDTECKEN-KVAKS-PAV                          
099100          END-IF                                                          
099200       END-IF                                                             
099300     ELSE                                                                 
099400       IF WS-KVAKS-PAV > ARTS-SLAG-KVAKS-PAV                              
099500          MOVE '-' TO LOGG-IDTECKEN-KVAKS-PAV                             
099600       ELSE                                                               
099700          IF WS-KVAKS-PAV < ARTS-SLAG-KVAKS-PAV                           
099800             MOVE '+' TO LOGG-IDTECKEN-KVAKS-PAV                          
099900          ELSE                                                            
100000             MOVE ' ' TO LOGG-IDTECKEN-KVAKS-PAV                          
100100          END-IF                                                          
100200       END-IF                                                             
100300     END-IF                                                               
100400     PERFORM S13-ISRT-SALDOLOGG                                           
100500     .                                                                    
100600     EJECT                                                                
100700 BCC-BERAKNA-KURS SECTION.                                                
100800     MOVE 'BCC-BERAKNA-KURS' TO CURRENT-SECTION                           
100900                                                                          
101000**********  HÄR BESTÄMS VILKEN KURS SOM SKALL GÄLLA FÖR                   
101100**********  TRANSFERS INOM NORDAMERIKA                                    
101200                                                                          
101300     IF MOT-WS-IDDC NOT = MOT-DCS-IDDC                                    
101400        MOVE MOT-WS-IDDC TO W-IDDC-B6-MOT                                 
101500        PERFORM IMS-GU-WDB601-MOT                                         
101600     END-IF                                                               
101700                                                                          
101800     IF SEND-DCS-NDC-NA AND SEND-DCS-USA AND                              
101900       (MOT-DCS-NDC-NA AND DCS-USA)                                       
102000       MOVE 1.00000                   TO INLC-INL-PRKURS                  
102100                                                                          
102200     ELSE                                                                 
102300       IF SEND-DCS-LAND-NON-VCC-OWNED                                     
102400*      IF SEND-DCS-CHINA OR                                               
102500*         SEND-DCS-INDIA OR                                               
102600*         SEND-DCS-KOREA                                                  
102700         MOVE 1.00000                 TO INLC-INL-PRKURS                  
102800                                                                          
102900       ELSE                                                               
103000         IF SEND-DCS-NDC-NA AND SEND-DCS-USA                              
103100           MOVE 'USD'                 TO CURR-KDVALISO-ROW                
103200         ELSE                                                             
103300           IF SEND-DCS-NDC-NA AND SEND-DCS-CANADA                         
103400             MOVE 'CAD'               TO CURR-KDVALISO-ROW                
103500           ELSE                                                           
103600             IF SEND-DCS-NDC-PF AND SEND-DCS-JAPAN                        
103700               MOVE 'JPY'             TO CURR-KDVALISO-ROW                
103800             ELSE                                                         
103900               IF SEND-DCS-NDC-PF AND SEND-DCS-AUSTRALIA                  
104000                 MOVE 'AUD'           TO CURR-KDVALISO-ROW                
104100               END-IF                                                     
104200             END-IF                                                       
104300           END-IF                                                         
104400         END-IF                                                           
104500         MOVE W-DATE-AAMM           TO CURR-TIAAMM                        
104600         MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                  
104700         MOVE 'M'                   TO CURR-KDVALTYP                      
104800                                                                          
104900         CALL W510CURR USING CURR-W510CURR WDG2-PCB                       
105000         IF CURR-KDSVAR = ' '                                             
105100            CONTINUE                                                      
105200         ELSE                                                             
105300            MOVE 1      TO CURR-PRKURS-NEW                                
105400         END-IF                                                           
105500         COMPUTE INLC-INL-PRKURS ROUNDED =                                
105600                  SPAR-PRKURS / CURR-PRKURS-NEW                           
105700       END-IF                                                             
105800     END-IF                                                               
105900     .                                                                    
106000     EJECT                                                                
106100 Z-FINIT SECTION.                                                         
106200     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
106300                                                                          
106400                                                                          
106500     CLOSE W47664                                                         
106600     SKIP2                                                                
106700* NOLLA ÅTERSTARTIFORMATIONEN                                             
106800     PERFORM IMS-LAS-ATERSTART                                            
106900     MOVE +0 TO 4751-KVPOST                                               
107000                4751-IDDISTR                                              
107100                4751-IDKOLLI                                              
107200                4751-IDFAKT                                               
107300     MOVE SPACE TO 4751-IDKUNDRF                                          
107400                                                                          
107500     PERFORM IMS-REPL-ATERSTART                                           
107600                                                                          
107700     MOVE 'S' TO POSTSUM-OPKOD                                            
107800     CALL POSTSUM USING POSTSUM-PARM                                      
107900     .                                                                    
108000     EJECT                                                                
108100 S01-LAES-W47664  SECTION.                                                
108200     SKIP2                                                                
108300     READ W47664 INTO IN-AREA                                             
108400     AT END                                                               
108500        MOVE JA TO W47664-EOF-SW                                          
108600                                                                          
108700     NOT AT END                                                           
108800        MOVE 'W47664' TO POSTSUM-FDNAMN                                   
108900        MOVE 'W47691D1' TO POSTSUM-DDNAMN2                                
109000        MOVE FAKT-001-IDPTYP TO POSTSUM-TRANSTYP                          
109100        CALL POSTSUM USING POSTSUM-PARM                                   
109200                                                                          
109300     END-READ                                                             
109400     .                                                                    
109500     EJECT                                                                
109600 S10-TEST-AV-IDDISTR SECTION.                                             
109700*    MOVE ' S10-TEST       ' TO CURRENT-SECTION                           
109800     SKIP2                                                                
109900                                                                          
110000******************************************************************        
110100*                                                                         
110200*  KOLLA OM TRANSFER (GER SVARET RADER-FINNS)                             
110300*  GÄLLER I PRAKTIKEN BARA EU-TRANSFER NOV-05                             
110400*                                                                         
110500******************************************************************        
110600                                                                          
110700     IF DIST35-RETUR                                                      
110800        AND NOT (DIST35-CDC-NL-RETUR OR DIST35-JP-NDC-RETURNS)            
110900        MOVE WC-CDC-SE TO MOT-WS-IDDC                                     
111000                          W-IDDC-B6                                       
111100     ELSE                                                                 
111200       MOVE TEST-IDDISTR        TO W-TP4TRAN-IDDISTR                      
111300                                                                          
111400       PERFORM DB2-SELECT-TP4TRAN                                         
111500                                                                          
111600       IF RADER-FINNS                                                     
111700         MOVE TP4TRAN-IDDC-REC    TO MOT-WS-IDDC                          
111800                                   W-IDDC-B6                              
111900       ELSE                                                               
112000         SEARCH ALL DIST57-REFILL-DC                                      
112100            AT END                                                        
112200               MOVE 'EJ TRÄFF I REFILLTAB WWDIST57'                       
112300                              TO FELTEXT                                  
112400*FIX CO                                                                   
112500*        IF TEST-IDDISTR = 8203                                           
112600*          MOVE '67'              TO MOT-WS-IDDC                          
112700*                                    W-IDDC-B6                            
112800*        ELSE                                                             
112900*          CALL FELLOG                                                    
113000*        END-IF                                                           
113100*FIX CO SLUT                                                              
113200            WHEN DIST57-SOK-IDDISTR(DIST57-IX) = TEST-IDDISTR             
113300               MOVE DIST57-REFILL-TO-DC(DIST57-IX) TO MOT-WS-IDDC         
113400                                                      W-IDDC-B6           
113500         END-SEARCH                                                       
113600       END-IF                                                             
113700     END-IF                                                               
113800                                                                          
113900     IF W-IDDC-B6 NOT = DCS-IDDC                                          
114000       PERFORM IMS-GU-WDB601                                              
114100     END-IF                                                               
114200                                                                          
114300     .                                                                    
114400     EJECT                                                                
114500 S11-BERAKNA-ETA SECTION.                                                 
114600     MOVE 'S11-BERAKNA-ETA ' TO CURRENT-SECTION                           
114700                                                                          
114800**** SKICKA MED FAKT-001-TIFAKT TILL SUBMODUL W218ETA OCH                 
114900**** FÅ TILLBAKA DABERANK                                                 
115000                                                                          
115100     IF FAKT-001-KDORDKL-MAX < 2                                          
115200       MOVE 603              TO ETA-KDCALL                                
115300     ELSE                                                                 
115400       MOVE 604              TO ETA-KDCALL                                
115500     END-IF                                                               
115600     MOVE FAKT-001-IDDC      TO ETA-IDDC-SEND                             
115700*                                                                         
115800**** FÖR 'STUDS' REFILLDISTR GÄLLER DET GODSSÄNDANDE DC-T                 
115900**** OCH INTE DET FAKTURERANDE (FINANSIELLA) DC-T                         
116000     IF DIST35-NONVCC-NONVCC-REFILL   OR                                  
116010        DIST35-NONVCC-NONVCC-TRANSFER                                     
116100       MOVE FAKT-001-IDDC-LEV                                             
116200                             TO ETA-IDDC-SEND                             
116300     END-IF                                                               
116400*                                                                         
116500     MOVE MOT-WS-IDDC        TO ETA-IDDC-REC                              
116600     MOVE ZERO               TO ETA-IDARTNR                               
116700                                ETA-KDFRAKT                               
116800     MOVE SPACE              TO ETA-IDLEVNR                               
116900     MOVE FAKT-001-TIFAKT    TO ETA-TIAAMMDD-ANROP                        
117000                                WS-AAMMDD                                 
117100     IF WS-AA < 50                                                        
117200       MOVE 20               TO ETA-TISEKEL-ANROP                         
117300     ELSE                                                                 
117400       MOVE 19               TO ETA-TISEKEL-ANROP                         
117500     END-IF                                                               
117600                                                                          
117700     CALL W218ETA USING ETA-W218LETA ETA-ARTC-PCB ETA-ARTS-PCB            
117800                                     ETA-INLC-PCB ETA-LEVA-PCB            
117900                                     ETA-WDB6-PCB                         
118000     IF TEST-IDDISTR = 8203                                               
118100       MOVE SPACE TO   ETA-SVAR-OK                                        
118200       MOVE ZERO  TO ETA-TIAAMMDD-SVAR                                    
118300     END-IF                                                               
118400     .                                                                    
118500     EJECT                                                                
118600 S12-FLYTTA-SALDOLOGG-DATA SECTION.                                       
118700     MOVE 'S12-FLYTTA-SALDO' TO CURRENT-SECTION                           
118800     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
118900     ACCEPT WS-TID           FROM TIME                                    
119000     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - WS-DAGENS-DATUM            
119100     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - WS-TID                    
119200     MOVE 9                   TO LOGG-IDSEKVNR                            
119300     MOVE 'INBO'              TO LOGG-IDHUVTYP                            
119400     MOVE '310'               TO LOGG-IDSUBTYP                            
119500     MOVE 'W4769100'          TO LOGG-IDPGM                               
119600     MOVE SPACE               TO LOGG-IDTRANS                             
119700     MOVE 'W4769100'          TO LOGG-IDUSER                              
119800     MOVE SPACE               TO LOGG-REF                                 
119900     MOVE FAKT-003-IDDISTR    TO LOGG-IDDISTR                             
120000     MOVE SPAR-IDKUNDNR       TO LOGG-IDKUNDNR                            
120100     MOVE FAKT-003-IDKUNDRF   TO LOGG-IDKUNDRF                            
120200     MOVE FAKT-003-IDFAKT     TO LOGG-IDFAKT                              
120300     MOVE FAKT-003-IDARTNR    TO LOGG-IDARTNR                             
120400     IF DIST35-NONVCC-CDC-REFILL                                          
120500     OR (DIST35-RETUR                                                     
120600        AND NOT (DIST35-CDC-NL-RETUR OR DIST35-JP-NDC-RETURNS))           
120700       MOVE MOT-WS-IDDC       TO LOGG-IDDC                                
120800       MOVE CLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                           
120900       MOVE CLAG-KVEFRS       TO LOGG-KVEFRS                              
121000       MOVE CLAG-KVLS         TO LOGG-KVLS                                
121100       MOVE CLAG-KVAKS-CDC    TO LOGG-KVAKS                               
121200     ELSE                                                                 
121300       MOVE W-IDDC-WDK7         TO LOGG-IDDC                              
121400       MOVE ARTS-SLAG-KVAKS-PAV TO LOGG-KVAKS-PAV                         
121500       MOVE ARTS-SLAG-KVEFRS    TO LOGG-KVEFRS                            
121600       MOVE ARTS-SLAG-KVLS      TO LOGG-KVLS                              
121700       MOVE ARTS-SLAG-KVAKS-SDC TO LOGG-KVAKS                             
121800     END-IF                                                               
121900     MOVE SPACE               TO LOGG-IDTECKEN-KVAKS                      
122000     MOVE SPACE               TO LOGG-IDTECKEN-KVEFRS                     
122100     MOVE SPACE               TO LOGG-IDTECKEN-KVLS                       
122200     MOVE FAKT-003-KVLEVART   TO LOGG-KVART-SALDO                         
122300     MOVE '00000000'          TO LOGG-DAREGDAT-LADD                       
122400     .                                                                    
122500     EJECT                                                                
122600 S13-ISRT-SALDOLOGG SECTION.                                              
122700     MOVE 'S13-ISRT-SALDO  ' TO CURRENT-SECTION                           
122800     PERFORM IMS-ISRT-WDL901                                              
122900     IF SEGMENT-FINNS-REDAN                                               
123000       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
123100         SUBTRACT 1         FROM LOGG-IDSEKVNR                            
123200         PERFORM IMS-ISRT-WDL901                                          
123300       END-PERFORM                                                        
123400     END-IF                                                               
123500     .                                                                    
123600     EJECT                                                                
123700 S99-FEL-POSTTYP SECTION.                                                 
123800     SKIP2                                                                
123900     DISPLAY 'FEL POSTTYP'                                                
124000     DISPLAY 'POST      ' IN-AREA                                         
124100     .                                                                    
124200     EJECT                                                                
124300 X-TAG-CHECKPOINT   SECTION.                                              
124400                                                                          
124500* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
124600* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
124700     PERFORM IMS-LAS-ATERSTART                                            
124800                                                                          
124900     MOVE POST-ANT       TO 4751-KVPOST                                   
125000     MOVE SPAR-IDFAKT    TO 4751-IDFAKT                                   
125100     MOVE SPAR-IDDISTR   TO 4751-IDDISTR                                  
125200     MOVE SPAR-IDKOLLI   TO 4751-IDKOLLI                                  
125300     MOVE SPAR-IDKUNDRF  TO 4751-IDKUNDRF                                 
125400                                                                          
125500     PERFORM IMS-REPL-ATERSTART                                           
125600                                                                          
125700     PERFORM IMS-CHECKPOINT                                               
125800     MOVE ZERO TO CHKP-ANT                                                
125900* --- LÄS OM DATABAS OM DET BEHÖVS                                        
126000     .                                                                    
126100     EJECT                                                                
126200* --- IMS SEKTIONER ---                                                   
126300     SKIP3                                                                
126400 IMS-GHU-WDL601 SECTION.                                                  
126500     MOVE 'IMS-GHU-WDL601  ' TO CURRENT-IMS-SECTION                       
126600                                                                          
126700     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
126800          DELIMITED BY SIZE INTO SSA1                                     
126900     MOVE '  GE'       TO GODK-STATUSKODER                                
127000     CALL CBLTDLI USING GHU WDL6-PCB DLI-IO-WDL601 SSA1                   
127100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
127200     PERFORM IMS-STATUSKONTROLL                                           
127300     .                                                                    
127400     SKIP3                                                                
127500 IMS-ISRT-WDL601 SECTION.                                                 
127600     MOVE 'IMS-ISRT-WDL601 ' TO CURRENT-IMS-SECTION                       
127700                                                                          
127800     MOVE 'WDL601   '  TO SSA1                                            
127900     MOVE '  II'       TO GODK-STATUSKODER                                
128000     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-WDL601 SSA1                  
128100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
128200     PERFORM IMS-STATUSKONTROLL                                           
128300     .                                                                    
128400     EJECT                                                                
128500 IMS-ISRT-WDL611 SECTION.                                                 
128600     MOVE 'IMS-ISRT-WDL611 ' TO CURRENT-IMS-SECTION                       
128700                                                                          
128800     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
128900          DELIMITED BY SIZE INTO SSA1                                     
129000     MOVE 'WDL611   '  TO SSA2                                            
129100     MOVE '  II'       TO GODK-STATUSKODER                                
129200     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-WDL611 SSA1 SSA2             
129300     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
129400     PERFORM IMS-STATUSKONTROLL                                           
129500     .                                                                    
129600     EJECT                                                                
129700 IMS-GHU-6301  SECTION.                                                   
129800     MOVE 'IMS-GHU-6301    ' TO CURRENT-IMS-SECTION                       
129900                                                                          
130000     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6301 ')'                      
130100          DELIMITED BY SIZE INTO SSA1                                     
130200     MOVE '  GE'       TO GODK-STATUSKODER                                
130300     CALL CBLTDLI USING GHU 6301-PCB DLI-IO-AREA-6301 SSA1                
130400     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
130500     PERFORM IMS-STATUSKONTROLL                                           
130600     .                                                                    
130700     SKIP3                                                                
130800 IMS-ISRT-6301 SECTION.                                                   
130900     MOVE 'IMS-ISRT-6301   ' TO CURRENT-IMS-SECTION                       
131000                                                                          
131100     MOVE 'WDR501   '  TO SSA1                                            
131200     MOVE '  '         TO GODK-STATUSKODER                                
131300     CALL CBLTDLI USING ISRT 6301-PCB DLI-IO-AREA-6301 SSA1               
131400     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
131500     PERFORM IMS-STATUSKONTROLL                                           
131600     .                                                                    
131700     EJECT                                                                
131800 IMS-ISRT-6302 SECTION.                                                   
131900     MOVE 'IMS-ISRT-6302   ' TO CURRENT-IMS-SECTION                       
132000                                                                          
132100     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-6301 ')'                      
132200          DELIMITED BY SIZE INTO SSA1                                     
132300     MOVE 'WDGX6302 '  TO SSA2                                            
132400     MOVE '  '         TO GODK-STATUSKODER                                
132500     CALL CBLTDLI USING ISRT 6301-PCB DLI-IO-AREA-6302 SSA1 SSA2          
132600     MOVE 6301-STATUS-CODE TO STATUS-WS                                   
132700     PERFORM IMS-STATUSKONTROLL                                           
132800     .                                                                    
132900     EJECT                                                                
133000 IMS-LAES-WDK711-SLAGSEG SECTION.                                         
133100     MOVE 'IMS-LAES-WDK711 ' TO CURRENT-IMS-SECTION                       
133200                                                                          
133300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
133400          DELIMITED BY SIZE INTO SSA1                                     
133500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
133600          DELIMITED BY SIZE INTO SSA2                                     
133700     MOVE '  '         TO GODK-STATUSKODER                                
133800     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
133900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
134000     PERFORM IMS-STATUSKONTROLL                                           
134100     .                                                                    
134200     EJECT                                                                
134300 IMS-REPL-WDK711-SLAGSEG SECTION.                                         
134400     MOVE 'IMS-REPL-WDK711 ' TO CURRENT-IMS-SECTION                       
134500                                                                          
134600     MOVE '  '              TO GODK-STATUSKODER                           
134700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
134800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
134900     PERFORM IMS-STATUSKONTROLL                                           
135000     .                                                                    
135100     EJECT                                                                
135200 IMS-RESTART SECTION.                                                     
135300     MOVE 'IMS- RESTART    ' TO CURRENT-IMS-SECTION                       
135400     SKIP2                                                                
135500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
135600     MOVE '  ' TO GODK-STATUSKODER                                        
135700     CALL CBLTDLI USING XRST MSG-PCB                                      
135800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
135900                        CHKP-AREA-LENGTH CHKP-AREA                        
136000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
136100     PERFORM IMS-STATUSKONTROLL                                           
136200     .                                                                    
136300     EJECT                                                                
136400 IMS-CHECKPOINT SECTION.                                                  
136500     MOVE 'IMS-CHECKPOINT  ' TO CURRENT-IMS-SECTION                       
136600     SKIP2                                                                
136700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
136800     MOVE '  XD' TO GODK-STATUSKODER                                      
136900     CALL CBLTDLI USING CHKP MSG-PCB                                      
137000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
137100                        CHKP-AREA-LENGTH CHKP-AREA                        
137200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
137300     PERFORM IMS-STATUSKONTROLL                                           
137400                                                                          
137500     IF IMS-EJ-OK                                                         
137600       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
137700       DISPLAY FELTEXT                                                    
137800       CALL FELLOG                                                        
137900     END-IF                                                               
138000     .                                                                    
138100 IMS-LAS-ATERSTART SECTION.                                               
138200     MOVE 'IMS-LAS-ATERST  ' TO CURRENT-IMS-SECTION                       
138300     SKIP2                                                                
138400     MOVE '4751'         TO W-IDHTYP                                      
138500     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
138600     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
138700                    DELIMITED BY SIZE INTO SSA1                           
138800     MOVE 'WDR540   '    TO SSA2                                          
138900     MOVE '  GE'           TO GODK-STATUSKODER                            
139000     CALL CBLTDLI USING GHU 4751-PCB DLI-IO-AREA-4751 SSA1 SSA2           
139100     MOVE 4751-STATUS-CODE TO STATUS-WS                                   
139200     PERFORM IMS-STATUSKONTROLL                                           
139300     .                                                                    
139400                                                                          
139500 IMS-REPL-ATERSTART SECTION.                                              
139600     MOVE 'IMS-REPL-ATERST ' TO CURRENT-IMS-SECTION                       
139700     SKIP2                                                                
139800     MOVE '  '             TO GODK-STATUSKODER                            
139900     CALL CBLTDLI USING REPL 4751-PCB DLI-IO-AREA-4751                    
140000     MOVE 4751-STATUS-CODE TO STATUS-WS                                   
140100     PERFORM IMS-STATUSKONTROLL                                           
140200     .                                                                    
140300                                                                          
140400 IMS-ISRT-ATERSTART SECTION.                                              
140500     MOVE 'IMS-ISRT-ATERST ' TO CURRENT-IMS-SECTION                       
140600     SKIP2                                                                
140700     MOVE '  '             TO GODK-STATUSKODER                            
140800     CALL CBLTDLI USING ISRT 4751-PCB DLI-IO-AREA-4751                    
140900     MOVE 4751-STATUS-CODE TO STATUS-WS                                   
141000     PERFORM IMS-STATUSKONTROLL                                           
141100     .                                                                    
141200     EJECT                                                                
141300 IMS-ISRT-WDL901 SECTION.                                                 
141400     MOVE 'IMS-ISRT--WDL901' TO CURRENT-IMS-SECTION                       
141500                                                                          
141600     MOVE 'WDL901   ' TO SSA1                                             
141700     MOVE '  II'      TO GODK-STATUSKODER                                 
141800     CALL CBLTDLI USING ISRT WDL9-PCB WLLOGA01 SSA1                       
141900     MOVE WDL9-STATUS-CODE TO STATUS-WS                                   
142000     PERFORM IMS-STATUSKONTROLL                                           
142100     .                                                                    
142200     EJECT                                                                
142300 IMS-GU-WDB601    SECTION.                                                
142400     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
142500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
142600          DELIMITED BY SIZE INTO SSA1                                     
142700     MOVE '  '        TO GODK-STATUSKODER                                 
142800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
142900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
143000     PERFORM IMS-STATUSKONTROLL                                           
143100     .                                                                    
143200     EJECT                                                                
143300  IMS-GU-WDB601-MOT    SECTION.                                           
143400     MOVE 'IMS-GU-WDB601-M ' TO CURRENT-IMS-SECTION                       
143500      STRING 'WDB601  (IDDC     =' W-IDDC-B6-MOT-X ')'                    
143600           DELIMITED BY SIZE INTO SSA1                                    
143700      MOVE '  GE'     TO GODK-STATUSKODER                                 
143800      CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-MOT SSA1            
143900      MOVE WDB6-STATUS-CODE    TO STATUS-WS                               
144000      PERFORM IMS-STATUSKONTROLL                                          
144100      IF SEGMENT-SAKNAS                                                   
144200         MOVE SPACE TO MOT-DCS-KDDC                                       
144300      END-IF                                                              
144400      .                                                                   
144500      EJECT                                                               
144600 IMS-GU-WDB601-SEND   SECTION.                                            
144700     MOVE 'IMS-GU-WDB601-S ' TO CURRENT-IMS-SECTION                       
144800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-SEND-X ')'                    
144900          DELIMITED BY SIZE INTO SSA1                                     
145000     MOVE '  GE'      TO GODK-STATUSKODER                                 
145100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
145200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
145300     PERFORM IMS-STATUSKONTROLL                                           
145400     IF SEGMENT-SAKNAS                                                    
145500        MOVE SPACE TO SEND-DCS-KDDC                                       
145600     END-IF                                                               
145700     .                                                                    
145800     EJECT                                                                
145900 IMS-GHU-WDK611 SECTION.                                                  
146000     MOVE 'IMS-GHU-WDK611  ' TO CURRENT-IMS-SECTION                       
146100                                                                          
146200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
146300          DELIMITED BY SIZE INTO SSA1                                     
146400     MOVE 'WDK611   '         TO SSA2                                     
146500     MOVE '    '              TO GODK-STATUSKODER                         
146600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
146700     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
146800     PERFORM IMS-STATUSKONTROLL                                           
146900     .                                                                    
147000 IMS-REPL-WDK611 SECTION.                                                 
147100     MOVE 'IMS-REPL-WDK611 ' TO CURRENT-IMS-SECTION                       
147200                                                                          
147300     MOVE '    '               TO GODK-STATUSKODER                        
147400     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
147500     MOVE WDK6-STATUS-CODE     TO STATUS-WS                               
147600     PERFORM IMS-STATUSKONTROLL                                           
147700     .                                                                    
147800     EJECT                                                                
147900 DB2-SELECT-TP4TRAN     SECTION.                                          
148000     MOVE 'DB2-SELECT-TP4TRAN   ' TO  WS-DB2-SEKTION                      
148100                                                                          
148200     MOVE 000100 TO GODK-SQLCODEKODER                                     
148300                                                                          
148400     EXEC SQL                                                             
148500           SELECT  DISTINCT                                               
148600                   IDDC_REC                                               
148700                                                                          
148800           INTO   :TP4TRAN-IDDC-REC                                       
148900                                                                          
149000           FROM    TP4TRAN                                                
149100                                                                          
149200           WHERE IDDISTR   = :W-TP4TRAN-IDDISTR                           
149300     END-EXEC                                                             
149400                                                                          
149500     MOVE SQLCODE TO SQLCODE-WS                                           
149600     PERFORM DB2-STATUSKONTROLL                                           
149700     .                                                                    
149800     EJECT                                                                
149900 IMS-STATUSKONTROLL SECTION.                                              
150000     SKIP2                                                                
150100     SET STATUS-IX TO 1                                                   
150200     SEARCH GODK-STATUS                                                   
150300       AT END                                                             
150400         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
150500         DISPLAY FELTEXT                                                  
150600         CALL FELLOG                                                      
150700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
150800         CONTINUE                                                         
150900     END-SEARCH                                                           
151000     .                                                                    
151100     EJECT                                                                
151200 DB2-STATUSKONTROLL  SECTION.                                             
151300                                                                          
151400     SET SQLCODE-IX TO 1                                                  
151500     SEARCH GODK-SQLCODE                                                  
151600       AT END                                                             
151700          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
151800          DELIMITED BY SIZE INTO FELTEXT                                  
151900          CALL ABEND USING RKOD-ABEND-DB2                                 
152000       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
152100     END-SEARCH                                                           
152200     .                                                                    

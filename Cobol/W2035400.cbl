000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2035400.                                                
000400 AUTHOR.         KENT JEBSEN.                                             
000500 DATE-WRITTEN.   97/01/27.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        VISAR LAGERSALDO, ANKOMSTSALDO SAMT BERÄKNADE                    
001000*        INLEVERANSER DE KOMMANDE 6 VECKORNA / IDDC                       
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDK7                                       
001300*        PROGRAMMET LÄSER      WDK6                                       
001400*        PROGRAMMET LÄSER      WDB6                                       
001500*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001600*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W2T354                                              
002000*        MID:         W2I35401                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W2O35401                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(8)   VALUE 'W2035400'.             
003300 77  INDX                        PIC S9(3)  VALUE ZERO.                   
003400 77  MAX-INDX                    PIC S9(3)  VALUE 14.                     
003500 77  IX-B6                       PIC S9(3)  VALUE ZERO.                   
003600 77  MAX-IX-B6                   PIC S9(3)  VALUE 13.                     
003700 77  IX-TOT                      PIC S9(3)  VALUE ZERO.                   
003800 77  SPRAK-IX                    PIC 9(3)   VALUE ZERO.                   
003900*                                                                         
004000 01  WS-TIAAVV-IDAG-X            PIC 9(4).                                
004100 01  WS-TIAAVV-IDAG REDEFINES WS-TIAAVV-IDAG-X.                           
004200     03 WS-TIAA-IDAG             PIC 9(2).                                
004300     03 WS-TIVV-IDAG             PIC 9(2).                                
004400 01  SUM-VV                      PIC 9(3).                                
004500 01  SUM-AA                      PIC 9(2).                                
004600                                                                          
004700 01  WS-TIAAVV-X                 PIC 9(4).                                
004800 01  WS-TIAAVV REDEFINES WS-TIAAVV-X.                                     
004900     03 WS-TIAA                  PIC 9(2).                                
005000     03 WS-TIVV                  PIC 9(2).                                
005100*                                                                         
005200 01  WS-DC-TEXT-GRP.                                                      
005300     03 WS-TEXT-FIXED            PIC X(2)   VALUE 'DC'.                   
005400     03 FILLER                   PIC X(1)   VALUE SPACE.                  
005500     03 WS-IDDC-TEXT             PIC X(2).                                
005600 01  WS-TOTAL-TEXT               PIC X(3)   VALUE 'Tot'.                  
005700*                                                                         
005800 01  W-TAB-WDB6.                                                          
005900     05  W-TAB-IDDC  OCCURS 13   PIC X(2)   VALUE SPACES.                 
006000*                                                                         
006100 77  WS-TIVV-PLUS-MINUS          PIC S9(3)  VALUE ZERO.                   
006200 77  WS-TISEKEL-IDAG             PIC 9(2)   VALUE ZERO.                   
006300 77  WS-TISEKEL                  PIC 9(2)   VALUE ZERO.                   
006400 77  MAX-IX                      PIC 9      VALUE 7.                      
006500 77  IX                          PIC 9      VALUE ZERO.                   
006600*                                                                         
006700 01  WS-BALANCE-TAB.                                                      
006800     03 WS-BAL-ALL-DC   OCCURS 13.                                        
006900        05 WS-KVLS-DC            PIC 9(7)   VALUE ZERO.                   
007000        05 WS-KVAK-DC            PIC 9(7)   VALUE ZERO.                   
007100        05 WS-BAL-DC    OCCURS 07.                                        
007200           10 WS-KVAVIS-DC       PIC 9(7)   VALUE ZERO.                   
007300*                                                                         
007400 77  WS-SPAR-KDDC                PIC X(2)   VALUE SPACES.                 
007500 77  WS-SPAR-B6-IDDC             PIC X(2)   VALUE SPACES.                 
007600 01  WS-TOTAL.                                                            
007700     05  WS-KVLS-TOT             PIC 9(7)   VALUE ZERO.                   
007800     05  WS-KVAK-TOT             PIC 9(7)   VALUE ZERO.                   
007900     05  WS-KVAVIS-WC-TOT        PIC 9(7)   VALUE ZERO.                   
008000     05  WS-KVAVIS-W1-TOT        PIC 9(7)   VALUE ZERO.                   
008100     05  WS-KVAVIS-W2-TOT        PIC 9(7)   VALUE ZERO.                   
008200     05  WS-KVAVIS-W3-TOT        PIC 9(7)   VALUE ZERO.                   
008300     05  WS-KVAVIS-W4-TOT        PIC 9(7)   VALUE ZERO.                   
008400     05  WS-KVAVIS-W5-TOT        PIC 9(7)   VALUE ZERO.                   
008500     05  WS-KVAVIS-W6-TOT        PIC 9(7)   VALUE ZERO.                   
008600                                                                          
008700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
008800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
008900                                                                          
009000 77  JA                          PIC X       VALUE 'J'.                   
009100 77  NEJ                         PIC X       VALUE 'N'.                   
009200                                                                          
009300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009400                                                                          
009500                                                                          
009600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009700     88  NYCKLAR-OK                          VALUE 'J'.                   
009800     88  NYCKLAR-FEL                         VALUE 'N'.                   
009900                                                                          
010000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
010100     88  INDATA-OK                           VALUE 'J'.                   
010200     88  INDATA-FEL                          VALUE 'N'.                   
010300                                                                          
010400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010500     88  EGEN-MID                            VALUE '2354'.                
010600     88  GODK-MID                            VALUE '2352'                 
010700                                                   '2353' '2354'          
010800                                                   '2355' '2356'          
010900                                                   '2357' '2358'          
011000                                                   '2359'.                
011100     88  HELP-MID                            VALUE '0551'.                
011200     EJECT                                                                
011300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011400*      --- VALID IDDC CODES                                               
011500*                                                                         
011600*01    -COPY WWDC99                                                       
011700*01    -COPY WWDCKONS                                                     
011800       EJECT                                                              
011900 01  GENERELLA-SUBPROGRAM.                                                
012000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
012600     EJECT                                                                
012700*01  -COPY WDATAREA                                                       
012800     EJECT                                                                
012900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013000*01 -COPY WMEDAREA                                                        
013100     SKIP3                                                                
013200 01  MESSAGE-CODES.                                                       
013300     03  PARTNO-MISSING          PIC X(3)    VALUE '017'.                 
013400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013500     EJECT                                                                
013600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013700*                                                                         
013800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013900     SKIP3                                                                
014000*01 -COPY WMSGINIT                                                        
014100     EJECT                                                                
014200*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
014300*                                                                         
014400 01  FILLER                      PIC X(16)                                
014500                                       VALUE 'DC-TABELL-SORT'.            
014600 01  TABENTRY-PARM.                                                       
014700     03  STEGLANGD               PIC S9(9) COMP.                          
014800     03  ANTAL                   PIC S9(9) COMP.                          
014900     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 2.                 
015000                                                                          
015100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015200*                                                                         
015300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015400     SKIP3                                                                
015500*01  MID -COPY W2I35401                                                   
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015800     SKIP3                                                                
015900*01  -COPY WMSGAREA                                                       
016000     EJECT                                                                
016100     03  MOD REDEFINES MSG-AREA.                                          
016200*      05  -COPY W2O35401                                                 
016300     EJECT                                                                
016400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016500     SKIP3                                                                
016600*01  -COPY WMFSAREA                                                       
016700     EJECT                                                                
016800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016900*                                                                         
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017200     SKIP3                                                                
017300 01  NYCKLAR-TILL-DLI.                                                    
017400     03  W-IDARTNR-X.                                                     
017500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017600     03  W-IDDC-X.                                                        
017700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
017800     03  W-IDDC-K7-X.                                                     
017900         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
018000     03  W-IDSKYLT-X.                                                     
018100         05  W-IDSKYLT           PIC X(3)    VALUE 'USA'.                 
018200     03  W-IDDC-B6-X.                                                     
018300         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
018400     SKIP2                                                                
018500*    --- STATUS-KOD FRÅN IMS                                              
018600 01  STATUS-WS                   PIC XX.                                  
018700     88  SEGMENT-FINNS                       VALUE '  '.                  
018800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
019100     SKIP2                                                                
019200 01  GODK-STATUSKODER.                                                    
019300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019400     SKIP3                                                                
019500 01  SSA1                        PIC X(64).                               
019600 01  SSA2                        PIC X(64).                               
019700     EJECT                                                                
019800*    --- IMS FUNKTIONSKODER                                               
019900*01  -COPY W0003                                                          
020000     EJECT                                                                
020100*    ---  DLI INPUT-OUTPUT AREA                                           
020200                                                                          
020300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
020400 01  DLI-IO-WDK601.                                                       
020500*    03  -COPY WDK601  -PRE WDK6-                                         
020600     EJECT                                                                
020700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
020800 01  DLI-IO-WDK611.                                                       
020900*    03  -COPY WDK611  -PRE WDK6-                                         
021000     EJECT                                                                
021100 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
021200 01  DLI-IO-WDK629.                                                       
021300*    03  -COPY WDK629  -PRE WDK6-                                         
021400     EJECT                                                                
021500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
021600 01  DLI-IO-WDK701.                                                       
021700*    03  -COPY WDK701  -PRE WDK7-                                         
021800     EJECT                                                                
021900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
022000 01  DLI-IO-WDK711.                                                       
022100*    03  -COPY WDK711  -PRE WDK7-                                         
022200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC01'.                    
022300 01  DLI-IO-WLINLC01.                                                     
022400*    03  -COPY WDL601  -PRE INLC-                                         
022500     EJECT                                                                
022600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC11'.                    
022700 01  DLI-IO-WLINLC11.                                                     
022800*    03  -COPY WDL611  -PRE INLC-                                         
022900     EJECT                                                                
023000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLINLC12'.                    
023100 01  DLI-IO-WLINLC12.                                                     
023200*    03  -COPY WDL612  -PRE INLC-                                         
023300     SKIP3                                                                
023400 01  DLI-IO-WLBENA.                                                       
023500     03  IO-WLBENA               PIC X(150)  VALUE SPACE.                 
023600     EJECT                                                                
023700     03  WLBENA11 REDEFINES IO-WLBENA.                                    
023800*        05  -COPY WDD311  -PRE BENA-                                     
023900     EJECT                                                                
024000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB601'.                      
024100 01  DLI-IO-WDB601.                                                       
024200*    03  -COPY WDB601                                                     
024300     EJECT                                                                
024400 LINKAGE SECTION.                                                         
024500*01  -COPY W0009   -PRE MSG-                                              
024600*01  -COPY W0008   -PRE USEA-                                             
024700     05  FILLER                  PIC X.                                   
024800     EJECT                                                                
024900*01  -COPY W0008  -PRE WDK6-                                              
025000     05  FILLER                  PIC X.                                   
025100     EJECT                                                                
025200*01  -COPY W0008  -PRE WDK7-                                              
025300     05  FILLER                  PIC X.                                   
025400     EJECT                                                                
025500*01  -COPY W0008  -PRE INLC-                                              
025600     05  FILLER                  PIC X.                                   
025700     EJECT                                                                
025800*01  -COPY W0008  -PRE BENA-                                              
025900     05  FILLER                  PIC X.                                   
026000     EJECT                                                                
026100*01  -COPY W0008  -PRE WDB6-                                              
026200     05  FILLER                  PIC X.                                   
026300     EJECT                                                                
026400*01  -COPY W0008  -PRE WDB6-1-                                            
026500     05  FILLER                  PIC X.                                   
026600     EJECT                                                                
026700 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK6-PCB WDK7-PCB             
026800     INLC-PCB BENA-PCB WDB6-PCB WDB6-1-PCB.                               
026900 MAIN SECTION.                                                            
027000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK6-PCB WDK7-PCB             
027100     INLC-PCB BENA-PCB WDB6-PCB WDB6-1-PCB.                               
027200                                                                          
027300     PERFORM IMS-GET-MSG                                                  
027400     IF SEGMENT-FINNS                                                     
027500       PERFORM A-INIT                                                     
027600       PERFORM B-KOLLA-NYCKLAR                                            
027700       IF NYCKLAR-OK                                                      
027800         PERFORM F-LAES-VISA-INFO                                         
027900       END-IF                                                             
028000       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O35401 + 4                      
028100       PERFORM IMS-INSERT-MSG                                             
028200     END-IF                                                               
028300                                                                          
028400     MOVE ZERO TO RETURN-CODE                                             
028500     GOBACK                                                               
028600     .                                                                    
028700     EJECT                                                                
028800 A-INIT SECTION.                                                          
028900                                                                          
029000     IF MSG-DUBBLA-TRANSKODER                                             
029100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I35401                 
029200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
029300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
029400     ELSE                                                                 
029500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I35401                  
029600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
029700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
029800     END-IF                                                               
029900                                                                          
030000     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
030100     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
030200     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
030300                                                                          
030400     MOVE LOW-VALUE        TO MSG-AREA                                    
030500     MOVE 'W2O354N1'       TO MFS-IDMOD                                   
030600     MOVE '2354'           TO MOD-IDTRANS                                 
030700     MOVE MFS-RENSA-FAELT  TO MOD-TEMFSFEL MOD-TEMFSINF                   
030800                                                                          
030900     MOVE +2               TO SPRAK-IX                                    
031000     MOVE 'GB '            TO MED-IDSKYLT                                 
031100                                                                          
031200     IF EGEN-MID OR HELP-MID                                              
031300       CONTINUE                                                           
031400     ELSE                                                                 
031500       MOVE SPACE          TO MFS-KDTRTYP                                 
031600       MOVE '7'            TO MFS-IDPFK                                   
031700     END-IF                                                               
031800     .                                                                    
031900     EJECT                                                                
032000 B-KOLLA-NYCKLAR SECTION.                                                 
032100                                                                          
032200     MOVE ALL '+'                  TO MSGI-WMSGINIT                       
032300     MOVE '001'                    TO MSGI-KDCALL                         
032400     MOVE MSG-LTERM-NAME           TO MSGI-IDLTERM-USER                   
032500     MOVE MSG-SIGNON-USERID        TO MSGI-IDUSER                         
032600     MOVE '2354'                   TO MSGI-IDTRANS                        
032700     IF GODK-MID                                                          
032800        MOVE MID-IDARTNR-IN        TO MSGI-IDARTNR                        
032900        MOVE MID-IDDC-IN           TO MSGI-IDDC-KEY                       
033000     END-IF                                                               
033100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033200                                                                          
033300     MOVE JA                       TO NYCKLAR-SW                          
033400                                                                          
033500*    -- KONTROLL AV IDARTNR                                               
033600     MOVE MFS-RENSA-FAELT          TO MOD-IDARTNR-IN                      
033700                                                                          
033800     IF  MID-IDARTNR-IN             = ALL '+'                             
033900     AND MID-IDARTNR-IN             = ALL '+'                             
034000         CONTINUE                                                         
034100     ELSE                                                                 
034200       MOVE '7'                    TO MFS-IDPFK                           
034300       MOVE SPACE                  TO MFS-KDTRTYP                         
034400     END-IF                                                               
034500     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
034600     IF MSGI-IDARTNR NUMERIC                                              
034700        MOVE MSGI-IDARTNR          TO W-IDARTNR                           
034800     ELSE                                                                 
034900        MOVE NEJ                   TO NYCKLAR-SW                          
035000     END-IF                                                               
035100                                                                          
035200*    -- KONTROLL AV IDDC                                                  
035300     MOVE MFS-RENSA-FAELT          TO MOD-IDDC-IN                         
035400                                                                          
035500     MOVE MSGI-IDDC-KEY            TO W-IDDC-B6                           
035600*                                                                         
035700     PERFORM BA-CHECK-IDDC                                                
035800                                                                          
035900     IF GODK-MID OR NYCKLAR-OK                                            
036000        MOVE W-IDARTNR             TO MOD-IDARTNR-UT                      
036100        MOVE W-IDDC-B6             TO MOD-IDDC-UT                         
036200     ELSE                                                                 
036300        MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-UT                      
036400                                      MOD-IDDC-UT                         
036500     END-IF                                                               
036600                                                                          
036700     IF NYCKLAR-FEL                                                       
036800        MOVE ERR-WRONG-KEY         TO MED-IDMFSFEL                        
036900        CALL WMEDKONV           USING MED-WMEDAREA                        
037000        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
037100        PERFORM MFS-RENSA-FAELT-IN                                        
037200        PERFORM MFS-RENSA-FAELT-UT                                        
037300     END-IF                                                               
037400     .                                                                    
037500     EJECT                                                                
037600 BA-CHECK-IDDC    SECTION.                                                
037700                                                                          
037800     PERFORM IMS-GU-WDB601                                                
037900                                                                          
038000     IF SEGMENT-SAKNAS                                                    
038100        MOVE NEJ                   TO NYCKLAR-SW                          
038200        MOVE SPACES                TO W-IDDC-B6                           
038300     ELSE                                                                 
038400        IF DCS-NDC OR DCS-CDC                                             
038500           MOVE DCS-KDDC           TO WS-SPAR-KDDC                        
038600           MOVE DCS-IDDC           TO WS-IDDC                             
038700           PERFORM BAA-LOAD-WDB6-TABLE                                    
038800           PERFORM BAB-SORT-WDB6-TABLE                                    
038900        ELSE                                                              
039000           MOVE NEJ                TO NYCKLAR-SW                          
039100        END-IF                                                            
039200     END-IF                                                               
039300     .                                                                    
039400     EJECT                                                                
039500 BAA-LOAD-WDB6-TABLE  SECTION.                                            
039600                                                                          
039700     INITIALIZE W-TAB-WDB6                                                
039800     MOVE +1                       TO IX-B6                               
039900     PERFORM IMS-GN-WDB601                                                
040000     IF SEGMENT-FINNS                                                     
040100        PERFORM UNTIL SEGMENT-SLUT                                        
040200          IF DCS-KDDC               = WS-SPAR-KDDC                        
040300             MOVE DCS-IDDC         TO W-TAB-IDDC (IX-B6)                  
040400             ADD +1                TO IX-B6                               
040500          END-IF                                                          
040600          PERFORM IMS-GN-WDB601                                           
040700        END-PERFORM                                                       
040800     END-IF                                                               
040900*                                                                         
041000     IF  IX-B6 > +1                                                       
041100     AND W-TAB-IDDC (IX-B6)     NOT > SPACES                              
041200         COMPUTE IX-B6  = IX-B6 - 1                                       
041300     END-IF                                                               
041400     MOVE IX-B6                    TO MAX-IX-B6                           
041500     .                                                                    
041600     EJECT                                                                
041700 BAB-SORT-WDB6-TABLE  SECTION.                                            
041800                                                                          
041900*    --- SORT TABLE ON IDDC                                               
042000     MOVE MAX-IX-B6               TO ANTAL                                
042100     MOVE LENGTH OF W-TAB-IDDC(1) TO STEGLANGD                            
042200     MOVE LENGTH OF W-TAB-IDDC(1) TO NYCKELLANGD                          
042300                                                                          
042400     CALL WINTSOR USING W-TAB-WDB6  STEGLANGD  ANTAL                      
042500                  W-TAB-IDDC(1) NYCKELLANGD                               
042600     .                                                                    
042700     EJECT                                                                
042800 F-LAES-VISA-INFO SECTION.                                                
042900                                                                          
043000     IF CDC                                                               
043100        PERFORM FA-READ-CDCREF-INFO                                       
043200     ELSE                                                                 
043300        PERFORM FB-READ-NDCINFO                                           
043400     END-IF                                                               
043500                                                                          
043600     IF INDATA-OK                                                         
043700        PERFORM S02-NUVARANDE-VECKA                                       
043800        PERFORM FC-LAES-BENA                                              
043900        IF CDC                                                            
044000          PERFORM FD-READ-ETAREF-CDC                                      
044100        ELSE                                                              
044200          PERFORM FE-READ-ETAREF-NDC                                      
044300        END-IF                                                            
044400        PERFORM FF-ALL-TOTAL                                              
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800 FA-READ-CDCREF-INFO  SECTION.                                            
044900                                                                          
045000     PERFORM IMS-GU-WDK601                                                
045100*                                                                         
045200     IF SEGMENT-SAKNAS                                                    
045300        MOVE NEJ                   TO INDATA-SW                           
045400        MOVE PARTNO-MISSING        TO MED-IDMFSFEL                        
045500        CALL WMEDKONV           USING MED-WMEDAREA                        
045600        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
045700        PERFORM MFS-RENSA-FAELT-UT                                        
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 FB-READ-NDCINFO   SECTION.                                               
046200                                                                          
046300     PERFORM IMS-GET-WDK701                                               
046400*                                                                         
046500     IF SEGMENT-SAKNAS                                                    
046600        MOVE NEJ                   TO INDATA-SW                           
046700        MOVE PARTNO-MISSING        TO MED-IDMFSFEL                        
046800        CALL WMEDKONV           USING MED-WMEDAREA                        
046900        MOVE MED-MFSFEL            TO MOD-TEMFSFEL                        
047000        PERFORM MFS-RENSA-FAELT-UT                                        
047100     END-IF                                                               
047200     .                                                                    
047300     EJECT                                                                
047400 FC-LAES-BENA SECTION.                                                    
047500                                                                          
047600     PERFORM IMS-GU-WLBENA11                                              
047700     IF SEGMENT-FINNS                                                     
047800       MOVE BENA-TEXT-BEART        TO MOD-BEART                           
047900     ELSE                                                                 
048000       MOVE SPACE                  TO MOD-BEART                           
048100     END-IF                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 FD-READ-ETAREF-CDC   SECTION.                                            
048500                                                                          
048600     MOVE +1                       TO INDX                                
048700     PERFORM UNTIL   INDX           > MAX-IX-B6                           
048800                                                                          
048900       MOVE W-TAB-IDDC (INDX)      TO W-IDDC                              
049000                                      W-IDDC-K7                           
049100                                      WS-IDDC-TEXT                        
049200                                      WS-SPAR-B6-IDDC                     
049300       MOVE WS-DC-TEXT-GRP         TO MOD-TEXT-DC  (INDX)                 
049400*                                                                         
049500       PERFORM IMS-GU-WDK611                                              
049600       IF SEGMENT-FINNS                                                   
049700          PERFORM IMS-GNP-WDK629                                          
049800*---      (CHECKING IF REFILL PART)                                       
049900          IF SEGMENT-FINNS                                                
050000             MOVE WDK6-CLAG-KVLS   TO MOD-KVLS-DC      (INDX)             
050100                                      WS-KVLS-DC       (INDX)             
050200             MOVE WDK6-CLAG-KVAKS-CDC                                     
050300                                   TO MOD-KVAK-DC      (INDX)             
050400                                      WS-KVAK-DC       (INDX)             
050500             PERFORM S01-READ-INFO-WDL6                                   
050600          ELSE                                                            
050700             MOVE ZERO             TO MOD-KVLS-DC      (INDX)             
050800                                      MOD-KVAK-DC      (INDX)             
050900                                      MOD-KVAVIS-DC-WC (INDX)             
051000                                      MOD-KVAVIS-DC-W1 (INDX)             
051100                                      MOD-KVAVIS-DC-W2 (INDX)             
051200                                      MOD-KVAVIS-DC-W3 (INDX)             
051300                                      MOD-KVAVIS-DC-W4 (INDX)             
051400                                      MOD-KVAVIS-DC-W5 (INDX)             
051500                                      MOD-KVAVIS-DC-W6 (INDX)             
051600          END-IF                                                          
051700       END-IF                                                             
051800       ADD +1                      TO INDX                                
051900     END-PERFORM                                                          
052000     .                                                                    
052100     EJECT                                                                
052200 FE-READ-ETAREF-NDC   SECTION.                                            
052300                                                                          
052400     MOVE +1                       TO INDX                                
052500     PERFORM UNTIL   INDX           > MAX-IX-B6                           
052600                                                                          
052700       MOVE W-TAB-IDDC (INDX)      TO W-IDDC                              
052800                                      W-IDDC-K7                           
052900                                      WS-IDDC-TEXT                        
053000                                      WS-SPAR-B6-IDDC                     
053100       MOVE WS-DC-TEXT-GRP         TO MOD-TEXT-DC  (INDX)                 
053200*                                                                         
053300       PERFORM IMS-GU-WDK711                                              
053400       IF SEGMENT-FINNS                                                   
053500          IF WDK7-SLAG-IDDC-REF     > SPACES                              
053600             MOVE WDK7-SLAG-KVLS   TO MOD-KVLS-DC      (INDX)             
053700                                      WS-KVLS-DC       (INDX)             
053800             MOVE WDK7-SLAG-KVAKS-SDC                                     
053900                                   TO MOD-KVAK-DC      (INDX)             
054000                                      WS-KVAK-DC       (INDX)             
054100             PERFORM S01-READ-INFO-WDL6                                   
054200          ELSE                                                            
054300             MOVE ZERO             TO MOD-KVLS-DC      (INDX)             
054400                                      MOD-KVAK-DC      (INDX)             
054500                                      MOD-KVAVIS-DC-WC (INDX)             
054600                                      MOD-KVAVIS-DC-W1 (INDX)             
054700                                      MOD-KVAVIS-DC-W2 (INDX)             
054800                                      MOD-KVAVIS-DC-W3 (INDX)             
054900                                      MOD-KVAVIS-DC-W4 (INDX)             
055000                                      MOD-KVAVIS-DC-W5 (INDX)             
055100                                      MOD-KVAVIS-DC-W6 (INDX)             
055200          END-IF                                                          
055300       ELSE                                                               
055400          MOVE ZERO                TO MOD-KVLS-DC      (INDX)             
055500                                      MOD-KVAK-DC      (INDX)             
055600                                      MOD-KVAVIS-DC-WC (INDX)             
055700                                      MOD-KVAVIS-DC-W1 (INDX)             
055800                                      MOD-KVAVIS-DC-W2 (INDX)             
055900                                      MOD-KVAVIS-DC-W3 (INDX)             
056000                                      MOD-KVAVIS-DC-W4 (INDX)             
056100                                      MOD-KVAVIS-DC-W5 (INDX)             
056200                                      MOD-KVAVIS-DC-W6 (INDX)             
056300       END-IF                                                             
056400                                                                          
056500       ADD +1                      TO INDX                                
056600     END-PERFORM                                                          
056700     .                                                                    
056800     EJECT                                                                
056900 FF-ALL-TOTAL   SECTION.                                                  
057000                                                                          
057100     MOVE +1                       TO IX-TOT                              
057200     MOVE SPACES                   TO WS-DC-TEXT-GRP                      
057300*                                                                         
057400     MOVE WS-TOTAL-TEXT            TO WS-DC-TEXT-GRP                      
057500     MOVE WS-DC-TEXT-GRP           TO MOD-TEXT-DC  (INDX)                 
057600*                                                                         
057700     PERFORM UNTIL IX-TOT           > MAX-IX-B6                           
057800       COMPUTE WS-KVLS-TOT          = WS-KVLS-TOT         +               
057900                                      WS-KVLS-DC (IX-TOT)                 
058000       COMPUTE WS-KVAK-TOT          = WS-KVAK-TOT         +               
058100                                      WS-KVAK-DC (IX-TOT)                 
058200*                                                                         
058300       MOVE +1                     TO IX                                  
058400       PERFORM UNTIL  IX            > MAX-IX                              
058500         EVALUATE IX                                                      
058600           WHEN 1                                                         
058700             COMPUTE WS-KVAVIS-WC-TOT                                     
058800                                    = WS-KVAVIS-WC-TOT    +               
058900                                      WS-KVAVIS-DC (IX-TOT, IX)           
059000           WHEN 2                                                         
059100             COMPUTE WS-KVAVIS-W1-TOT                                     
059200                                    = WS-KVAVIS-W1-TOT    +               
059300                                      WS-KVAVIS-DC (IX-TOT, IX)           
059400           WHEN 3                                                         
059500             COMPUTE WS-KVAVIS-W2-TOT                                     
059600                                    = WS-KVAVIS-W2-TOT    +               
059700                                      WS-KVAVIS-DC (IX-TOT, IX)           
059800           WHEN 4                                                         
059900             COMPUTE WS-KVAVIS-W3-TOT                                     
060000                                    = WS-KVAVIS-W3-TOT    +               
060100                                      WS-KVAVIS-DC (IX-TOT, IX)           
060200           WHEN 5                                                         
060300             COMPUTE WS-KVAVIS-W4-TOT                                     
060400                                    = WS-KVAVIS-W4-TOT    +               
060500                                      WS-KVAVIS-DC (IX-TOT, IX)           
060600           WHEN 6                                                         
060700             COMPUTE WS-KVAVIS-W5-TOT                                     
060800                                    = WS-KVAVIS-W5-TOT    +               
060900                                      WS-KVAVIS-DC (IX-TOT, IX)           
061000           WHEN 7                                                         
061100             COMPUTE WS-KVAVIS-W6-TOT                                     
061200                                    = WS-KVAVIS-W6-TOT    +               
061300                                      WS-KVAVIS-DC (IX-TOT, IX)           
061400         END-EVALUATE                                                     
061500         ADD +1                    TO IX                                  
061600       END-PERFORM                                                        
061700       ADD +1                      TO IX-TOT                              
061800     END-PERFORM                                                          
061900*                                                                         
062000     MOVE WS-KVLS-TOT              TO MOD-KVLS-DC      (INDX)             
062100     MOVE WS-KVAK-TOT              TO MOD-KVAK-DC      (INDX)             
062200     MOVE WS-KVAVIS-WC-TOT         TO MOD-KVAVIS-DC-WC (INDX)             
062300     MOVE WS-KVAVIS-W1-TOT         TO MOD-KVAVIS-DC-W1 (INDX)             
062400     MOVE WS-KVAVIS-W2-TOT         TO MOD-KVAVIS-DC-W2 (INDX)             
062500     MOVE WS-KVAVIS-W3-TOT         TO MOD-KVAVIS-DC-W3 (INDX)             
062600     MOVE WS-KVAVIS-W4-TOT         TO MOD-KVAVIS-DC-W4 (INDX)             
062700     MOVE WS-KVAVIS-W5-TOT         TO MOD-KVAVIS-DC-W5 (INDX)             
062800     MOVE WS-KVAVIS-W6-TOT         TO MOD-KVAVIS-DC-W6 (INDX)             
062900     .                                                                    
063000     EJECT                                                                
063100 S01-READ-INFO-WDL6  SECTION.                                             
063200                                                                          
063300     IF  (CDC                                                             
063400     AND  WDK6-CLAG-KVAKS-PAV       = 0 )                                 
063500     OR  (NDC                                                             
063600     AND  WDK7-SLAG-KVAKS-PAV       = 0 )                                 
063700         CONTINUE                                                         
063800     ELSE                                                                 
063900       PERFORM IMS-GET-INLC-ART                                           
064000       IF SEGMENT-FINNS                                                   
064100         PERFORM IMS-GET-INLC-INL                                         
064200         IF SEGMENT-FINNS                                                 
064300           PERFORM UNTIL SEGMENT-SAKNAS                                   
064400             IF  INLC-INL-IDPTYP          = 'R30'                         
064500             AND INLC-INL-IDDC            = WS-SPAR-B6-IDDC               
064600               IF  WS-TIAAVV-IDAG         > 0                             
064700               AND INLC-INL-TIBERANK      > 0                             
064800                 PERFORM S03-BERAKNA-ANKOMSTVECKA-INL                     
064900                 IF WS-TIVV-PLUS-MINUS    < 6                             
065000                   COMPUTE IX             = WS-TIVV + 1                   
065100                   ADD INLC-INL-KVAVIS   TO                               
065200                                       WS-KVAVIS-DC (INDX, IX)            
065300                 ELSE                                                     
065400                   ADD INLC-INL-KVAVIS   TO                               
065500                                       WS-KVAVIS-DC (INDX, MAX-IX)        
065600                 END-IF                                                   
065700               END-IF                                                     
065800             END-IF                                                       
065900             PERFORM IMS-GET-INLC-INL                                     
066000           END-PERFORM                                                    
066100         END-IF                                                           
066200       END-IF                                                             
066300     END-IF                                                               
066400                                                                          
066500     PERFORM IMS-GET-INLC-ART                                             
066600     IF SEGMENT-FINNS                                                     
066700       PERFORM IMS-GET-INLC-ORD                                           
066800       IF SEGMENT-FINNS                                                   
066900         PERFORM UNTIL SEGMENT-SAKNAS                                     
067000           IF INLC-ORD-IDDC               = WS-SPAR-B6-IDDC               
067100             IF INLC-ORD-IDLOPNRM         = 0                             
067200               IF  WS-TIAAVV-IDAG         > 0                             
067300               AND INLC-ORD-TIBERANK      > 0                             
067400                 PERFORM S04-BERAKNA-ANKOMSTVECKA-ORD                     
067500                 IF WS-TIVV-PLUS-MINUS    < 6                             
067600                    COMPUTE IX            = WS-TIVV + 1                   
067700                    ADD INLC-ORD-KVBEART TO                               
067800                                       WS-KVAVIS-DC (INDX, IX)            
067900                 ELSE                                                     
068000                    ADD INLC-ORD-KVBEART TO                               
068100                                       WS-KVAVIS-DC (INDX, MAX-IX)        
068200                 END-IF                                                   
068300               END-IF                                                     
068400             END-IF                                                       
068500           END-IF                                                         
068600           PERFORM IMS-GET-INLC-ORD                                       
068700         END-PERFORM                                                      
068800       END-IF                                                             
068900     END-IF                                                               
069000*                                                                         
069100     MOVE +1                             TO IX                            
069200     PERFORM UNTIL IX  > MAX-IX                                           
069300       EVALUATE IX                                                        
069400         WHEN 1                                                           
069500            MOVE WS-KVAVIS-DC (INDX, IX) TO MOD-KVAVIS-DC-WC(INDX)        
069600         WHEN 2                                                           
069700            MOVE WS-KVAVIS-DC (INDX, IX) TO MOD-KVAVIS-DC-W1(INDX)        
069800         WHEN 3                                                           
069900            MOVE WS-KVAVIS-DC (INDX, IX) TO MOD-KVAVIS-DC-W2(INDX)        
070000         WHEN 4                                                           
070100            MOVE WS-KVAVIS-DC (INDX, IX) TO MOD-KVAVIS-DC-W3(INDX)        
070200         WHEN 5                                                           
070300            MOVE WS-KVAVIS-DC (INDX, IX) TO MOD-KVAVIS-DC-W4(INDX)        
070400         WHEN 6                                                           
070500            MOVE WS-KVAVIS-DC (INDX, IX) TO MOD-KVAVIS-DC-W5(INDX)        
070600         WHEN 7                                                           
070700            MOVE WS-KVAVIS-DC (INDX, IX) TO MOD-KVAVIS-DC-W6(INDX)        
070800         WHEN OTHER                                                       
070900            CONTINUE                                                      
071000       END-EVALUATE                                                       
071100       ADD +1                            TO IX                            
071200     END-PERFORM                                                          
071300     .                                                                    
071400     EJECT                                                                
071500 S02-NUVARANDE-VECKA SECTION.                                             
071600                                                                          
071700     MOVE 'IDAG'                 TO DAT-KDDATFORM                         
071800     CALL WDATKONV USING         DAT-KDDATFORM                            
071900                                 DAT-I-TIDATUM                            
072000                                 DAT-O-TIDATUM                            
072100                                 DAT-KDSVAR                               
072200                                                                          
072300     IF  DAT-KDSVAR-OK                                                    
072400       MOVE DAT-TIAAVV-GRP       TO WS-TIAAVV-IDAG-X                      
072500       MOVE DAT-TISEKEL          TO WS-TISEKEL-IDAG                       
072600     ELSE                                                                 
072700       MOVE ZERO                 TO WS-TIAAVV-IDAG-X                      
072800     END-IF                                                               
072900     .                                                                    
073000     EJECT                                                                
073100 S03-BERAKNA-ANKOMSTVECKA-INL SECTION.                                    
073200                                                                          
073300     MOVE "AAMMDD"               TO DAT-KDDATFORM                         
073400     MOVE INLC-INL-TIBERANK      TO DAT-I-TIDATUM                         
073500                                                                          
073600     CALL WDATKONV USING         DAT-KDDATFORM                            
073700                                 DAT-I-TIDATUM                            
073800                                 DAT-O-TIDATUM                            
073900                                 DAT-KDSVAR                               
074000                                                                          
074100     IF  DAT-KDSVAR-OK                                                    
074200       MOVE DAT-TIAAVV-GRP       TO WS-TIAAVV-X                           
074300       MOVE DAT-TISEKEL          TO WS-TISEKEL                            
074400       PERFORM S05-BERAKNA-ANKOMST                                        
074500     END-IF                                                               
074600     .                                                                    
074700     EJECT                                                                
074800 S04-BERAKNA-ANKOMSTVECKA-ORD SECTION.                                    
074900                                                                          
075000     MOVE "AAMMDD"               TO DAT-KDDATFORM                         
075100     MOVE INLC-ORD-TIBERANK      TO DAT-I-TIDATUM                         
075200                                                                          
075300     CALL WDATKONV USING         DAT-KDDATFORM                            
075400                                 DAT-I-TIDATUM                            
075500                                 DAT-O-TIDATUM                            
075600                                 DAT-KDSVAR                               
075700                                                                          
075800     IF  DAT-KDSVAR-OK                                                    
075900       MOVE DAT-TIAAVV-GRP       TO WS-TIAAVV-X                           
076000       MOVE DAT-TISEKEL          TO WS-TISEKEL                            
076100       PERFORM S05-BERAKNA-ANKOMST                                        
076200     END-IF                                                               
076300     .                                                                    
076400     EJECT                                                                
076500 S05-BERAKNA-ANKOMST SECTION.                                             
076600                                                                          
076700     IF WS-TIAA = WS-TIAA-IDAG                                            
076800       CONTINUE                                                           
076900     ELSE                                                                 
077000       IF WS-TISEKEL < WS-TISEKEL-IDAG                                    
077100         MOVE ZERO               TO WS-TIVV                               
077200       ELSE                                                               
077300         IF WS-TIAA < WS-TIAA-IDAG                                        
077400           MOVE ZERO             TO WS-TIVV                               
077500         ELSE                                                             
077600*         *DVS  WS-TIAA > WS-TIAA-IDAG                                    
077700           COMPUTE SUM-AA = WS-TIAA - WS-TIAA-IDAG                        
077800           COMPUTE SUM-VV = 52 * SUM-AA                                   
077900           ADD WS-TIVV           TO SUM-VV                                
078000           IF SUM-VV > 90                                                 
078100              MOVE 90            TO WS-TIVV                               
078200           ELSE                                                           
078300              MOVE SUM-VV        TO WS-TIVV                               
078400           END-IF                                                         
078500         END-IF                                                           
078600       END-IF                                                             
078700     END-IF                                                               
078800* -- KOLLA OM ETA-VECKA ÄR PASSERAD. ISFÅLL LÄGGS AVISERAT ANTAL          
078900* -- I CURRENT WEEK OAVSETT OM DET ÄR RÄTT ELLER INTE :-)                 
079000     COMPUTE WS-TIVV-PLUS-MINUS = WS-TIVV - WS-TIVV-IDAG                  
079100     IF WS-TIVV-PLUS-MINUS < ZERO                                         
079200       MOVE ZERO TO WS-TIVV                                               
079300     ELSE                                                                 
079400       COMPUTE WS-TIVV = WS-TIVV - WS-TIVV-IDAG                           
079500     END-IF                                                               
079600     .                                                                    
079700     EJECT                                                                
079800 MFS-RENSA-FAELT-UT SECTION.                                              
079900                                                                          
080000*    --- ALLA UTDATA-FÄLT                                                 
080100     MOVE MFS-RENSA-FAELT   TO  MOD-BEART                                 
080200                                                                          
080300     MOVE +1                TO  INDX                                      
080400     PERFORM UNTIL  INDX     >  MAX-INDX                                  
080500       MOVE MFS-RENSA-FAELT TO                                            
080600                                MOD-TEXT-DC      (INDX)                   
080700                                MOD-KVLS-DC      (INDX)                   
080800                                MOD-KVAK-DC      (INDX)                   
080900                                MOD-KVAVIS-DC-WC (INDX)                   
081000                                MOD-KVAVIS-DC-W1 (INDX)                   
081100                                MOD-KVAVIS-DC-W2 (INDX)                   
081200                                MOD-KVAVIS-DC-W3 (INDX)                   
081300                                MOD-KVAVIS-DC-W4 (INDX)                   
081400                                MOD-KVAVIS-DC-W5 (INDX)                   
081500                                MOD-KVAVIS-DC-W6 (INDX)                   
081600       ADD +1               TO  INDX                                      
081700     END-PERFORM                                                          
081800     .                                                                    
081900     SKIP3                                                                
082000 MFS-RENSA-FAELT-IN SECTION.                                              
082100                                                                          
082200*    --- ALLA INDATA-FÄLT                                                 
082300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
082400                             MOD-IDDC-IN                                  
082500     .                                                                    
082600     EJECT                                                                
082700* --- IMS SEKTIONER ---                                                   
082800     SKIP3                                                                
082900 IMS-GET-MSG SECTION.                                                     
083000                                                                          
083100     MOVE '  QC' TO GODK-STATUSKODER                                      
083200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
083300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
083400     PERFORM IMS-STATUSKONTROLL                                           
083500     .                                                                    
083600     SKIP3                                                                
083700 IMS-INSERT-MSG SECTION.                                                  
083800                                                                          
083900     MOVE 'N'       TO MFS-KDHUVOMR                                       
084000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
084100     MOVE SPACE TO GODK-STATUSKODER                                       
084200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
084300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
084400     PERFORM IMS-STATUSKONTROLL                                           
084500     .                                                                    
084600     EJECT                                                                
084700 IMS-GU-WDK601      SECTION.                                              
084800                                                                          
084900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
085000          DELIMITED BY SIZE INTO SSA1                                     
085100     MOVE '  GE' TO GODK-STATUSKODER                                      
085200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
085300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
085400     PERFORM IMS-STATUSKONTROLL                                           
085500     .                                                                    
085600     EJECT                                                                
085700 IMS-GU-WDK611       SECTION.                                             
085800                                                                          
085900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
086000          DELIMITED BY SIZE INTO SSA1                                     
086100     MOVE 'WDK611  '       TO SSA2                                        
086200     MOVE '  GE' TO GODK-STATUSKODER                                      
086300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
086400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
086500     PERFORM IMS-STATUSKONTROLL                                           
086600     .                                                                    
086700     EJECT                                                                
086800 IMS-GNP-WDK629     SECTION.                                              
086900                                                                          
087000     MOVE 'WDK629  '       TO SSA1                                        
087100     MOVE '  GE'   TO GODK-STATUSKODER                                    
087200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK629 SSA1                   
087300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
087400     PERFORM IMS-STATUSKONTROLL                                           
087500     .                                                                    
087600     EJECT                                                                
087700 IMS-GET-WDK701    SECTION.                                               
087800                                                                          
087900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
088000          DELIMITED BY SIZE INTO SSA1                                     
088100     MOVE '  GE' TO GODK-STATUSKODER                                      
088200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
088300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
088400     PERFORM IMS-STATUSKONTROLL                                           
088500     .                                                                    
088600     EJECT                                                                
088700 IMS-GU-WDK711    SECTION.                                                
088800                                                                          
088900     STRING 'WDK711  (IDDC     =' W-IDDC-K7-X ')'                         
089000          DELIMITED BY SIZE INTO SSA1                                     
089100     MOVE '  GE' TO GODK-STATUSKODER                                      
089200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1                    
089300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
089400     PERFORM IMS-STATUSKONTROLL                                           
089500     .                                                                    
089600     EJECT                                                                
089700 IMS-GET-INLC-ART SECTION.                                                
089800                                                                          
089900     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
090000          DELIMITED BY SIZE INTO SSA1                                     
090100     MOVE '  GE' TO GODK-STATUSKODER                                      
090200     CALL CBLTDLI USING GU INLC-PCB DLI-IO-WLINLC01 SSA1                  
090300     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
090400     PERFORM IMS-STATUSKONTROLL                                           
090500     .                                                                    
090600     EJECT                                                                
090700 IMS-GET-INLC-INL SECTION.                                                
090800                                                                          
090900     STRING 'WLINLC11 '                                                   
091000          DELIMITED BY SIZE INTO SSA1                                     
091100     MOVE '  GE' TO GODK-STATUSKODER                                      
091200     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-WLINLC11 SSA1                 
091300     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
091400     PERFORM IMS-STATUSKONTROLL                                           
091500     .                                                                    
091600     EJECT                                                                
091700 IMS-GET-INLC-ORD SECTION.                                                
091800                                                                          
091900     STRING 'WLINLC12 '                                                   
092000          DELIMITED BY SIZE INTO SSA1                                     
092100     MOVE '  GE' TO GODK-STATUSKODER                                      
092200     CALL CBLTDLI USING GNP INLC-PCB DLI-IO-WLINLC12 SSA1                 
092300     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
092400     PERFORM IMS-STATUSKONTROLL                                           
092500     .                                                                    
092600     EJECT                                                                
092700 IMS-GU-WLBENA11 SECTION.                                                 
092800                                                                          
092900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
093000          DELIMITED BY SIZE INTO SSA1                                     
093100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
093200          DELIMITED BY SIZE INTO SSA2                                     
093300     MOVE '  GE' TO GODK-STATUSKODER                                      
093400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA SSA1 SSA2               
093500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
093600     PERFORM IMS-STATUSKONTROLL                                           
093700     .                                                                    
093800     EJECT                                                                
093900 IMS-GU-WDB601 SECTION.                                                   
094000                                                                          
094100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
094200          DELIMITED BY SIZE INTO SSA1                                     
094300     MOVE '  GE' TO GODK-STATUSKODER                                      
094400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
094500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
094600     PERFORM IMS-STATUSKONTROLL                                           
094700     .                                                                    
094800     EJECT                                                                
094900 IMS-GN-WDB601 SECTION.                                                   
095000                                                                          
095100     MOVE 'WDB601  ' TO SSA1                                              
095200     MOVE '  GB' TO GODK-STATUSKODER                                      
095300     CALL CBLTDLI USING GN WDB6-1-PCB DLI-IO-WDB601 SSA1                  
095400     MOVE WDB6-1-STATUS-CODE TO STATUS-WS                                 
095500     PERFORM IMS-STATUSKONTROLL                                           
095600     .                                                                    
095700     EJECT                                                                
095800 IMS-STATUSKONTROLL SECTION.                                              
095900                                                                          
096000     SET STATUS-IX TO 1                                                   
096100     SEARCH GODK-STATUS                                                   
096200       AT END                                                             
096300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
096400         DELIMITED BY SIZE INTO FELTEXT                                   
096500         CALL FELLOG                                                      
096600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
096700         CONTINUE                                                         
096800     END-SEARCH                                                           
096900     .                                                                    

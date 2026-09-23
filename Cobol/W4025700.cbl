000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4025700.                                                
000400 AUTHOR.         MOGREN STINA.                                            
000500 DATE-WRITTEN.   08/07/29.                                                
000600 DATE-COMPILED.                                                           
000700*    FUNKTION:                                                            
000800*        PROGRAMMET ÄR EN BAKGRUNDS-MPP EFTER W40252                      
000900*        SOM KOLLAR WDGX4251/4252 I WDR5                                  
001000*        FINNS POSTER SKA DE LÄGGAS UPP PÅ DISPATCHERN                    
001100*                                                                         
001200*        PROGRAMMET STARTAS AV WZ01 FRÅN  4252                            
001300*                                                                         
001400*        INFORMATIONEN SÄNDES MED WZ01                                    
001500*                                                                         
001600*        PROGRAMMET LÄSER      WDGX  DLEV-RADER I WDR5                    
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W4T257X                                             
002000*        MID:         W4I25701                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        DLEV-RADER   TILL DISPATCHERN                                    
002400*        MOD:         WMSGKOMI                                            
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W4025700'.            
003200                                                                          
003300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003500                                                                          
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  YES                         PIC X       VALUE 'Y'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000*------                                                                   
004100 77  ANTAL-SEND                  PIC S9(4)   BINARY VALUE ZERO.           
004200                                                                          
004300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004400                                                                          
004500                                                                          
004600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004700     88  NYCKLAR-OK                          VALUE 'J'.                   
004800     88  NYCKLAR-FEL                         VALUE 'N'.                   
004900                                                                          
005000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005100     88  ALLT-OK                             VALUE 'J'.                   
005200                                                                          
005300 77  W-RAD                       PIC 9(5)    VALUE ZERO.                  
005400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33  COMP-3.           
005500 77  KDRC-DISP                   PIC 9(4)    VALUE ZERO.                  
005600                                                                          
005700 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
005800 77  ORAD-IX                     PIC S9(9)  COMP SYNC VALUE ZERO.         
005900 77  ORAD-IX-MAX                 PIC S9(9)  COMP SYNC VALUE +5.           
006000                                                                          
006100 01  FILLER                      PIC X(16)  VALUE 'WS-SEKTION'.           
006200 01  WS-SEKTION                  PIC X(30)  VALUE SPACE.                  
006300                                                                          
006400 01  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
006500 01  WS-TIKLOCK                  PIC S9(9)  VALUE ZERO COMP-3.            
006600                                                                          
006700 01  WS-DATUM                    PIC 9(8).                                
006800 01  FILLER                      REDEFINES WS-DATUM.                      
006900     03  WS-SEKEL                PIC 9(2).                                
007000     03  WS-AAMMDD               PIC 9(6).                                
007100                                                                          
007200 01  W-KUNDREF                   PIC 9(10)   VALUE ZERO.                  
007300 01  FILLER                      REDEFINES W-KUNDREF.                     
007400     03  W-IDKUNDRF-RO           PIC 9(5).                                
007500     03  FILLER                  PIC 9(5).                                
007600                                                                          
007700 01  ARBETSFALT.                                                          
007800     03 WS-ADDISPABS.                                                     
007900        05 WS-ADDISPABS-START    PIC X(14)  VALUE                         
008000                                     'CARPARTS.VIPS.'.                    
008100        05 WS-ADDISPABS-SLUT     PIC X(10)  VALUE                         
008200                                     'DLEVRAD   '.                        
008300        05 FILLER                PIC X(26) VALUE SPACE.                   
008400                                                                          
008500     03 WS-SPAR-IDDISTR          PIC S9(5)   VALUE ZERO.                  
008600                                                                          
008700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008800 01  GENERELLA-SUBPROGRAM.                                                
008900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009400     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
009500     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
009600     EJECT                                                                
009700                                                                          
009800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009900*01 -COPY WMEDAREA                                                        
010000     SKIP3                                                                
010100 01  MESSAGE-CODES.                                                       
010200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010300     EJECT                                                                
010400*    --- AREOR FÖR ANROP FRÅN WZ01                                        
010500 01  FILLER                      PIC X(16)   VALUE 'WZ01-RECV '.          
010600*01  -COPY WZ01RECV                                                       
010700                                                                          
010800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010900*                                                                         
011000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011100*01 -COPY WMSGINIT                                                        
011200     EJECT                                                                
011300 01  MID-IO-AREA.                                                         
011400*    03  -COPY WMSGKOMI                                                   
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
011700*01  -COPY WMSGAREA                                                       
011800     EJECT                                                                
011900                                                                          
012000 01  WS-IDCOM                    PIC S9(9)   VALUE ZERO COMP-3.           
012100*                                                                         
012200*    ---- AREOR FÖR W006KOM SUBMODUL                                      
012300 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
012400 01  KOM-IO-AREA.                                                         
012500  03 KOM-AREA                    PIC X(2000) VALUE SPACE.                 
012600  03  ORAD     REDEFINES KOM-AREA.                                        
012700*  05      -COPY W4I25201   -PRE ORAD-                                    
012800     EJECT                                                                
012900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013000*                                                                         
013100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013200*01  MID -COPY W4I25701                                                   
013300     EJECT                                                                
013400                                                                          
013500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013600*                                                                         
013700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013800     SKIP3                                                                
013900 01  NYCKLAR-TILL-DLI.                                                    
014000     03  W-WDGX4251-X.                                                    
014100         05  FILLER              PIC X(4)    VALUE '4251'.                
014200         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
014300         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
014400         05  W-IDKUNDRF          PIC X(10)   VALUE SPACE.                 
014500         05  W-FILLER            PIC X(09)   VALUE LOW-VALUE.             
014600                                                                          
014700     03  W-WDGX4252-X.                                                    
014800         05  W-IDLOPNR           PIC S9(3)   VALUE ZERO COMP-3.           
014900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015000                                                                          
015100     03  W-IDGMTREF-X.                                                    
015200         05  W-IDDISTR-C         PIC S9(5)   VALUE ZERO COMP-3.           
015300         05  W-IDKUNDNR-C        PIC S9(7)   VALUE ZERO COMP-3.           
015400         05  W-IDKUNDRF-C        PIC X(10)   VALUE SPACE.                 
015500                                                                          
015600     03  W-IDORDER-X.                                                     
015700         05  W-IDORDER           PIC S9(7)   VALUE ZERO COMP-3.           
015800                                                                          
015900*    --- STATUS-KOD FRÅN IMS                                              
016000 01  STATUS-WS                   PIC XX.                                  
016100     88  SEGMENT-FINNS                       VALUE '  '.                  
016200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016500     SKIP2                                                                
016600 01  GODK-STATUSKODER.                                                    
016700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016800     SKIP3                                                                
016900 01  SSA1                        PIC X(64).                               
017000 01  SSA2                        PIC X(64).                               
017100     EJECT                                                                
017200*    --- IMS FUNKTIONSKODER                                               
017300*01  -COPY W0003                                                          
017400     EJECT                                                                
017500                                                                          
017600*    ---  DLI INPUT-OUTPUT AREA                                           
017700                                                                          
017800 01  FILLER                    PIC X(16) VALUE 'DLI-IO-4251'.             
017900 01  DLI-IO-4251.                                                         
018000*    03  -COPY WDGX4251                                                   
018100     EJECT                                                                
018200 01  FILLER                    PIC X(16) VALUE 'DLI-IO-4252'.             
018300 01  DLI-IO-4252.                                                         
018400*    03  -COPY WDGX4252                                                   
018500     EJECT                                                                
018600 01  FILLER                    PIC X(16) VALUE 'DLI-IO-Q201'.             
018700 01  DLI-IO-WDQ201.                                                       
018800*    03  -COPY WDQ201                                                     
018810 01  FILLER                    PIC X(16) VALUE 'DLI-IO-Q212'.             
018820 01  DLI-IO-WDQ212.                                                       
018830*    03  -COPY WDQ212                                                     
018900     EJECT                                                                
019000 LINKAGE SECTION.                                                         
019100                                                                          
019200 01  MSG-PCB        PIC X.                                                
019300                                                                          
019400 01  DISP-PCB                    PIC X.                                   
019500 01  KOMA-PCB                    PIC X.                                   
019600*01  -COPY W0008  -PRE 4251-                                              
019700     05  FILLER                  PIC X.                                   
019800     EJECT                                                                
019900*01  -COPY W0008  -PRE WDQ2C-                                             
020000     05  FILLER                  PIC X.                                   
020100     EJECT                                                                
020200 PROCEDURE DIVISION  USING         MSG-PCB  DISP-PCB                      
020300                                            KOMA-PCB                      
020400                                            4251-PCB                      
020500                                            WDQ2C-PCB.                    
020600 MAIN SECTION.                                                            
020700     ENTRY 'DLITCBL' USING         MSG-PCB  DISP-PCB                      
020800                                            KOMA-PCB                      
020900                                            4251-PCB                      
021000                                            WDQ2C-PCB.                    
021100                                                                          
021200     PERFORM A-INIT                                                       
021300     PERFORM UNTIL  RECV-KDRC > 0                                         
021400       PERFORM B-KOLLA-NYCKLAR                                            
021500       IF NYCKLAR-OK                                                      
021600                                                                          
021700        PERFORM IMS-GHU-WDGX4251                                          
021800        IF SEGMENT-FINNS                                                  
021900*        LÄS WDQ201  MED C-INDEX                                          
022000         PERFORM IMS-GHU-WDQ201                                           
022100         IF SEGMENT-SAKNAS                                                
022200           MOVE 'WDQ201 SAKNAS..'  TO FELTEXT                             
022300           CALL FELLOG                                                    
022400         END-IF                                                           
022500         MOVE NEJ                TO OHUV-FLKLAR                           
022600         PERFORM IMS-REPL-WDQ201                                          
022700                                                                          
022800         PERFORM IMS-GHNP-WDQ212                                          
022900         PERFORM UNTIL SEGMENT-SAKNAS                                     
023000            MOVE 'E'             TO ARB-KDORDSTA                          
023100            PERFORM IMS-REPL-WDQ212                                       
023200            PERFORM IMS-GHNP-WDQ212                                       
023300         END-PERFORM                                                      
023400                                                                          
023500         PERFORM IMS-GHNP-WDGX4252                                        
023600         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                     
023700           MOVE ZERO             TO ORAD-IX                               
023800           MOVE SPACE            TO KOM-AREA                              
023900                                                                          
024000       COMPUTE MSG-KVLL = LENGTH OF ORAD-MID-W4I25201 + 17                
024100                                                                          
024200           MOVE LOW-VALUE         TO MSG-KDZ1                             
024300           MOVE LOW-VALUE         TO MSG-KDZ2                             
024400           MOVE 'W4T252Y '        TO MSG-KDTRANS-1                        
024500           MOVE '4252'            TO MSG-IDTRANS-1                        
024600           MOVE '1'               TO MSG-KDMFSFOR-1                       
024700                                                                          
024800                                                                          
024900          MOVE OHUV-IDSYSTEM       TO ORAD-MID-IDSYSTEM                   
025000          MOVE MID-IDDISTR         TO ORAD-MID-IDDISTR                    
025100          MOVE MID-IDKUNDNR        TO ORAD-MID-IDKUNDNR                   
025200          MOVE MID-IDORDNR         TO ORAD-MID-IDORDNR                    
025300*         MOVE '0000000   '        TO ORAD-MID-IDKUNDRF-RO                
025400          MOVE SPACE               TO ORAD-MID-IDKUNDRF-RO                
025500          MOVE OHUV-BEKUNDRF       TO ORAD-MID-BEVOLREF                   
025600          MOVE SPACE               TO ORAD-MID-IDKLIENT                   
025700                                      ORAD-MID-IDARBREF                   
025800                                      ORAD-MID-IDVIN                      
025900          MOVE 'N'                 TO ORAD-MID-FLSLUT                     
026000                                                                          
026100          MOVE +1                  TO ORAD-IX                             
026200          PERFORM UNTIL ORAD-IX > ORAD-IX-MAX                             
026300            OR SEGMENT-SAKNAS OR SEGMENT-SLUT                             
026400             PERFORM S01-FLAGGOR                                          
026500             MOVE 4252-RADER    TO ORAD-MID-RADER (ORAD-IX)               
026600             PERFORM IMS-GHNP-WDGX4252                                    
026700             ADD 1              TO ORAD-IX                                
026800          END-PERFORM                                                     
026900                                                                          
027000          IF ORAD-IX > 0                                                  
027100             IF SEGMENT-SAKNAS OR SEGMENT-SLUT                            
027200                MOVE 'J'           TO ORAD-MID-FLSLUT                     
027300             END-IF                                                       
027400                                                                          
027500             MOVE KOM-AREA       TO MSG-INDATA-MINUS-1-TRANSKOD           
027600             CALL W006KOM USING MSG-PCB                                   
027700                                DISP-PCB                                  
027800                                KOMA-PCB                                  
027900                                MSG-KOM-WMSGKOMI                          
028000                                MSG-IO-AREA                               
028100          END-IF                                                          
028200          MOVE ZERO  TO ORAD-IX                                           
028300          MOVE SPACE TO ORAD-MID-W4I25201                                 
028400                                                                          
028500         END-PERFORM                                                      
028600         PERFORM IMS-GHU-WDGX4251                                         
028700         PERFORM IMS-DLET-WDGX4251                                        
028800         END-IF                                                           
028900                                                                          
029000        ELSE                                                              
029100         MOVE 'SEGMENT SAKNAS'   TO FELTEXT                               
029200         CALL ABEND USING RKOD-ABEND-MED-DUMP                             
029300        END-IF                                                            
029400*       MOVE 'TESTA'   TO FELTEXT                                         
029500*       CALL FELLOG                                                       
029600       PERFORM S12-RECV-MESSAGE                                           
029700     END-PERFORM                                                          
029800     IF RECV-KDRC > 1                                                     
029900       MOVE 'WZ01-RECV AVSLUTAS FEL..'  TO FELTEXT                        
030000       CALL FELLOG                                                        
030100     END-IF                                                               
030200*                                                                         
030300     PERFORM S13-RECV-CLOSE                                               
030400     MOVE ZERO TO RETURN-CODE                                             
030500     GOBACK                                                               
030600     .                                                                    
030700     EJECT                                                                
030800 A-INIT SECTION.                                                          
030900     MOVE 'A-INIT'              TO WS-SEKTION                             
031000                                                                          
031100     PERFORM S11-RECV-OPEN                                                
031200     PERFORM S12-RECV-MESSAGE                                             
031300                                                                          
031400     ACCEPT DAGENS-DATUM    FROM DATE                                     
031500     ACCEPT WS-TIKLOCK      FROM TIME                                     
031600     PERFORM S03-SKAPA-MSG-KOM-AREA                                       
031700     .                                                                    
031800     EJECT                                                                
031900 B-KOLLA-NYCKLAR SECTION.                                                 
032000     MOVE 'B-KOLLA-NYCKLAR'     TO WS-SEKTION                             
032200     MOVE JA                    TO NYCKLAR-SW                             
032300     IF MID-IDDISTR         = ALL '+'                                     
032400       MOVE NEJ TO NYCKLAR-SW                                             
032500     ELSE                                                                 
032600       IF MID-IDDISTR NUMERIC                                             
032700         MOVE MID-IDDISTR     TO W-IDDISTR                                
032800                                 W-IDDISTR-C                              
032900       ELSE                                                               
033000         MOVE NEJ TO NYCKLAR-SW                                           
033100       END-IF                                                             
033200     END-IF                                                               
033300                                                                          
033400     IF MID-IDKUNDNR        = ALL '+'                                     
033500       MOVE NEJ TO NYCKLAR-SW                                             
033600     ELSE                                                                 
033700       IF MID-IDKUNDNR NUMERIC                                            
033800         MOVE MID-IDKUNDNR    TO W-IDKUNDNR                               
033900                                 W-IDKUNDNR-C                             
034000       ELSE                                                               
034100         MOVE NEJ TO NYCKLAR-SW                                           
034200       END-IF                                                             
034300     END-IF                                                               
034400                                                                          
034500     IF MID-IDORDNR         = ALL '+'                                     
034600       MOVE NEJ TO NYCKLAR-SW                                             
034700     ELSE                                                                 
034800       IF MID-IDORDNR NUMERIC                                             
034900         MOVE MID-IDORDNR    TO W-IDKUNDRF(1:7)                           
035000                                W-IDKUNDRF-C(1:7)                         
035100       ELSE                                                               
035200         MOVE NEJ TO NYCKLAR-SW                                           
035300       END-IF                                                             
035400     END-IF                                                               
035500     MOVE SPACES              TO W-FILLER                                 
035600                                                                          
035700     IF NYCKLAR-FEL                                                       
035800       STRING 'NYCKLAR FEL '                                              
035900            DELIMITED BY SIZE INTO FELTEXT                                
036000       CALL FELLOG                                                        
036100                                                                          
036200     END-IF                                                               
036300     MOVE ZERO                  TO W-RAD                                  
036400     .                                                                    
036500     EJECT                                                                
036600 S01-FLAGGOR SECTION.                                                     
036700     IF 4252-FLRESTN = 'J' OR 'N' OR ' '                                  
036800       CONTINUE                                                           
036900     ELSE                                                                 
037000       MOVE SPACE                TO 4252-FLRESTN                          
037100     END-IF                                                               
037200     IF 4252-FLINVEST = 'J' OR 'N' OR ' '                                 
037300       CONTINUE                                                           
037400     ELSE                                                                 
037500       MOVE SPACE                TO 4252-FLINVEST                         
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 S03-SKAPA-MSG-KOM-AREA SECTION.                                          
038000                                                                          
038100     MOVE SPACE                  TO MSG-KOM-WMSGKOMI                      
038200     MOVE +54                    TO MSG-KOM-KVLL                          
038300     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
038400     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
038500     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
038600     MOVE 'W4I25201'             TO MSG-KOM-IDCPYTXT                      
038700     MOVE 'OC-DDGS'              TO MSG-KOM-IDSNDNOD                      
038800**   MOVE IN-OHUV-IDDISTR        TO MSG-KOM-IDSNDNOD (5:4)                
038900     MOVE 'W4025700'             TO MSG-KOM-IDSNDJOB                      
039000     MOVE DAGENS-DATUM           TO MSG-KOM-TIREGDAT                      
039100     MOVE WS-TIKLOCK             TO MSG-KOM-TIKLOCK                       
039200     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
039300     .                                                                    
039400     EJECT                                                                
039500 S11-RECV-OPEN SECTION.                                                   
039600     MOVE 'S11-RECV-OPEN'          TO WS-SEKTION                          
039700                                                                          
039800     MOVE 'OPEN'                   TO RECV-KDFUNC                         
039900     MOVE 'CARPARTS.PULS.DLEVRAD'  TO RECV-ADDISPABS                      
040000                                                                          
040100     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
040200                                   RECV-OPEN-AREA                         
040300                                                                          
040400     IF RECV-KDRC > 0                                                     
040500      MOVE RECV-KDRC               TO KDRC-DISP                           
040600      STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISP                         
040700        DELIMITED BY SIZE INTO FELTEXT                                    
040800      CALL FELLOG                                                         
040900     END-IF                                                               
041000     .                                                                    
041100     EJECT                                                                
041200 S12-RECV-MESSAGE SECTION.                                                
041300     MOVE 'S12-RECV-MESSAGE'      TO WS-SEKTION                           
041400                                                                          
041500     MOVE 'GET'                    TO RECV-KDFUNC                         
041600     MOVE LENGTH OF MID-W4I25701   TO RECV-KVDLEN                         
041700     CALL WZ01RECV USING RECV-CONTROL-AREA                                
041800                         RECV-KVDLEN                                      
041900                         MID-W4I25701                                     
042000*                                                                         
042100     IF RECV-KDRC > 1                                                     
042200       MOVE RECV-KDRC            TO KDRC-DISP                             
042300       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISP                        
042400         DELIMITED BY SIZE INTO FELTEXT                                   
042500       CALL FELLOG                                                        
042600     END-IF                                                               
042700     .                                                                    
042800     EJECT                                                                
042900 S13-RECV-CLOSE SECTION.                                                  
043000     MOVE 'S13-RECV-CLOSE'      TO WS-SEKTION                             
043100                                                                          
043200     MOVE 'CLOSE'                TO RECV-KDFUNC                           
043300     CALL WZ01RECV     USING        RECV-CONTROL-AREA                     
043400*                                                                         
043500     IF RECV-KDRC > 0                                                     
043600       MOVE RECV-KDRC            TO KDRC-DISP                             
043700       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISP                       
043800         DELIMITED BY SIZE INTO FELTEXT                                   
043900       CALL FELLOG                                                        
044000     END-IF                                                               
044100     .                                                                    
044200     EJECT                                                                
044300* --- IMS SEKTIONER ---                                                   
044400     SKIP3                                                                
044500 IMS-GHU-WDGX4251 SECTION.                                                
044600     MOVE 'IMS-GHU-WDGX4251'      TO WS-SEKTION                           
044700                                                                          
044800     STRING 'WDR501  (WDGXKEY  =' W-WDGX4251-X ')'                        
044900          DELIMITED BY SIZE INTO SSA1                                     
045000     MOVE '  GE'                TO GODK-STATUSKODER                       
045100     CALL CBLTDLI USING GHU 4251-PCB DLI-IO-4251 SSA1                     
045200     MOVE 4251-STATUS-CODE      TO STATUS-WS                              
045300     PERFORM IMS-STATUSKONTROLL                                           
045400     .                                                                    
045500     SKIP2                                                                
045600 IMS-GHNP-WDGX4252 SECTION.                                               
045700     MOVE 'IMS-GHNP-WDGX4252'    TO WS-SEKTION                            
045800                                                                          
045900     MOVE 'WDGX4252'            TO SSA1                                   
046000     MOVE '  GEGB'              TO GODK-STATUSKODER                       
046100     CALL CBLTDLI USING GHNP 4251-PCB DLI-IO-4252 SSA1                    
046200     MOVE 4251-STATUS-CODE      TO STATUS-WS                              
046300     PERFORM IMS-STATUSKONTROLL                                           
046400     .                                                                    
046500     SKIP2                                                                
046600 IMS-DLET-WDGX4251 SECTION.                                               
046700     MOVE 'IMS-DLET-WDGX4251'    TO WS-SEKTION                            
046800                                                                          
046900     MOVE '  '                TO GODK-STATUSKODER                         
047000     CALL CBLTDLI USING DLET 4251-PCB DLI-IO-4251                         
047100     MOVE 4251-STATUS-CODE    TO STATUS-WS                                
047200     PERFORM IMS-STATUSKONTROLL                                           
047300     .                                                                    
047400     SKIP2                                                                
047500 IMS-GHU-WDQ201 SECTION.                                                  
047600     MOVE 'IMS-GHU-WDQ201'    TO WS-SEKTION                               
047700                                                                          
047800     STRING 'WDQ201  (WDQ2CSEQ =' W-IDGMTREF-X ')'                        
047900          DELIMITED BY SIZE INTO SSA1                                     
048000     MOVE '  GE'               TO GODK-STATUSKODER                        
048100     CALL CBLTDLI USING GHU WDQ2C-PCB DLI-IO-WDQ201 SSA1                  
048200     MOVE WDQ2C-STATUS-CODE    TO STATUS-WS                               
048300     PERFORM IMS-STATUSKONTROLL                                           
048400     .                                                                    
048500     SKIP2                                                                
048600 IMS-REPL-WDQ201 SECTION.                                                 
048700     MOVE 'IMS-REPL-WDQ201'    TO WS-SEKTION                              
048800                                                                          
048900     MOVE '    '               TO GODK-STATUSKODER                        
049000     CALL CBLTDLI USING REPL WDQ2C-PCB DLI-IO-WDQ201                      
049100     MOVE WDQ2C-STATUS-CODE     TO STATUS-WS                              
049200     PERFORM IMS-STATUSKONTROLL                                           
049300     .                                                                    
049400     EJECT                                                                
049410 IMS-GHNP-WDQ212         SECTION.                                         
049411     MOVE 'IMS-GHNP-WDQ212'    TO WS-SEKTION                              
049420                                                                          
049430     MOVE 'WDQ212  '       TO SSA1                                        
049440     MOVE '  GE'           TO GODK-STATUSKODER                            
049450     CALL CBLTDLI USING GHNP WDQ2C-PCB DLI-IO-WDQ212 SSA1                 
049460     MOVE WDQ2C-STATUS-CODE TO STATUS-WS                                  
049470     PERFORM IMS-STATUSKONTROLL                                           
049480     .                                                                    
049490     SKIP3                                                                
049491 IMS-REPL-WDQ212 SECTION.                                                 
049492     MOVE 'IMS-REPL-WDQ212'    TO WS-SEKTION                              
049493                                                                          
049494     MOVE '    '               TO GODK-STATUSKODER                        
049495     CALL CBLTDLI USING REPL WDQ2C-PCB DLI-IO-WDQ212                      
049496     MOVE WDQ2C-STATUS-CODE     TO STATUS-WS                              
049497     PERFORM IMS-STATUSKONTROLL                                           
049498     .                                                                    
049499     EJECT                                                                
049500 IMS-STATUSKONTROLL SECTION.                                              
049600                                                                          
049700     SET STATUS-IX TO 1                                                   
049800     SEARCH GODK-STATUS                                                   
049900       AT END                                                             
050000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
050100         DELIMITED BY SIZE INTO FELTEXT                                   
050200         CALL FELLOG                                                      
050300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
050400         CONTINUE                                                         
050500     END-SEARCH                                                           
050600     .                                                                    

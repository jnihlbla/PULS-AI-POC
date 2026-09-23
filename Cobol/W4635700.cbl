000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W4635700.                                        
000300 AUTHOR.                 BO SVENSSON.                                     
000400 DATE-WRITTEN.           MARS 1999.                                       
000500                                                                          
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*            BMP, UPPLÄGGNING AV PACNINGSTRANSAR FÅN DIREKT-              
001000*            LEVERANTöR På DISPATCHERN.                                   
001100*            CHECKPOINT TAGES FÖR VARJE NY ORDER.                         
001200*                                                                         
001300*    DATABASER: UPPDATERAR   KOMMUNIKATIONS DB                            
001400*                            WLKOMA-(WDP8)                                
001500*               UPPDATERAR   HÄNDELSE REGISTER (CHKPOINT)                 
001600*                            WL4579-(WDGX)                                
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*            U0999      - FELLOG                                          
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*                  INFIL: GODKÄNDA POSTER                                 
002800     SELECT  W46357                   ASSIGN TO    W46357D1.              
002900     SKIP3                                                                
003000 DATA DIVISION.                                                           
003100                                                                          
003200 FILE SECTION.                                                            
003300                                                                          
003400 FD  W46357                                                               
003500     LABEL RECORD STANDARD                                                
003600     RECORDING      F                                                     
003700     BLOCK CONTAINS 0.                                                    
003800                                                                          
003900*01  -COPY W46357 -L.                                                     
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300*    -- CHECKED BY WY2000                                                 
004400 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4635700'.            
004500 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004600 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
004700 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
004800 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
004900 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
005000 77  CHKP-MAX                    PIC S9(4)   COMP SYNC VALUE +150.        
005010 77  CHKP-ANT                    PIC S9(4)   COMP SYNC VALUE +0.          
005100 77  IDEX                        PIC S9(4)   COMP SYNC VALUE +0.          
005200 77  W-ANT-POSTER-FORBI          PIC S9(7)   COMP-3.                      
005300 77  W-ANT-POSTER                PIC S9(7)   VALUE +0  COMP-3.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  W46357-EOF                  PIC X       VALUE 'N'.                   
005700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
005710 77  STOPP                       PIC X.                                   
005720     EJECT                                                                
005730                                                                          
005740 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005750 01  TIDPUNKT                    PIC 9(8)    VALUE ZERO.                  
005760*    ---- AREA FÖR INFIL W46357                                           
005770 01  FILLER                      PIC X(8)    VALUE 'IN-AREA'.             
005780 01  IN-AREA                     PIC X(2000).                             
005790*01  FILLER -COPY W46357 -PRE IN- -RED IN-AREA.                           
005800     EJECT                                                                
005900*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006200   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
006300   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006400   03  W006KOM                   PIC X(8)    VALUE 'W006KOM '.            
006500   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
006600     EJECT                                                                
006700*01  FILLER -COPY W0005       -PRE POSTSUM-.                              
006800     EJECT                                                                
006900*                                                                         
007000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007100     SKIP3                                                                
007200*    ---- STATUSKOD FRÅN IMS                                              
007300                                                                          
007400 01  STATUS-WS                   PIC XX.                                  
007500     88  IMS-EJ-OK                           VALUE 'XD'.                  
007600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
007700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007800     SKIP3                                                                
007900 01  GODK-STATUSKODER.                                                    
008000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
008100     SKIP3                                                                
008200 01  SSA1                        PIC X(64).                               
008300 01  SSA2                        PIC X(64).                               
008400     EJECT                                                                
008500 01  FILLER                  PIC X(16)   VALUE 'NYCKLAR-TILL-DLI'.        
008600 01  NYCKLAR-TILL-DLI.                                                    
008700     03  W-WDGXKEY-X.                                                     
008800         05  W-IDHTYP            PIC X(4)     VALUE '4579'.               
008900         05  W-IDPGM             PIC X(8)     VALUE 'W4635700'.           
009000         05  W-LOW-VALUE         PIC X(18)    VALUE LOW-VALUE.            
009100                                                                          
009200     SKIP3                                                                
009300*01  -COPY WDGX01                                                         
009400     EJECT                                                                
009500*    IMS FUNKTIONSKODER                                                   
009600*    -COPY W0003                                                          
009700     EJECT                                                                
009800 01  FILLER                  PIC X(16)   VALUE '4579-IO-AREA'.            
009900 01  4579-IO-AREA.                                                        
010000*03  FILLER  -COPY WDGX4579                                               
010100     EJECT                                                                
010200 01  FILLER                  PIC X(16)   VALUE '4580-IO-AREA'.            
010300 01  4580-IO-AREA.                                                        
010400*03  FILLER  -COPY WDGX4580                                               
010500     EJECT                                                                
010600 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
010700*01  -COPY WMSGKOM                                                        
010800     EJECT                                                                
010900                                                                          
011000 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
011100     SKIP3                                                                
011200*01  -COPY WMSGAREA                                                       
011300     EJECT                                                                
011400*                                                                         
011500*    --- AREOR FÖR W006KOM SUBMODUL                                       
011600*                                                                         
011700 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
011800 01  KOM-IO-AREA.                                                         
011900   03  KOM-AREA                     PIC X(1000) VALUE SPACE.              
012000*03  FILLER  -COPY W4I39001      -RED KOM-AREA.                           
012100     EJECT                                                                
012200                                                                          
012300 LINKAGE SECTION.                                                         
012400*01  -COPY W0009         -PRE MSG-                                        
012500     SKIP3                                                                
012600 01  DISP-PCB                PIC X.                                       
012700 01  KOMA-PCB                PIC X.                                       
012800     EJECT                                                                
012900*01  -COPY W0008         -PRE 4579-                                       
013000     05  FILLER              PIC X.                                       
013100     EJECT                                                                
013200 PROCEDURE DIVISION  USING                                                
013300                     MSG-PCB                                              
013400                     DISP-PCB                                             
013500                     KOMA-PCB                                             
013600                     4579-PCB.                                            
013700     ENTRY 'DLITCBL' USING                                                
013800                     MSG-PCB                                              
013900                     DISP-PCB                                             
014000                     KOMA-PCB                                             
014100                     4579-PCB.                                            
014200                                                                          
014300     PERFORM A-INITIERA                                                   
014400     PERFORM IMS-RESTART                                                  
014500     PERFORM IMS-LAS-ATERSTART                                            
014600                                                                          
014700     IF SEGMENT-SAKNAS                                                    
014800        MOVE SPACE        TO 4580-WDGX4580-CTX                            
014900        MOVE '1'          TO 4580-KDSEGKEY                                
015000        MOVE ZERO         TO 4580-KVPOST                                  
015100        MOVE DAGENS-DATUM TO 4580-TIUPPDAT                                
015200        MOVE TIDPUNKT     TO 4580-TIUPPTID                                
015300                                                                          
015400        PERFORM IMS-ISRT-ATERSTART                                        
015500        PERFORM IMS-LAS-ATERSTART                                         
015600     END-IF                                                               
015700                                                                          
015800     IF 4580-KVPOST > +0                                                  
015900        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
016000     ELSE                                                                 
016100        PERFORM S01-LAS-W46357                                            
016200     END-IF                                                               
016300                                                                          
016400     PERFORM UNTIL W46357-EOF = JA                                        
016500                                                                          
016600        PERFORM C-BEARBETA                                                
016700                                                                          
016800        IF  CHKP-ANT > CHKP-MAX                                           
016900         OR IN-FLSLUT-VORD = JA                                           
017000            PERFORM C-TAG-CHECKPOINT                                      
017100            ADD  +1 TO TIDPUNKT                                           
017200        END-IF                                                            
017300                                                                          
017400        PERFORM S01-LAS-W46357                                            
017500     END-PERFORM                                                          
017600                                                                          
017700     PERFORM Z-FINIT                                                      
017800     MOVE ZERO TO RETURN-CODE                                             
017900     GOBACK                                                               
018000     .                                                                    
018100     EJECT                                                                
018200                                                                          
018300 A-INITIERA SECTION.                                                      
018400     SKIP2                                                                
018500     OPEN INPUT W46357                                                    
018600                                                                          
018700     ACCEPT DAGENS-DATUM  FROM DATE                                       
018800     ACCEPT TIDPUNKT      FROM TIME                                       
018900     MOVE SPACE                TO MSG-AREA                                
019000                                                                          
019100     MOVE PROGRAM-NAMN         TO POSTSUM-PROGNAMN                        
019200     .                                                                    
019300     EJECT                                                                
019400 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
019500                                                                          
019600     MOVE +0                   TO W-ANT-POSTER-FORBI                      
019700     PERFORM S01-LAS-W46357                                               
019800                                                                          
019900     PERFORM UNTIL W46357-EOF     = JA                                    
020000                OR W-ANT-POSTER-FORBI = 4580-KVPOST                       
020100        PERFORM S01-LAS-W46357                                            
020200        ADD +1  TO W-ANT-POSTER-FORBI                                     
020300     END-PERFORM                                                          
020400                                                                          
020500     IF W46357-EOF = JA                                                   
020600        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
020700                      TO FELTEXT                                          
020800        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
020900     END-IF                                                               
021000     .                                                                    
021100     EJECT                                                                
021200                                                                          
021300 C-BEARBETA SECTION.                                                      
021400                                                                          
021500     MOVE 'N'                    TO STOPP                                 
021600     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
021700     MOVE +54                    TO MSG-KOM-KVLL                          
021800     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
021900     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
022000     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
022100     MOVE 'W46357'               TO MSG-KOM-IDCPYTXT                      
022200     MOVE IN-IDSNDNOD            TO MSG-KOM-IDSNDNOD                      
022300     MOVE 'W4635700'             TO MSG-KOM-IDSNDJOB                      
022400     MOVE DAGENS-DATUM           TO MSG-KOM-TIREGDAT                      
022600     MOVE TIDPUNKT               TO MSG-KOM-TIKLOCK                       
022700     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
022800                                                                          
022900     MOVE SPACE                  TO KOM-AREA                              
023000     MOVE +990                   TO MSG-KVLL                              
023100     MOVE LOW-VALUE              TO MSG-KDZ1                              
023200     MOVE LOW-VALUE              TO MSG-KDZ2                              
023300     MOVE 'W4T390X '             TO MSG-KDTRANS-1                         
023400     MOVE '4390'                 TO MSG-IDTRANS-1                         
023500     MOVE '2'                    TO MSG-KDMFSFOR-1                        
023600                                                                          
023700     MOVE IN-IDANSTNR            TO MID-IDANSTNR                          
023800     MOVE IN-IDDISTR             TO MID-IDDISTR                           
023900     MOVE IN-IDKUNDNR            TO MID-IDKUNDNR                          
024000     MOVE IN-IDORDNR             TO MID-IDORDNR                           
024100     MOVE IN-IDPRODNR            TO MID-IDPRODNR                          
024200     MOVE IN-IDDC                TO MID-IDDC                              
024300     MOVE IN-IDKOLLI             TO MID-IDKOLLI                           
024400     MOVE IN-IDSUPREF            TO MID-IDSUPREF                          
024500     MOVE IN-DASUPREF            TO MID-DASUPREF                          
024600     MOVE IN-TISUPTID            TO MID-TISUPTID                          
024700     MOVE IN-VKORDBTO-KOLLI      TO MID-VKORDBTO-KOLLI                    
024800     MOVE IN-KDEMBTYP            TO MID-KDEMBTYP                          
024900     MOVE IN-DIKOLLIL            TO MID-DIKOLLIL                          
025000     MOVE IN-DIKOLLIB            TO MID-DIKOLLIB                          
025100     MOVE IN-DIKOLLIH            TO MID-DIKOLLIH                          
025200     MOVE IN-FLSLUT              TO MID-FLSLUT                            
025300     MOVE IN-FLSLUT-VORD         TO MID-FLSLUT-VORD                       
025400     MOVE IN-VLORDBTO-KOLLI      TO MID-VLORDBTO-KOLLI                    
025500                                                                          
025600     MOVE +1                     TO IDEX                                  
025700     PERFORM UNTIL IDEX > 74                                              
025800         IF IN-KVLEVART (IDEX) = 0 AND IN-IDKOLLI > 0                     
025900           DISPLAY '** ANTAL = 0 '  IN-IDPRODNR  ' ' IN-IDKOLLI           
026000           MOVE 'J' TO STOPP                                              
026100         ELSE                                                             
026200           MOVE IN-IDRADNR (IDEX)  TO MID-IDRADNR (IDEX)                  
026300           MOVE IN-KVLEVART (IDEX) TO MID-KVLEVART (IDEX)                 
026400           MOVE IN-KDARTURS (IDEX) TO MID-KDARTURS (IDEX)                 
026500         END-IF                                                           
026600         ADD +1                  TO IDEX                                  
026700     END-PERFORM                                                          
026800                                                                          
026900     IF STOPP = 'N'                                                       
026910     ADD +1 TO CHKP-ANT                                                   
027000     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
027100     CALL W006KOM USING MSG-PCB                                           
027200                        DISP-PCB                                          
027300                        KOMA-PCB                                          
027400                        MSG-KOM-WMSGKOM                                   
027500                        MSG-IO-AREA                                       
027600     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
027700*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
027800*       DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                 
027900        MOVE ' FELAKTIG DATUM,TID PÅ INPUTFIL W46357 '                    
028000                      TO FELTEXT                                          
028100        DISPLAY ' FELAKTIG DATUM,TID INPUTFIL W46357 '                    
028200        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
028300     END-IF                                                               
028400     END-IF                                                               
028500                                                                          
028600     MOVE SPACE       TO KOM-AREA                                         
028800     .                                                                    
028900     EJECT                                                                
029000                                                                          
029100 C-TAG-CHECKPOINT SECTION.                                                
029200     SKIP2                                                                
029300*    UPPDATERA ÅTERSTARTREGISTRET                                         
029400     PERFORM IMS-LAS-ATERSTART                                            
029500     MOVE W-ANT-POSTER   TO 4580-KVPOST                                   
029700     ACCEPT 4580-TIUPPDAT FROM DATE                                       
029800     ACCEPT 4580-TIUPPTID FROM TIME                                       
029900                                                                          
030000     PERFORM IMS-REPL-ATERSTART                                           
030100                                                                          
030200*    TAG CHECKPOINT                                                       
030300     PERFORM IMS-CHECKPOINT                                               
030310     MOVE +0 TO CHKP-ANT                                                  
030400     .                                                                    
030500     EJECT                                                                
030600                                                                          
030700 Z-FINIT    SECTION.                                                      
030800     SKIP2                                                                
030900                                                                          
031000     CLOSE  W46357                                                        
031100                                                                          
031200*    NOLLA ÅTERSTARTINFORMATIONEN                                         
031300     PERFORM IMS-LAS-ATERSTART                                            
031400     MOVE +0                   TO 4580-KVPOST                             
031500     ACCEPT 4580-TIUPPDAT FROM DATE                                       
031600     ACCEPT 4580-TIUPPTID FROM TIME                                       
031700                                                                          
031800     PERFORM IMS-REPL-ATERSTART                                           
031900                                                                          
032000     MOVE 'S'      TO POSTSUM-OPKOD                                       
032100     CALL POSTSUM USING POSTSUM-PARM                                      
032200     .                                                                    
032300     EJECT                                                                
032400                                                                          
032500 S01-LAS-W46357 SECTION.                                                  
032600     SKIP2                                                                
032700     READ W46357 INTO IN-AREA                                             
032800       AT END                                                             
032900          MOVE JA TO W46357-EOF                                           
033000     END-READ                                                             
033100                                                                          
033200     IF W46357-EOF = NEJ                                                  
033300        MOVE 'W46357'       TO POSTSUM-FDNAMN                             
033400        MOVE 'W46357D1'     TO POSTSUM-DDNAMN2                            
033500        MOVE SPACE          TO POSTSUM-TRANSTYP                           
033501        CALL POSTSUM USING POSTSUM-PARM                                   
033502        ADD  +1  TO W-ANT-POSTER                                          
033508     END-IF                                                               
033509     .                                                                    
033510                                                                          
033511     EJECT                                                                
033512                                                                          
033513* IMS SECTIONER                                                           
033514     SKIP3                                                                
033515                                                                          
033520 IMS-RESTART SECTION.                                                     
033530     SKIP2                                                                
033540     MOVE SPACE TO MSG-IO-AREA-1                                          
033550     MOVE '  ' TO GODK-STATUSKODER                                        
033560     CALL CBLTDLI USING XRST MSG-PCB                                      
033570                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
033580                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
033590     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
033600     PERFORM IMS-STATUSKONTROLL                                           
033700     .                                                                    
033800                                                                          
033900 IMS-CHECKPOINT SECTION.                                                  
034000     MOVE PROGRAM-NAMN TO MSG-IO-AREA-1                                   
034100     MOVE '  XD'       TO GODK-STATUSKODER                                
034200     CALL CBLTDLI USING CHKP MSG-PCB                                      
034300                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
034400                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
034500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034600     PERFORM IMS-STATUSKONTROLL                                           
034700     IF IMS-EJ-OK                                                         
034800       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
034900       MOVE ' IMS-KONTROLLREGION EJ TILLGÄNGLIG '                         
035000                            TO FELTEXT                                    
035100       CALL FELLOG                                                        
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 IMS-LAS-ATERSTART SECTION.                                               
035600     SKIP2                                                                
035700     MOVE '4579'         TO IDHTYP                                        
035800     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
035900     MOVE 'W4635700'     TO NYCKEL-VALFRI(1:8)                            
036000     STRING 'WL457901(WDGXKEY  =' WDGX01 ')'                              
036100                    DELIMITED BY SIZE INTO SSA1                           
036200     MOVE 'WL457911 '    TO SSA2                                          
036300     MOVE '  GE'           TO GODK-STATUSKODER                            
036400     CALL CBLTDLI USING GHU 4579-PCB 4580-IO-AREA SSA1 SSA2               
036500     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
036600     PERFORM IMS-STATUSKONTROLL                                           
036700     .                                                                    
036800                                                                          
036900 IMS-ISRT-ATERSTART SECTION.                                              
037000     SKIP2                                                                
037100     MOVE '4579'         TO IDHTYP                                        
037200     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
037300     MOVE 'W4635700'     TO NYCKEL-VALFRI(1:8)                            
037400     STRING 'WL457901(WDGXKEY  =' WDGX01 ')'                              
037500                    DELIMITED BY SIZE INTO SSA1                           
037600     MOVE 'WL457911 '    TO SSA2                                          
037700     MOVE '  '           TO GODK-STATUSKODER                              
037800     CALL CBLTDLI USING ISRT 4579-PCB 4580-IO-AREA SSA1 SSA2              
037900     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
038000     PERFORM IMS-STATUSKONTROLL                                           
038100     .                                                                    
038200                                                                          
038300 IMS-REPL-ATERSTART SECTION.                                              
038400     SKIP2                                                                
038500     MOVE '  '             TO GODK-STATUSKODER                            
038600     CALL CBLTDLI USING REPL 4579-PCB 4580-IO-AREA                        
038700     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
038800     PERFORM IMS-STATUSKONTROLL                                           
038900     .                                                                    
039000                                                                          
039100 IMS-STATUSKONTROLL SECTION.                                              
039200     SET STATUS-IX TO 1                                                   
039300     SEARCH GODK-STATUS                                                   
039400       AT END                                                             
039500         MOVE ' STATUSKOD FRÅN IMS EJ TILLÅTEN '                          
039600                            TO FELTEXT                                    
039700         CALL FELLOG                                                      
039800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
039900         CONTINUE                                                         
040000     END-SEARCH                                                           
040100     .                                                                    

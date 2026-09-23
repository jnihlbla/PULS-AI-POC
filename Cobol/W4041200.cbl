000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4041200.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   96/04/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KUNDREGISTER  DISTRIKT INFORMATION.                              
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WLGMTA (WDB2)                              
001100*        PROGRAMMET LÄSER      WLGMTB (WDB3)                              
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W4T412                                              
001500*        MID:         W4I41201                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W4O41201                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W4041200'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  YES                         PIC X       VALUE 'Y'.                   
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600 77  IDEX                        PIC S9(2)   COMP-3 VALUE ZERO.           
003700 77  IDEX-TVSVOR-MAX             PIC S9(2)   COMP-3 VALUE +16.            
003710 77  IDEX-PREPLAN-MAX            PIC S9(2)   COMP-3 VALUE +8.             
003800************* SPAR FÄLT FÖR UPPDATERING AV DISTR-INFO                     
003900                                                                          
004000 77  WS-FLAUTORD                 PIC X(1)    VALUE SPACE.                 
004100 77  WS-IDRFTAB                  PIC X(3)    VALUE SPACE.                 
004200 77  WS-FLNC                     PIC X(1)    VALUE SPACE.                 
004300 77  WS-FLVR                     PIC X(1)    VALUE SPACE.                 
004400 77  WS-KDORDING                 PIC 9(1)    VALUE ZERO.                  
004500 77  WS-KVVECKOR-OB              PIC 9(3)    VALUE ZERO.                  
004600 77  WS-FLURSRAP                 PIC X(1)    VALUE SPACE.                 
004700 77  WS-FLSAMFAK                 PIC X(1)    VALUE SPACE.                 
004710 77  WS-FLSWCONS                 PIC X(1)    VALUE SPACE.                 
004800 77  WS-FLPRERS                  PIC X(1)    VALUE SPACE.                 
004810 77  WS-RELANDCO-EXP             PIC 9(3)V9(1) VALUE ZERO.                
004820 77  WS-DIST-WARNING             PIC X(43)   VALUE                        
004830                  'CHANGES WILL AFFECT THE COMPLETE DISTRICT!'.           
005100                                                                          
005200************* SPAR FÄLT FÖR UPPDATERING AV SAMFAK-INFO                    
005300                                                                          
005400 77  WS-FLFAKVKT                 PIC X(1)    VALUE SPACE.                 
005500 77  WS-REAVDRAG                 PIC 9(2)V9(1) VALUE ZERO.                
005600 77  WS-FLFAKURS                 PIC X(1)    VALUE SPACE.                 
005700 77  WS-REEMBHNT                 PIC 9(2)V9(1) VALUE ZERO.                
005800 77  WS-KDSTATNR                 PIC 9(1)    VALUE ZERO.                  
005900 77  WS-KDHBLKRV                 PIC 9(1)    VALUE ZERO.                  
006000 77  WS-KDSPRAK                  PIC 9(1)    VALUE ZERO.                  
006100 77  WS-IDPARTNR                 PIC X(9)    VALUE SPACE.                 
006200 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
006300                                                                          
006310*01  -COPY WWDCKONS                                                       
006400                                                                          
006410*01  -COPY WWIDFTG                                                        
006420                                                                          
006500 77  SPAR-IDKUNDNR               PIC 9(7)    VALUE ZERO.                  
006501 77  SPAR-IDKUNDNR-VTRANS        PIC 9(7)    VALUE ZERO.                  
006510 77  SPAR-FLSAMFAK               PIC X(1)    VALUE SPACE.                 
006600                                                                          
006700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006800                                                                          
006900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
007000     88  ALLT-OK                             VALUE 'J'.                   
007100                                                                          
007200 77  BETALAR-SW                  PIC X       VALUE 'J'.                   
007300     88  BETALARE-AENDRAD                    VALUE 'J'.                   
007400     88  BETALARE-EJ-AENDRAD                 VALUE 'N'.                   
007500                                                                          
007510 77  FTG-SW                      PIC X       VALUE 'J'.                   
007520     88  FTG-AENDRAD                         VALUE 'J'.                   
007530     88  FTG-EJ-AENDRAD                      VALUE 'N'.                   
007540                                                                          
007600 77  LEVNR-SW                    PIC X       VALUE 'J'.                   
007700     88  LEVNR-AENDRAD                       VALUE 'J'.                   
007800     88  LEVNR-EJ-AENDRAD                    VALUE 'N'.                   
007900                                                                          
008000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008100     88  INDATA-OK                           VALUE 'J'.                   
008200     88  INDATA-FEL                          VALUE 'N'.                   
008300                                                                          
008400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008500     88  NYCKLAR-OK                          VALUE 'J'.                   
008600     88  NYCKLAR-FEL                         VALUE 'N'.                   
008700                                                                          
008800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008900     88  EGEN-MID                            VALUE '4412'.                
009000     88  GODK-MID                            VALUE '4411' '4412'          
009100                                                   '4413' '4414'          
009200                                                   '4415' '4416'          
009300                                                   '4417' '4418'          
009400                                                   '4419'.                
009500     88  HELP-MID                            VALUE '0551'.                
009600     SKIP3                                                                
009700 77  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
009800     88  GODK-IDSKYLT                        VALUE 'D  ' 'E  '            
009900                                                   'F  ' 'GB '            
010000                                                   'I  ' 'NL '            
010100                                                   'P  ' 'S  '            
010200                                                   'SF ' 'USA'.           
010300     EJECT                                                                
010400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010500 01  GENERELLA-SUBPROGRAM.                                                
010600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
011100     EJECT                                                                
011200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011300*01 -COPY WMEDAREA                                                        
011400     SKIP3                                                                
011500 01  MESSAGE-CODES.                                                       
011600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
011700     03  ERR-RAT-FACT-MISS       PIC X(3)    VALUE '041'.                 
011800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
011900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
012010     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
012100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012200     EJECT                                                                
012300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012400*                                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012600     SKIP3                                                                
012700*01 -COPY WMSGINIT                                                        
012800     SKIP3                                                                
012900*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
013000*                                                                         
013100 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
013200     SKIP3                                                                
013300*01 -COPY WDECAREA                                                        
013400     SKIP3                                                                
013500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013600*                                                                         
013700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013800     SKIP3                                                                
013900*01  MID -COPY W4I41201                                                   
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014200     SKIP3                                                                
014300*01  -COPY WMSGAREA                                                       
014400     EJECT                                                                
014500     03  MOD REDEFINES MSG-AREA.                                          
014600*      05  -COPY W4O41201                                                 
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014900     SKIP3                                                                
015000*01  -COPY WMFSAREA                                                       
015100     EJECT                                                                
015200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015300*                                                                         
015301     EJECT                                                                
015302*01 -COPY WWDC03                                                          
015303*                                                                         
015304 01  TEST-IDDISTR                PIC 9(5)    COMP-3 VALUE ZERO.           
015310*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
015320     EJECT                                                                
015400                                                                          
015500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015600     SKIP3                                                                
015700 01  NYCKLAR-TILL-DLI.                                                    
015800     03  W-IDGMT-X.                                                       
015900         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
016000         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
016100     SKIP2                                                                
016200     03  W-IDGMT-MIN-X.                                                   
016300         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
016400         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
016500     SKIP2                                                                
016600     03  W-IDGMT-MAX-X.                                                   
016700         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
016800         05  W-IDKUNDNR-MAX      PIC S9(7)  VALUE +9999999 COMP-3.        
016900     SKIP2                                                                
017000     03  W-WDB101KY-X.                                                    
017100         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
017110         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
017200                                                                          
017300     03  W-WDGX4455-X.                                                    
017400         05  W-IDHTYP            PIC X(4)    VALUE '4455'.                
017500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
017600                                                                          
017700     03  W-WDGX4456-X.                                                    
017800         05  W-IDRFTAB           PIC X(3)    VALUE SPACE.                 
017900         05  FILLER              PIC X(7)    VALUE LOW-VALUE.             
018000                                                                          
018100     03  W-IDLEVNR-X             PIC  X(5)   VALUE SPACE.                 
018200                                                                          
018300     03  W-WDB301KY-DEF-X.                                                
018400         05  W-IDDC-DEF          PIC  X(2)   VALUE SPACE.                 
018500         05  W-IDDISTR-DC-DEF    PIC S9(5)   VALUE ZERO COMP-3.           
018600         05  W-IDKUNDNR-DC-DEF   PIC S9(7)  VALUE +9999999 COMP-3.        
018700                                                                          
018800     03  W-IDDC-B6-X.                                                     
018900         05 W-IDDC-B6            PIC X(2).                                
019000                                                                          
019010     03  W-IDDC-X.                                                        
019020         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
019030     03  W-KDSEGKEY-X.                                                    
019040         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
019100                                                                          
019200*    --- STATUS-KOD FRÅN IMS                                              
019300 01  STATUS-WS                   PIC XX.                                  
019400     88  SEGMENT-FINNS                       VALUE '  '.                  
019500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019700     88  BAS-SLUT                            VALUE 'GB'.                  
019800     SKIP2                                                                
019900 01  GODK-STATUSKODER.                                                    
020000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020100     SKIP3                                                                
020200 01  SSA1                        PIC X(64).                               
020300 01  SSA2                        PIC X(64).                               
020400     EJECT                                                                
020500*    --- IMS FUNKTIONSKODER                                               
020600*01  -COPY W0003                                                          
020700     EJECT                                                                
020800*    ---  DLI INPUT-OUTPUT AREA                                           
020900 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
021000 01  DLI-IO-WDB2.                                                         
021100*    03  -COPY WDB201  -PRE GMTA-                                         
021200     EJECT                                                                
021300                                                                          
021400 01  DLI-IO-AREA-XXKM.                                                    
021500*    03  -COPY WDGX4456 -PRE XXKM-                                        
021600     EJECT                                                                
021700                                                                          
021800 01  DLI-IO-AREA-WDB1.                                                    
021900*    03  -COPY WDB101  -PRE BETC-                                         
022000     EJECT                                                                
022100                                                                          
022200 01  DLI-IO-AREA-WDB2-NEXT.                                               
022300*    03  -COPY WDB201  -PRE NEXT-                                         
022400     EJECT                                                                
022500 01  FILLER                      PIC X(16)   VALUE 'WDF101-AREA'.         
022600 01  DLI-IO-AREA-WDF1.                                                    
022700*    03  -COPY WDF101  -PRE LEVA-                                         
022800     EJECT                                                                
022900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-B3'.        
023000     SKIP3                                                                
023100 01  DLI-IO-AREA-B3.                                                      
023200*    03  -COPY WDB301                                                     
023300                                                                          
023400 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
023500 01   DLI-IO-AREA-B601.                                                   
023600*     03  -COPY WDB601                                                    
023700     EJECT                                                                
023710 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB617'.                      
023720 01  DLI-IO-WDB617.                                                       
023730*    03  -COPY WDB617                                                     
023740     EJECT                                                                
023800                                                                          
023900 LINKAGE SECTION.                                                         
024000*01  -COPY W0009   -PRE MSG-                                              
024100*01  -COPY W0008   -PRE USEA-                                             
024200     05  FILLER                  PIC X.                                   
024300     EJECT                                                                
024400*01  -COPY W0008  -PRE GMTA-                                              
024500     05  FILLER                  PIC X.                                   
024600     EJECT                                                                
024700*01  -COPY W0008  -PRE XXKM-                                              
024800     05  FILLER                  PIC X.                                   
024900     EJECT                                                                
025000*01  -COPY W0008  -PRE BETC-                                              
025100     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300*01  -COPY W0008  -PRE LEVA-                                              
025400     05  FILLER                  PIC X.                                   
025500     EJECT                                                                
025600*01  -COPY W0008 -PRE GMTB-                                               
025700     05  FILLER                  PIC X.                                   
025800     EJECT                                                                
025900*01  -COPY W0008 -PRE WDB6-                                               
026000     05  FILLER                  PIC X.                                   
026100     EJECT                                                                
026200 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB GMTA-PCB XXKM-PCB             
026300                           BETC-PCB LEVA-PCB GMTB-PCB WDB6-PCB.           
026400 MAIN SECTION.                                                            
026500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB GMTA-PCB XXKM-PCB             
026600                           BETC-PCB LEVA-PCB GMTB-PCB WDB6-PCB.           
026700                                                                          
026800     PERFORM IMS-GET-MSG                                                  
026900     IF SEGMENT-FINNS                                                     
027000       PERFORM A-INIT                                                     
027100       PERFORM B-KOLLA-NYCKLAR                                            
027200       IF NYCKLAR-OK                                                      
027300         IF MFS-UPDATE OR MFS-UPD-V                                       
027400           PERFORM G-KOLLA-INPUT                                          
027500           IF INDATA-OK                                                   
027600             PERFORM H-UPPDATERA                                          
027700           END-IF                                                         
027800         ELSE                                                             
027900           IF MFS-FIRST                                                   
028000             PERFORM C-FOERSTA-SIDA                                       
028100           ELSE                                                           
028200             PERFORM E-SAMMA-SIDA                                         
028300           END-IF                                                         
028400         END-IF                                                           
028500         IF ALLT-OK                                                       
028600           PERFORM F-LAES-VISA-INFO                                       
028700         END-IF                                                           
028800       END-IF                                                             
028900*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
029000*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
029100       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O41201 + 4                      
029200       PERFORM IMS-INSERT-MSG                                             
029300     END-IF                                                               
029400                                                                          
029500     MOVE ZERO TO RETURN-CODE                                             
029600     GOBACK                                                               
029700     .                                                                    
029800     EJECT                                                                
029900 A-INIT SECTION.                                                          
030000                                                                          
030100     IF MSG-DUBBLA-TRANSKODER                                             
030200       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I41201                 
030300       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
030400       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
030500     ELSE                                                                 
030600       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I41201                  
030700       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
030800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030900     END-IF                                                               
031000                                                                          
031100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
031300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
031400                                                                          
031500     MOVE LOW-VALUE TO MSG-AREA                                           
031600     MOVE 'W4O412N1' TO MFS-IDMOD                                         
031700     MOVE '4412' TO MOD-IDTRANS                                           
031800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
031900                                                                          
032000     IF EGEN-MID OR HELP-MID                                              
032100       CONTINUE                                                           
032200     ELSE                                                                 
032300       MOVE SPACE TO MFS-KDTRTYP                                          
032400       MOVE '7' TO MFS-IDPFK                                              
032500     END-IF                                                               
032600                                                                          
032700     MOVE 'GB' TO MED-IDSKYLT                                             
032800     .                                                                    
032900     EJECT                                                                
033000 B-KOLLA-NYCKLAR SECTION.                                                 
033100                                                                          
033200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
033300     MOVE '001'             TO MSGI-KDCALL                                
033400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
033500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
033600     MOVE '4412'            TO MSGI-IDTRANS                               
033700     IF EGEN-MID                                                          
033800       MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                            
033900       MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                           
034000     END-IF                                                               
034100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
034200     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
034300                                                                          
034400     MOVE JA TO NYCKLAR-SW                                                
034500                                                                          
034600                                                                          
034700*    -- KONTROLL AV IDDISTR                                               
034800     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
034900                                                                          
035000     IF MID-IDDISTR-IN NOT = ALL '+'                                      
035100       MOVE '7'         TO MFS-IDPFK                                      
035200       MOVE SPACE       TO MFS-KDTRTYP                                    
035300     END-IF                                                               
035400     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
035500     IF MSGI-IDDISTR NUMERIC                                              
035600       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
035700                            W-IDDISTR-MIN                                 
035800                            W-IDDISTR-MAX                                 
035810                            TEST-IDDISTR                                  
035900     ELSE                                                                 
036000       MOVE NEJ TO NYCKLAR-SW                                             
036100     END-IF                                                               
036200                                                                          
036300*    -- KONTROLL AV IDKUNDNR                                              
036400     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
036500                                                                          
036600     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
036700       MOVE '7'         TO MFS-IDPFK                                      
036800       MOVE SPACE       TO MFS-KDTRTYP                                    
036900     END-IF                                                               
037000     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
037100     IF MSGI-IDKUNDNR NUMERIC                                             
037200       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
037300     ELSE                                                                 
037400       MOVE NEJ TO NYCKLAR-SW                                             
037500     END-IF                                                               
037600                                                                          
037610*    -- CHECK OF FLDISTCUST                                               
037630                                                                          
037640     IF MID-FLDISTCUST-IN NOT =  '+'                                      
037641       MOVE '7'         TO MFS-IDPFK                                      
037642       MOVE SPACE       TO MFS-KDTRTYP                                    
037643     ELSE                                                                 
037644       IF MID-FLDISTCUST-UT = '+'                                         
037645          MOVE 'D'               TO MID-FLDISTCUST-IN                     
037646       ELSE                                                               
037647          MOVE MID-FLDISTCUST-UT TO MID-FLDISTCUST-IN                     
037648       END-IF                                                             
037670     END-IF                                                               
037690     IF MID-FLDISTCUST-IN = 'D' OR 'C'                                    
037691        IF MID-FLDISTCUST-IN = 'D'                                        
037692           MOVE MFS-ALFA-FAELT-FEL TO MOD-TEWARNING-ATTR                  
037693           MOVE WS-DIST-WARNING    TO MOD-TEWARNING                       
037695        ELSE                                                              
037696           MOVE SPACE              TO MOD-TEWARNING                       
037697        END-IF                                                            
037698     ELSE                                                                 
037699       MOVE NEJ TO NYCKLAR-SW                                             
037700     END-IF                                                               
037701                                                                          
037710     IF GODK-MID OR NYCKLAR-OK                                            
037800       MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                         
037900       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
038000       MOVE MSGI-IDKUNDNR       TO MOD-IDKUNDNR-UT                        
038100       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
038110       MOVE MID-FLDISTCUST-IN   TO MOD-FLDISTCUST-UT                      
038200     ELSE                                                                 
038300       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
038400                               MOD-IDKUNDNR-UT                            
038410                               MOD-FLDISTCUST-UT                          
038500     END-IF                                                               
038600                                                                          
038700     IF NYCKLAR-FEL                                                       
038800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
038900       CALL WMEDKONV USING MED-WMEDAREA                                   
039000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
039100       PERFORM MFS-RENSA-FAELT-IN                                         
039200       PERFORM MFS-RENSA-FAELT-UT                                         
039300     END-IF                                                               
039400     .                                                                    
039500     EJECT                                                                
039600 C-FOERSTA-SIDA SECTION.                                                  
039700                                                                          
039800     MOVE JA TO ALLT-SW                                                   
039900     PERFORM MFS-RENSA-FAELT-IN                                           
040000     PERFORM MFS-FORM-ATTR                                                
040100     .                                                                    
040200     EJECT                                                                
040300 E-SAMMA-SIDA SECTION.                                                    
040400                                                                          
040500     IF EGEN-MID OR HELP-MID                                              
040600       IF MID-INPUT1 = ALL '+' AND                                        
040700          MID-INPUT2 = ALL '+' AND                                        
040710          MID-INPUT3 = ALL '+' AND                                        
040720          MID-INPUT4 = ALL '+'                                            
040800         PERFORM MFS-RENSA-FAELT-IN                                       
040900         MOVE JA TO ALLT-SW                                               
041000       ELSE                                                               
041100         MOVE NEJ TO ALLT-SW                                              
041200         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
041300         CALL WMEDKONV USING MED-WMEDAREA                                 
041400         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
041500         PERFORM MFS-FORM-ATTR                                            
041600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
041700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
041800         PERFORM EA-MID-INDATA-TILL-MOD                                   
041900       END-IF                                                             
042000     ELSE                                                                 
042100       PERFORM MFS-RENSA-FAELT-IN                                         
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500 EA-MID-INDATA-TILL-MOD SECTION.                                          
042600                                                                          
042610     MOVE MID-FLAUTORD                TO MOD-FLAUTORD                     
042620                                                                          
042700     IF MID-FLAUTORD NOT = ALL '+'                                        
042800       IF  MID-FLAUTORD = 'J'                                             
042900           MOVE 'Y'                   TO MOD-FLAUTORD                     
043000       ELSE                                                               
043100           MOVE MID-FLAUTORD          TO MOD-FLAUTORD                     
043200       END-IF                                                             
043300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLAUTORD-ATTR                    
043400     END-IF                                                               
043500                                                                          
043600     IF MID-IDRFTAB NOT = ALL '+'                                         
043700       MOVE MID-IDRFTAB           TO MOD-IDRFTAB                          
043800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDRFTAB-ATTR                     
043900     END-IF                                                               
044000                                                                          
044100     IF MID-FLNC NOT = ALL '+'                                            
044200       IF  MID-FLNC = 'J'                                                 
044300           MOVE 'Y'                   TO MOD-FLNC                         
044400       ELSE                                                               
044500           MOVE MID-FLNC              TO MOD-FLNC                         
044600       END-IF                                                             
044700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLNC-ATTR                        
044800     END-IF                                                               
044900                                                                          
045000     IF MID-KDORDING NOT = ALL '+'                                        
045100*      MOVE MID-KDORDING          TO MOD-KDORDING                         
045200       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDORDING                         
045300       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDORDING-ATTR                    
045400     END-IF                                                               
045500                                                                          
045600     IF MID-FLVR NOT = ALL '+'                                            
045700       IF  MID-FLVR = 'J'                                                 
045800           MOVE 'Y'                   TO MOD-FLVR                         
045900       ELSE                                                               
046000           MOVE MID-FLVR              TO MOD-FLVR                         
046100       END-IF                                                             
046200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLVR-ATTR                        
046300     END-IF                                                               
046400                                                                          
046500     IF MID-KVVECKOR-OB-IN NOT = ALL '+'                                  
046600*      MOVE MID-KVVECKOR-OB-IN       TO MOD-KVVECKOR-OB-IN                
046700       MOVE MFS-ROER-EJ-FAELT        TO MOD-KVVECKOR-OB-IN                
046800       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVVECKOR-OB-IN-ATTR              
046900     END-IF                                                               
047000                                                                          
047100     IF MID-FLURSRAP NOT = ALL '+'                                        
047200       IF  MID-FLURSRAP = 'J'                                             
047300           MOVE 'Y'                   TO MOD-FLURSRAP                     
047400       ELSE                                                               
047500           MOVE MID-FLURSRAP          TO MOD-FLURSRAP                     
047600       END-IF                                                             
047700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLURSRAP-ATTR                    
047800     END-IF                                                               
047900                                                                          
048100     MOVE +1 TO IDEX                                                      
048200     PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                                 
048300       IF MID-IDDC-TVSVOR (IDEX) NOT = ALL '+'                            
048400         MOVE MID-IDDC-TVSVOR (IDEX)                                      
048500                              TO MOD-IDDC-TVSVOR (IDEX)                   
048600         MOVE MFS-ADD-LAES-IN-FAELT                                       
048700                              TO MOD-IDDC-TVSVOR-ATTR (IDEX)              
048800       END-IF                                                             
048900       ADD +1 TO IDEX                                                     
049000     END-PERFORM                                                          
049010                                                                          
049100     IF MID-FLSAMFAK NOT = ALL '+'                                        
049200       IF  MID-FLSAMFAK = 'J'                                             
049300           MOVE 'Y'                   TO MOD-FLSAMFAK                     
049400       ELSE                                                               
049500           MOVE MID-FLSAMFAK          TO MOD-FLSAMFAK                     
049600       END-IF                                                             
049700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSAMFAK-ATTR                    
049800     END-IF                                                               
049900                                                                          
050000     IF MID-FLSWCONS NOT = ALL '+'                                        
050010       IF  MID-FLSWCONS = 'J'                                             
050020           MOVE 'Y'                   TO MOD-FLSWCONS                     
050030       ELSE                                                               
050040           MOVE MID-FLSWCONS          TO MOD-FLSWCONS                     
050050       END-IF                                                             
050060       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSWCONS-ATTR                    
050070     END-IF                                                               
050080                                                                          
050090     MOVE +1 TO IDEX                                                      
050091     PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                                
050092       IF MID-IDDC-PREPLAN (IDEX) NOT = ALL '+'                           
050093         MOVE MID-IDDC-PREPLAN (IDEX)                                     
050094                              TO MOD-IDDC-PREPLAN (IDEX)                  
050095         MOVE MFS-ADD-LAES-IN-FAELT                                       
050096                              TO MOD-IDDC-PREPLAN-ATTR (IDEX)             
050097       END-IF                                                             
050098       ADD +1 TO IDEX                                                     
050099     END-PERFORM                                                          
050100                                                                          
050110     IF MID-FLPRERS  NOT = ALL '+'                                        
050200       IF  MID-FLPRERS  = 'J'                                             
050300           MOVE 'Y'                   TO MOD-FLPRERS                      
050400       ELSE                                                               
050500           MOVE MID-FLPRERS           TO MOD-FLPRERS                      
050600       END-IF                                                             
050700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLPRERS-ATTR                     
050800     END-IF                                                               
050900                                                                          
051000     IF MID-FLFAKVKT NOT = ALL '+'                                        
051100       IF  MID-FLFAKVKT = 'J'                                             
051200           MOVE 'Y'                   TO MOD-FLFAKVKT                     
051300       ELSE                                                               
051400           MOVE MID-FLFAKVKT          TO MOD-FLFAKVKT                     
051500       END-IF                                                             
051600       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFAKVKT-ATTR                    
051700     END-IF                                                               
051800                                                                          
051900     IF MID-REAVDRAG-IN NOT = ALL '+'                                     
052000       MOVE MID-REAVDRAG-IN       TO DEC-IDFRIDATA                        
052100       MOVE 2                     TO DEC-KVHELTAL                         
052200       MOVE 1                     TO DEC-KVDECIMAL                        
052300                                                                          
052400       CALL WDECEDIT USING DEC-WDECAREA                                   
052500       IF DEC-KDSVAR-OK                                                   
052600         MOVE DEC-IDEDITDATA        TO MOD-REAVDRAG-IN                    
052700         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REAVDRAG-IN-ATTR               
052800       ELSE                                                               
052900         MOVE MFS-NUM-FAELT-FEL     TO MOD-REAVDRAG-IN-ATTR               
053000         MOVE MFS-ROER-EJ-FAELT     TO MOD-REAVDRAG-IN                    
053100         MOVE NEJ TO INDATA-SW                                            
053200       END-IF                                                             
053300     END-IF                                                               
053400                                                                          
053500     IF MID-FLFAKURS NOT = ALL '+'                                        
053600       IF  MID-FLFAKURS = 'J'                                             
053700           MOVE 'Y'                   TO MOD-FLFAKURS                     
053800       ELSE                                                               
053900           MOVE MID-FLFAKURS          TO MOD-FLFAKURS                     
054000       END-IF                                                             
054100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLFAKURS-ATTR                    
054200     END-IF                                                               
054300                                                                          
054400     IF MID-REEMBHNT-IN NOT = ALL '+'                                     
054500       MOVE MID-REEMBHNT-IN       TO DEC-IDFRIDATA                        
054600       MOVE 2                     TO DEC-KVHELTAL                         
054700       MOVE 1                     TO DEC-KVDECIMAL                        
054800                                                                          
054900       CALL WDECEDIT USING DEC-WDECAREA                                   
055000       IF DEC-KDSVAR-OK                                                   
055100         MOVE DEC-IDEDITDATA        TO MOD-REEMBHNT-IN                    
055200         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-REEMBHNT-IN-ATTR               
055300       ELSE                                                               
055400         MOVE MFS-NUM-FAELT-FEL     TO MOD-REEMBHNT-IN-ATTR               
055500         MOVE MFS-ROER-EJ-FAELT     TO MOD-REEMBHNT-IN                    
055600         MOVE NEJ TO INDATA-SW                                            
055700       END-IF                                                             
055800     END-IF                                                               
055900                                                                          
056000     IF MID-KDSTATNR NOT = ALL '+'                                        
056100       MOVE MID-KDSTATNR          TO MOD-KDSTATNR                         
056200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSTATNR-ATTR                    
056300     END-IF                                                               
056400                                                                          
056500     IF MID-KDHBLKRV NOT = ALL '+'                                        
056600       MOVE MID-KDHBLKRV          TO MOD-KDHBLKRV                         
056700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDHBLKRV-ATTR                    
056800     END-IF                                                               
056900                                                                          
057000     IF MID-KDSPRAK NOT = ALL '+'                                         
057100       MOVE MID-KDSPRAK           TO MOD-KDSPRAK                          
057200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSPRAK-ATTR                     
057300     END-IF                                                               
057400                                                                          
057500     IF MID-IDSKYLT-IN NOT = ALL '+'                                      
057600       MOVE MID-IDSKYLT-IN        TO MOD-IDSKYLT-IN                       
057700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDSKYLT-IN-ATTR                  
057800     END-IF                                                               
057900                                                                          
058000     IF MID-IDPARTNR NOT = ALL '+'                                        
058100       MOVE MID-IDPARTNR          TO MOD-IDPARTNR                         
058200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPARTNR-ATTR                    
058300     END-IF                                                               
058400                                                                          
058410     IF MID-IDFTG NOT = ALL '+'                                           
058420       MOVE MID-IDFTG             TO MOD-IDFTG                            
058430       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFTG-ATTR                       
058440     END-IF                                                               
058450                                                                          
058500     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
058600       MOVE MID-IDLEVNR-IN        TO MOD-IDLEVNR-IN                       
058700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVNR-IN-ATTR                  
058800     END-IF                                                               
058900                                                                          
059000     .                                                                    
059100     EJECT                                                                
059200                                                                          
059300 F-LAES-VISA-INFO SECTION.                                                
059400                                                                          
059500     PERFORM IMS-GHU-WDB201                                               
059600                                                                          
059700     IF SEGMENT-SAKNAS                                                    
059800* FLYTTA LÄMPLIGT FELMEDDELANDE                                           
059900*       CALL WMEDKONV USING MED-WMEDAREA                                  
060000*       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
060100        MOVE 'GOODS RECEIVER MISSING' TO MOD-TEMFSFEL                     
060200        PERFORM MFS-RENSA-FAELT-UT                                        
060300     ELSE                                                                 
060400       IF  GMTA-GMT-FLAUTORD = 'J'                                        
060500           MOVE 'Y'                   TO MOD-FLAUTORD                     
060600       ELSE                                                               
060700           MOVE GMTA-GMT-FLAUTORD     TO MOD-FLAUTORD                     
060800       END-IF                                                             
060900       MOVE GMTA-GMT-IDRFTAB             TO MOD-IDRFTAB                   
061000       IF  GMTA-GMT-FLNC = 'J'                                            
061100           MOVE 'Y'                   TO MOD-FLNC                         
061200       ELSE                                                               
061300           MOVE GMTA-GMT-FLNC         TO MOD-FLNC                         
061400       END-IF                                                             
061500       MOVE GMTA-GMT-KDORDING            TO MOD-KDORDING                  
061600       IF  GMTA-GMT-FLVR = 'J'                                            
061700           MOVE 'Y'                   TO MOD-FLVR                         
061800       ELSE                                                               
061900           MOVE GMTA-GMT-FLVR         TO MOD-FLVR                         
062000       END-IF                                                             
062100       MOVE GMTA-GMT-KVVECKOR-OB         TO MOD-KVVECKOR-OB-UT            
062200       IF  GMTA-GMT-FLURSRAP = 'J'                                        
062300           MOVE 'Y'                   TO MOD-FLURSRAP                     
062400       ELSE                                                               
062500           MOVE GMTA-GMT-FLURSRAP     TO MOD-FLURSRAP                     
062600       END-IF                                                             
062700                                                                          
062800       MOVE +1 TO IDEX                                                    
062900       PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                               
063000         MOVE GMTA-GMT-IDDC-TVSVOR (IDEX)                                 
063100                                      TO MOD-IDDC-TVSVOR (IDEX)           
063200         ADD +1 TO IDEX                                                   
063300       END-PERFORM                                                        
063400                                                                          
063500       IF  GMTA-GMT-FLSAMFAK = 'J'                                        
063600           MOVE 'Y'                   TO MOD-FLSAMFAK                     
063700       ELSE                                                               
063800           MOVE GMTA-GMT-FLSAMFAK     TO MOD-FLSAMFAK                     
063900       END-IF                                                             
063912       IF  GMTA-GMT-FLSWCONS = 'J'                                        
063920           MOVE 'Y'                   TO MOD-FLSWCONS                     
063930       ELSE                                                               
063940           MOVE GMTA-GMT-FLSWCONS     TO MOD-FLSWCONS                     
063950       END-IF                                                             
063960       MOVE +1 TO IDEX                                                    
063970       PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                              
063980         MOVE GMTA-GMT-IDDC-PREPLAN (IDEX)                                
063990                                      TO MOD-IDDC-PREPLAN (IDEX)          
063991         ADD +1 TO IDEX                                                   
063992       END-PERFORM                                                        
064000       IF  GMTA-GMT-FLFAKVKT = 'J'                                        
064100           MOVE 'Y'                   TO MOD-FLFAKVKT                     
064200       ELSE                                                               
064300           MOVE GMTA-GMT-FLFAKVKT     TO MOD-FLFAKVKT                     
064400       END-IF                                                             
064500       IF  GMTA-GMT-FLPRERS  = 'J'                                        
064600           MOVE 'Y'                   TO MOD-FLPRERS                      
064700       ELSE                                                               
064800           MOVE GMTA-GMT-FLPRERS      TO MOD-FLPRERS                      
064900       END-IF                                                             
065000       MOVE GMTA-GMT-REAVDRAG            TO MOD-REAVDRAG-UT               
065100       IF  GMTA-GMT-FLFAKURS = 'J'                                        
065200           MOVE 'Y'                   TO MOD-FLFAKURS                     
065300       ELSE                                                               
065400           MOVE GMTA-GMT-FLFAKURS     TO MOD-FLFAKURS                     
065500       END-IF                                                             
065600       MOVE GMTA-GMT-REEMBHNT            TO MOD-REEMBHNT-UT               
065602       IF DIST35-NONVCC-REFILL                                            
065611                                                                          
065612         SEARCH ALL WWDC03-IDDC                                           
065613           AT END                                                         
065614              MOVE 'GOODS RECEIVER MISSING ' TO MOD-TEMFSFEL              
065615              PERFORM MFS-RENSA-FAELT-IN                                  
065616              PERFORM MFS-RENSA-FAELT-UT                                  
065617           WHEN WWDC03-SOK-IDDISTR-TAB2(WWDC03-IX2)                       
065618                                         = TEST-IDDISTR                   
065619              MOVE WWDC03-SOK-IDDC-SEND(WWDC03-IX2)                       
065620                                             TO W-IDDC                    
065621         END-SEARCH                                                       
065622                                                                          
065623         PERFORM IMS-GU-WDB617                                            
065624         IF SEGMENT-FINNS                                                 
065625**** IF SOMEONE MANUALLY SET THE VALUE TO ZERO                            
065626            IF PROC-RELANDCO-EXP = 0.000                                  
065627              MOVE ZERO TO WS-RELANDCO-EXP                                
065628            ELSE                                                          
065629              COMPUTE WS-RELANDCO-EXP =                                   
065630                    (PROC-RELANDCO-EXP - 1) * 100                         
065631            END-IF                                                        
065632         ELSE                                                             
065633           MOVE ZERO TO WS-RELANDCO-EXP                                   
065634         END-IF                                                           
065635         IF WS-RELANDCO-EXP(1:1) = ZERO                                   
065636           IF WS-RELANDCO-EXP(2:1) = ZERO                                 
065637             MOVE SPACE                 TO MOD-BETEXT-INFO(1:2)           
065638             MOVE WS-RELANDCO-EXP(3:1)  TO MOD-BETEXT-INFO(3:1)           
065639           ELSE                                                           
065640             MOVE SPACE                 TO MOD-BETEXT-INFO(1:1)           
065641             MOVE WS-RELANDCO-EXP(2:2)  TO MOD-BETEXT-INFO(2:2)           
065642           END-IF                                                         
065643         ELSE                                                             
065644           MOVE WS-RELANDCO-EXP(1:3)  TO MOD-BETEXT-INFO(1:3)             
065645         END-IF                                                           
065646         MOVE '.'                   TO MOD-BETEXT-INFO(4:1)               
065647         MOVE WS-RELANDCO-EXP(4:1)  TO MOD-BETEXT-INFO(5:1)               
065648         MOVE ' % EXPORT FACTOR ADDED TO P&H' TO                          
065649               MOD-BETEXT-INFO(6:29)                                      
065650       END-IF                                                             
065700       MOVE GMTA-GMT-KDSTATNR            TO MOD-KDSTATNR                  
065800       MOVE GMTA-GMT-KDHBLKRV            TO MOD-KDHBLKRV                  
065900       MOVE GMTA-GMT-KDSPRAK             TO MOD-KDSPRAK                   
066000       MOVE GMTA-GMT-IDSKYLT             TO MOD-IDSKYLT-UT                
066100       MOVE GMTA-GMT-IDPARTNR            TO MOD-IDPARTNR                  
066110       MOVE GMTA-GMT-IDFTG               TO MOD-IDFTG                     
066200       MOVE GMTA-GMT-IDLEVNR             TO MOD-IDLEVNR-UT                
066201       MOVE NEJ TO BETALAR-SW                                             
066202       MOVE NEJ TO FTG-SW                                                 
066210       PERFORM S03-KOLLA-BETALARE                                         
066300     END-IF                                                               
066310                                                                          
066320     IF MOD-FLDISTCUST-UT = 'C'                                           
066330        PERFORM FA-PROTECT-DISTRICT-FIELDS                                
066340     ELSE                                                                 
066350        PERFORM FB-PROTECT-CUSTOMER-FIELDS                                
066360     END-IF                                                               
066400     .                                                                    
066500     EJECT                                                                
066600                                                                          
066700 FA-PROTECT-DISTRICT-FIELDS SECTION.                                      
066800                                                                          
066801     MOVE MFS-CLOSE-FIELD-NOMOD TO MOD-FLAUTORD-ATTR                      
066806                                   MOD-IDRFTAB-ATTR                       
066810                                   MOD-FLNC-ATTR                          
066814                                   MOD-KDORDING-ATTR                      
066818                                   MOD-FLVR-ATTR                          
066824                                   MOD-KVVECKOR-OB-IN-ATTR                
066829                                   MOD-FLURSRAP-ATTR                      
066842                                   MOD-FLSAMFAK-ATTR                      
066846                                   MOD-FLSWCONS-ATTR                      
066856                                   MOD-FLPRERS-ATTR                       
066857     MOVE 1 TO IDEX                                                       
066858     PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                                 
066859        MOVE MFS-CLOSE-FIELD-NOMOD TO MOD-IDDC-TVSVOR-ATTR (IDEX)         
066860        ADD +1 TO IDEX                                                    
066861     END-PERFORM                                                          
066862     MOVE 1 TO IDEX                                                       
066863     PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                                
066864        MOVE MFS-CLOSE-FIELD-NOMOD TO MOD-IDDC-PREPLAN-ATTR (IDEX)        
066865        ADD +1 TO IDEX                                                    
066866     END-PERFORM                                                          
066867     IF MOD-FLSAMFAK = YES                                                
066868        MOVE MFS-CLOSE-FIELD-NOMOD TO MOD-REAVDRAG-IN-ATTR                
066869                                      MOD-REEMBHNT-IN-ATTR                
066870                                      MOD-IDPARTNR-ATTR                   
066871                                      MOD-IDFTG-ATTR                      
066872                                      MOD-IDLEVNR-IN-ATTR                 
066873     END-IF                                                               
066874     .                                                                    
066875     EJECT                                                                
066876                                                                          
066877 FB-PROTECT-CUSTOMER-FIELDS SECTION.                                      
066878                                                                          
066879     IF MID-FLSAMFAK = NEJ                                                
066880        MOVE MFS-CLOSE-FIELD-NOMOD TO MOD-FLFAKVKT-ATTR                   
066881                                      MOD-REAVDRAG-IN-ATTR                
066884                                      MOD-FLFAKURS-ATTR                   
066890                                      MOD-REEMBHNT-IN-ATTR                
066894                                      MOD-KDSTATNR-ATTR                   
066904                                      MOD-KDHBLKRV-ATTR                   
066908                                      MOD-KDSPRAK-ATTR                    
066915                                      MOD-IDSKYLT-IN-ATTR                 
066920                                      MOD-IDPARTNR-ATTR                   
066924                                      MOD-IDFTG-ATTR                      
066930                                      MOD-IDLEVNR-IN-ATTR                 
066931     ELSE                                                                 
066932        MOVE MFS-CLOSE-FIELD-NOMOD TO MOD-FLFAKVKT-ATTR                   
066933                                      MOD-FLFAKURS-ATTR                   
066935                                      MOD-KDSTATNR-ATTR                   
066936                                      MOD-KDHBLKRV-ATTR                   
066937                                      MOD-KDSPRAK-ATTR                    
066938                                      MOD-IDSKYLT-IN-ATTR                 
066944     END-IF                                                               
066945     .                                                                    
066946     EJECT                                                                
066947                                                                          
066948 G-KOLLA-INPUT SECTION.                                                   
066949                                                                          
066950     MOVE JA  TO INDATA-SW                                                
067000     MOVE NEJ TO BETALAR-SW                                               
067100                 LEVNR-SW                                                 
067110                 FTG-SW                                                   
067200                                                                          
067300     IF MID-INPUT1 = ALL '+' AND                                          
067400        MID-INPUT2 = ALL '+' AND                                          
067410        MID-INPUT3 = ALL '+' AND                                          
067420        MID-INPUT4 = ALL '+'                                              
067500       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
067600       CALL WMEDKONV USING MED-WMEDAREA                                   
067700       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
067800       PERFORM MFS-ROER-EJ-FAELT-IN                                       
067900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
068000       MOVE NEJ TO INDATA-SW                                              
068100     ELSE                                                                 
068200                                                                          
068210       IF MFS-UPDATE                                                      
068220          IF MID-IDFTG = ALL '+'                                          
068300            IF MID-INPUT1 NOT = ALL '+'                                   
068400              PERFORM GA-KOLLA-DISTR-INFO                                 
068500            END-IF                                                        
068700            IF MID-INPUT2 NOT = ALL '+'                                   
068800              PERFORM GB-KOLLA-OM-SAMFAKT                                 
068900            END-IF                                                        
068910            IF MID-INPUT3 NOT = ALL '+'                                   
068911              PERFORM GC-KOLLA-TVSVOR                                     
068912            END-IF                                                        
068913            IF MID-INPUT4 NOT = ALL '+'                                   
068914              PERFORM GD-KOLLA-PREPLAN                                    
068915            END-IF                                                        
068916          ELSE                                                            
068917*           *MID-IDFTG MUST BE UPDATED WITH PF23                          
068918            MOVE INF-PRESS-PF23    TO MED-IDMFSINF                        
068919            CALL WMEDKONV USING MED-WMEDAREA                              
068920            MOVE MED-MFSINF TO MOD-TEMFSINF                               
068921            PERFORM MFS-ROER-EJ-FAELT-IN                                  
068922            PERFORM MFS-ROER-EJ-FAELT-UT                                  
068923            PERFORM EA-MID-INDATA-TILL-MOD                                
068924            MOVE MFS-NUM-FAELT-FEL TO MOD-IDFTG-ATTR                      
068925            MOVE NEJ TO INDATA-SW                                         
068926          END-IF                                                          
068927       ELSE                                                               
068928          IF MID-INPUT1 NOT = ALL '+'                                     
068929            PERFORM GA-KOLLA-DISTR-INFO                                   
068930          END-IF                                                          
068931          IF MID-INPUT2 NOT = ALL '+'                                     
068932            PERFORM GB-KOLLA-OM-SAMFAKT                                   
068933          END-IF                                                          
068934          IF MID-INPUT3 NOT = ALL '+'                                     
068935            PERFORM GC-KOLLA-TVSVOR                                       
068936          END-IF                                                          
068937          IF MID-INPUT4 NOT = ALL '+'                                     
068938            PERFORM GD-KOLLA-PREPLAN                                      
068939          END-IF                                                          
068940       END-IF                                                             
069000                                                                          
069100       IF INDATA-FEL                                                      
069200         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
069300         CALL WMEDKONV USING MED-WMEDAREA                                 
069400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
069500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
069600         PERFORM MFS-ROER-EJ-FAELT-IN                                     
069700         MOVE NEJ TO ALLT-SW                                              
069800       ELSE                                                               
069900         PERFORM IMS-GHU-WDB201                                           
070000         IF SEGMENT-FINNS                                                 
070100           CONTINUE                                                       
070200         ELSE                                                             
070300* FLYTTA LÄMPLIGT FELMEDDELANDE                                           
070400           MOVE 'GOODS RECEIVER MISSING ' TO MOD-TEMFSFEL                 
070500*          CALL WMEDKONV USING MED-WMEDAREA                               
070600*          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
070700           PERFORM MFS-RENSA-FAELT-IN                                     
070800           PERFORM MFS-RENSA-FAELT-UT                                     
070900         END-IF                                                           
071000       END-IF                                                             
071100     END-IF                                                               
071200     .                                                                    
071300     EJECT                                                                
071400                                                                          
071500 GA-KOLLA-DISTR-INFO SECTION.                                             
071600                                                                          
071700     IF MID-FLAUTORD NOT = ALL '+'                                        
071800       IF MID-FLAUTORD = 'Y' OR 'J' OR 'N'                                
071900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAUTORD-ATTR                   
072000       ELSE                                                               
072100         MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLAUTORD-ATTR                 
072200         MOVE NEJ TO INDATA-SW                                            
072300       END-IF                                                             
072400     END-IF                                                               
072500                                                                          
072600     IF MID-IDRFTAB NOT = ALL '+'                                         
072700       MOVE MID-IDRFTAB TO W-IDRFTAB                                      
072800       PERFORM IMS-GU-XXKM11                                              
072900       IF SEGMENT-FINNS                                                   
073000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDRFTAB-ATTR                    
073100       ELSE                                                               
073200         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDRFTAB-ATTR                      
073300         MOVE NEJ TO INDATA-SW                                            
073400         MOVE ERR-RAT-FACT-MISS TO MED-IDMFSINF                           
073500         CALL WMEDKONV USING MED-WMEDAREA                                 
073600         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
073700       END-IF                                                             
073800     END-IF                                                               
073900                                                                          
074000     IF MID-FLNC NOT = ALL '+'                                            
074100       IF MID-FLNC = 'Y' OR 'J' OR 'N'                                    
074200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLNC-ATTR                       
074300       ELSE                                                               
074400         MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLNC-ATTR                     
074500         MOVE NEJ TO INDATA-SW                                            
074600       END-IF                                                             
074700     END-IF                                                               
074800                                                                          
074900     IF MID-KDORDING NOT = ALL '+'                                        
075000       IF MID-KDORDING NUMERIC                                            
075100         IF MID-KDORDING >= 1 AND <= 3                                    
075200           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDORDING-ATTR                  
075300         ELSE                                                             
075400           MOVE MFS-NUM-FAELT-FEL TO MOD-KDORDING-ATTR                    
075500           MOVE NEJ TO INDATA-SW                                          
075600         END-IF                                                           
075700       ELSE                                                               
075800         MOVE MFS-NUM-FAELT-FEL TO MOD-KDORDING-ATTR                      
075900         MOVE NEJ TO INDATA-SW                                            
076000       END-IF                                                             
076100     END-IF                                                               
076200                                                                          
076300     IF MID-FLVR NOT = ALL '+'                                            
076400       IF MID-FLVR = 'Y' OR 'J' OR 'N'                                    
076500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLVR-ATTR                       
076600       ELSE                                                               
076700         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLVR-ATTR                         
076800         MOVE NEJ TO INDATA-SW                                            
076900       END-IF                                                             
077000     END-IF                                                               
077100                                                                          
077200     IF MID-KVVECKOR-OB-IN NOT = ALL '+'                                  
077300       IF MID-KVVECKOR-OB-IN NUMERIC                                      
077400         IF MID-KVVECKOR-OB-IN > 0                                        
077500           MOVE MFS-NUM-FAELT-RAETT TO MOD-KVVECKOR-OB-IN-ATTR            
077600         ELSE                                                             
077700           MOVE MFS-NUM-FAELT-FEL     TO MOD-KVVECKOR-OB-IN-ATTR          
077800           MOVE NEJ TO INDATA-SW                                          
077900         END-IF                                                           
078000       ELSE                                                               
078100         MOVE MFS-NUM-FAELT-FEL       TO MOD-KVVECKOR-OB-IN-ATTR          
078200         MOVE NEJ TO INDATA-SW                                            
078300       END-IF                                                             
078400     END-IF                                                               
078500                                                                          
078600     IF MID-FLURSRAP NOT = ALL '+'                                        
078700       IF MID-FLURSRAP = 'Y' OR 'J' OR 'N'                                
078800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLURSRAP-ATTR                   
078900       ELSE                                                               
079000         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLURSRAP-ATTR                     
079100         MOVE NEJ TO INDATA-SW                                            
079200       END-IF                                                             
079300     END-IF                                                               
079400                                                                          
079500                                                                          
082700                                                                          
082800     IF MID-FLSAMFAK NOT = ALL '+'                                        
082900       IF MID-FLSAMFAK = 'Y' OR 'J' OR 'N'                                
083000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSAMFAK-ATTR                   
083100       ELSE                                                               
083200         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSAMFAK-ATTR                     
083300         MOVE NEJ TO INDATA-SW                                            
083400       END-IF                                                             
083500     END-IF                                                               
083600                                                                          
083610     IF MID-FLSWCONS NOT = ALL '+'                                        
083620       IF MID-FLSWCONS = 'Y' OR 'J' OR 'N'                                
083630         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSWCONS-ATTR                   
083640       ELSE                                                               
083650         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSWCONS-ATTR                     
083660         MOVE NEJ TO INDATA-SW                                            
083670       END-IF                                                             
083680     END-IF                                                               
083690                                                                          
083700     IF MID-FLPRERS  NOT = ALL '+'                                        
083800       IF MID-FLPRERS  = 'Y' OR 'J' OR 'N'                                
083900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLPRERS-ATTR                    
084000       ELSE                                                               
084100         MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLPRERS-ATTR                  
084200         MOVE NEJ TO INDATA-SW                                            
084300       END-IF                                                             
084400     END-IF                                                               
084500     .                                                                    
084600     EJECT                                                                
084700                                                                          
084800 GB-KOLLA-OM-SAMFAKT SECTION.                                             
084900                                                                          
085000     IF MID-FLFAKVKT NOT = ALL '+'                                        
085100       IF MID-FLFAKVKT = 'Y' OR 'J' OR 'N'                                
085200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLFAKVKT-ATTR                   
085300       ELSE                                                               
085400         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFAKVKT-ATTR                     
085500         MOVE NEJ TO INDATA-SW                                            
085600       END-IF                                                             
085700     END-IF                                                               
085800                                                                          
085900     IF MID-REAVDRAG-IN NOT = ALL '+'                                     
086000       MOVE MID-REAVDRAG-IN TO DEC-IDFRIDATA                              
086100       MOVE 2                 TO DEC-KVHELTAL                             
086200       MOVE 1                 TO DEC-KVDECIMAL                            
086300                                                                          
086400       CALL WDECEDIT USING DEC-WDECAREA                                   
086500       IF DEC-KDSVAR-OK                                                   
086600         MOVE DEC-IDEDITDATA        TO WS-REAVDRAG                        
086700         MOVE MFS-NUM-FAELT-RAETT TO MOD-REAVDRAG-IN-ATTR                 
086800       ELSE                                                               
086900         MOVE MFS-NUM-FAELT-FEL     TO MOD-REAVDRAG-IN-ATTR               
087000         MOVE NEJ TO INDATA-SW                                            
087100       END-IF                                                             
087200     END-IF                                                               
087300                                                                          
087400     IF MID-FLFAKURS NOT = ALL '+'                                        
087500       IF MID-FLFAKURS = 'Y' OR 'J' OR 'N'                                
087600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLFAKURS-ATTR                   
087700       ELSE                                                               
087800         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLFAKURS-ATTR                     
087900         MOVE NEJ TO INDATA-SW                                            
088000       END-IF                                                             
088100     END-IF                                                               
088200                                                                          
088300     IF MID-REEMBHNT-IN NOT = ALL '+'                                     
088400       MOVE MID-REEMBHNT-IN TO DEC-IDFRIDATA                              
088500       MOVE 2                 TO DEC-KVHELTAL                             
088600       MOVE 1                 TO DEC-KVDECIMAL                            
088700                                                                          
088800       CALL WDECEDIT USING DEC-WDECAREA                                   
088900       IF DEC-KDSVAR-OK                                                   
089000         MOVE DEC-IDEDITDATA    TO WS-REEMBHNT                            
089100         MOVE MFS-NUM-FAELT-RAETT TO MOD-REEMBHNT-IN-ATTR                 
089200       ELSE                                                               
089300         MOVE MFS-NUM-FAELT-FEL     TO MOD-REEMBHNT-IN-ATTR               
089400         MOVE NEJ TO INDATA-SW                                            
089500       END-IF                                                             
089600     END-IF                                                               
089700                                                                          
089800     IF MID-KDSTATNR NOT = ALL '+'                                        
089900       IF MID-KDSTATNR NUMERIC                                            
090000         IF MID-KDSTATNR < 8 OR = 9                                       
090100           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDSTATNR-ATTR                  
090200         ELSE                                                             
090300           MOVE MFS-NUM-FAELT-FEL TO MOD-KDSTATNR-ATTR                    
090400           MOVE NEJ TO INDATA-SW                                          
090500         END-IF                                                           
090600       ELSE                                                               
090700         MOVE MFS-NUM-FAELT-FEL TO MOD-KDSTATNR-ATTR                      
090800         MOVE NEJ TO INDATA-SW                                            
090900       END-IF                                                             
091000     END-IF                                                               
091100                                                                          
091200     IF MID-KDHBLKRV NOT = ALL '+'                                        
091300       IF MID-KDHBLKRV NUMERIC AND                                        
091400          MID-KDHBLKRV < 4                                                
091500          MOVE MFS-NUM-FAELT-RAETT TO MOD-KDHBLKRV-ATTR                   
091600       ELSE                                                               
091700         MOVE MFS-NUM-FAELT-FEL TO MOD-KDHBLKRV-ATTR                      
091800         MOVE NEJ TO INDATA-SW                                            
091900       END-IF                                                             
092000     END-IF                                                               
092100                                                                          
092200     IF MID-KDSPRAK NOT = ALL '+'                                         
092300       IF MID-KDSPRAK NUMERIC AND                                         
092400          MID-KDSPRAK < 6                                                 
092500          MOVE MFS-NUM-FAELT-RAETT TO MOD-KDSPRAK-ATTR                    
092600       ELSE                                                               
092700         MOVE MFS-NUM-FAELT-FEL TO MOD-KDSPRAK-ATTR                       
092800         MOVE NEJ TO INDATA-SW                                            
092900       END-IF                                                             
093000     END-IF                                                               
093100                                                                          
093200     IF MID-IDSKYLT-IN NOT = ALL '+'                                      
093300       MOVE MID-IDSKYLT-IN TO WS-IDSKYLT                                  
093400       IF GODK-IDSKYLT                                                    
093500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDSKYLT-IN-ATTR                 
093600       ELSE                                                               
093700         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDSKYLT-IN-ATTR                   
093800         MOVE NEJ TO INDATA-SW                                            
093900       END-IF                                                             
094000     END-IF                                                               
094100                                                                          
094200     IF MID-IDPARTNR NOT = ALL '+'                                        
094300       MOVE MFS-ALFA-FAELT-RAETT     TO MOD-IDPARTNR-ATTR                 
094400       MOVE MID-IDPARTNR             TO WS-IDPARTNR                       
094500       MOVE JA TO BETALAR-SW                                              
094600     END-IF                                                               
094700                                                                          
094800     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
094900         IF MID-IDLEVNR-IN NOT = SPACE                                    
095000           MOVE MID-IDLEVNR-IN TO W-IDLEVNR-X                             
095100           PERFORM IMS-GU-WDF101                                          
095200           IF SEGMENT-FINNS                                               
095300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR             
095400             MOVE JA TO LEVNR-SW                                          
095500           ELSE                                                           
095600             MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDLEVNR-IN-ATTR         
095700             MOVE NEJ TO INDATA-SW                                        
095800           END-IF                                                         
095900         ELSE                                                             
096000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-IN-ATTR               
096100           MOVE JA TO LEVNR-SW                                            
096200         END-IF                                                           
096300     END-IF                                                               
096400                                                                          
096410     IF MID-IDFTG NOT = ALL '+'                                           
096424       IF MID-FLSAMFAK = 'Y' OR 'J'                                       
096425         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDFTG-ATTR                     
096426         MOVE NEJ TO INDATA-SW                                            
096427       ELSE                                                               
096429         PERFORM IMS-GHU-WDB201                                           
096430         IF SEGMENT-FINNS                                                 
096432* WE WANT TO UPDATE CC WITHOUT GETTING TO UPDATE CONS INV FIELD           
096433*          IF GMTA-GMT-FLSAMFAK = 'J' AND MID-FLSAMFAK = ALL '+'          
096434*            MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFTG-ATTR                   
096435*            MOVE NEJ TO INDATA-SW                                        
096436*          ELSE                                                           
096437*            *MID-FLSAMFAK = N AND GMTA-GMT-FLSAMFAK = N                  
096438             MOVE MID-IDFTG             TO WS-IDFTG                       
096439             IF IDFTG-GODKAEND                                            
096440               MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFTG-ATTR                 
096441               MOVE JA TO FTG-SW                                          
096442             ELSE                                                         
096443               MOVE MFS-NUM-FAELT-FEL   TO MOD-IDFTG-ATTR                 
096444               MOVE NEJ TO INDATA-SW                                      
096445             END-IF                                                       
096446*          END-IF                                                         
096447         END-IF                                                           
096450       END-IF                                                             
096494     END-IF                                                               
096495                                                                          
096500     IF INDATA-OK                                                         
096600*      IF (BETALARE-AENDRAD) OR (LEVNR-AENDRAD)                           
096700       IF  BETALARE-AENDRAD  OR FTG-AENDRAD                               
096800         PERFORM S03-KOLLA-BETALARE                                       
096900       END-IF                                                             
097000     END-IF                                                               
097100     .                                                                    
097200     EJECT                                                                
097300 GC-KOLLA-TVSVOR SECTION.                                                 
097400                                                                          
097493     MOVE +1 TO IDEX                                                      
097494     PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                                 
097495       IF MID-IDDC-TVSVOR (IDEX) NOT = ALL '+'                            
097496         MOVE MID-IDDC-TVSVOR (IDEX) TO W-IDDC-B6                         
097497         PERFORM  IMS-GU-WDB601                                           
097498         IF DCS-SDC                                                       
097499         OR W-IDDC-B6 = SPACE                                             
097500           IF DCS-SDC                                                     
097501             MOVE DCS-IDDC     TO W-IDDC-DEF                              
097502             MOVE MSGI-IDDISTR TO W-IDDISTR-DC-DEF                        
097503             PERFORM IMS-GU-GMTB-WDB301                                   
097504             IF SEGMENT-FINNS                                             
097505               MOVE MFS-ALFA-FAELT-RAETT                                  
097506                             TO MOD-IDDC-TVSVOR-ATTR (IDEX)               
097507             ELSE                                                         
097508               MOVE MFS-ALFA-FAELT-FEL                                    
097509                             TO MOD-IDDC-TVSVOR-ATTR (IDEX)               
097510               MOVE NEJ TO INDATA-SW                                      
097511             END-IF                                                       
097512           ELSE                                                           
097513             MOVE MFS-ALFA-FAELT-RAETT                                    
097514                             TO MOD-IDDC-TVSVOR-ATTR (IDEX)               
097515           END-IF                                                         
097516         ELSE                                                             
097517           MOVE MFS-ALFA-FAELT-FEL                                        
097518                             TO MOD-IDDC-TVSVOR-ATTR (IDEX)               
097519           MOVE NEJ TO INDATA-SW                                          
097520         END-IF                                                           
097521       END-IF                                                             
097522       ADD +1 TO IDEX                                                     
097523     END-PERFORM                                                          
097524                                                                          
097530     .                                                                    
097600     EJECT                                                                
097700 GD-KOLLA-PREPLAN SECTION.                                                
097800                                                                          
097900     MOVE +1 TO IDEX                                                      
098000     PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                                
098100       IF MID-IDDC-PREPLAN (IDEX) NOT = ALL '+'                           
098200         MOVE MID-IDDC-PREPLAN (IDEX) TO W-IDDC-B6                        
098300         PERFORM  IMS-GU-WDB601                                           
098400         IF DCS-SDC OR  DCS-CDC                                           
098500         OR W-IDDC-B6 = SPACE                                             
098600           IF DCS-SDC OR  DCS-CDC                                         
098700             MOVE DCS-IDDC     TO W-IDDC-DEF                              
098800             MOVE MSGI-IDDISTR TO W-IDDISTR-DC-DEF                        
098900             PERFORM IMS-GU-GMTB-WDB301                                   
099000             IF SEGMENT-FINNS                                             
099100               MOVE MFS-ALFA-FAELT-RAETT                                  
099200                             TO MOD-IDDC-PREPLAN-ATTR (IDEX)              
099300             ELSE                                                         
099400               MOVE MFS-ALFA-FAELT-FEL                                    
099500                             TO MOD-IDDC-PREPLAN-ATTR (IDEX)              
099600               MOVE NEJ TO INDATA-SW                                      
099700             END-IF                                                       
099800           ELSE                                                           
099900             MOVE MFS-ALFA-FAELT-RAETT                                    
100000                             TO MOD-IDDC-PREPLAN-ATTR (IDEX)              
100100           END-IF                                                         
100200         ELSE                                                             
100300           MOVE MFS-ALFA-FAELT-FEL                                        
100400                             TO MOD-IDDC-PREPLAN-ATTR (IDEX)              
100500           MOVE NEJ TO INDATA-SW                                          
100600         END-IF                                                           
100700       END-IF                                                             
100800       ADD +1 TO IDEX                                                     
100900     END-PERFORM                                                          
101000                                                                          
101100     .                                                                    
101200     EJECT                                                                
104100 H-UPPDATERA SECTION.                                                     
104200                                                                          
104300     MOVE ZERO TO SPAR-IDKUNDNR                                           
104310     MOVE ZERO TO SPAR-IDKUNDNR-VTRANS                                    
104400     PERFORM IMS-GHU-WDB201                                               
104500     IF SEGMENT-FINNS                                                     
104510       IF MFS-UPD-V                                                       
104520          MOVE GMTA-GMT-IDKUNDNR TO SPAR-IDKUNDNR-VTRANS                  
104530       END-IF                                                             
104600       IF MID-INPUT1 NOT = ALL '+' AND                                    
104700          MID-INPUT2 NOT = ALL '+'                                        
104800          IF GMTA-GMT-FLSAMFAK = JA OR                                    
104900             MID-FLSAMFAK = JA OR YES                                     
105000             IF MID-FLSAMFAK = JA OR YES                                  
105100               PERFORM S01-HAEMTA-DEF-UPPG-DISTR                          
105200               PERFORM S02-HAEMTA-DEF-UPPG-SAMFAK                         
105300               PERFORM HA-UPPDAT-ALLA-KUNDER                              
105400             ELSE                                                         
105500               MOVE GMTA-GMT-IDKUNDNR TO SPAR-IDKUNDNR                    
105600               PERFORM S01-HAEMTA-DEF-UPPG-DISTR                          
105700               PERFORM HB-UPPDAT-ALLA-KUNDER                              
105800             END-IF                                                       
105900          ELSE                                                            
106000            IF MID-FLSAMFAK = NEJ                                         
106010              PERFORM S01-HAEMTA-DEF-UPPG-DISTR                           
106020              PERFORM HC-UPPDAT-DISTR-INFO                                
106030              PERFORM S04-HAEMTA-DEF-UPPG-KUND                            
106040              PERFORM HE-UPPDAT-UNIK-KUND                                 
106400            ELSE                                                          
106500              PERFORM S01-HAEMTA-DEF-UPPG-DISTR                           
106600              PERFORM HB-UPPDAT-ALLA-KUNDER                               
106700            END-IF                                                        
106800          END-IF                                                          
106900       ELSE                                                               
107000         IF MID-INPUT1 NOT = ALL '+'                                      
107100******* ????? OM DENNA KUND SÄTTS TILL SAMFAK = JA GÄLLER DÅ DENNA        
107200******* ????? KUNDS UPPGIFTER SOM DEFAULT                                 
107300           IF GMTA-GMT-FLSAMFAK = JA                                      
107400              PERFORM S01-HAEMTA-DEF-UPPG-DISTR                           
107500              PERFORM HC-UPPDAT-DISTR-INFO                                
107600           ELSE                                                           
107700             IF MID-FLSAMFAK = JA                                         
107800               PERFORM S01-HAEMTA-DEF-UPPG-DISTR                          
107900               PERFORM S02-HAEMTA-DEF-UPPG-SAMFAK                         
108000               PERFORM HA-UPPDAT-ALLA-KUNDER                              
108100             ELSE                                                         
108200               PERFORM S01-HAEMTA-DEF-UPPG-DISTR                          
108300               PERFORM HC-UPPDAT-DISTR-INFO                               
108400             END-IF                                                       
108500           END-IF                                                         
108600         ELSE                                                             
108700           IF MID-INPUT2 NOT = ALL '+'                                    
108800             IF GMTA-GMT-FLSAMFAK = JA                                    
108900               PERFORM S02-HAEMTA-DEF-UPPG-SAMFAK                         
109000               PERFORM HD-UPPDAT-SAMFAK-ALL                               
109100             ELSE                                                         
109200               PERFORM HE-UPPDAT-UNIK-KUND                                
109300             END-IF                                                       
109400           END-IF                                                         
109500         END-IF                                                           
109600       END-IF                                                             
109610       IF MID-INPUT3 NOT = ALL '+'                                        
109611          PERFORM HF-UPPDAT-UNIK-KUND-TVSVOR                              
109620       END-IF                                                             
109630       IF MID-INPUT4 NOT = ALL '+'                                        
109640          PERFORM HG-UPPDAT-UNIK-KUND-PREPLAN                             
109650       END-IF                                                             
109700     ELSE                                                                 
109800       MOVE 'GOODS RECEIVER MISSING ' TO MOD-TEMFSFEL                     
109900       MOVE 'NOTHING UPDATED ' TO MOD-TEMFSINF                            
110000     END-IF                                                               
110100     .                                                                    
110200     EJECT                                                                
110300                                                                          
110400 HA-UPPDAT-ALLA-KUNDER SECTION.                                           
110500                                                                          
110600     MOVE ZERO TO W-IDKUNDNR-MIN                                          
110700     PERFORM IMS-GHN-WDB201-FIRST                                         
110800                                                                          
110900*    IF SEGMENT-FINNS                                                     
111000       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
111100               BAS-SLUT                                                   
111200                                                                          
111300         MOVE WS-FLAUTORD       TO NEXT-GMT-FLAUTORD                      
111400                                                                          
111500         MOVE WS-IDRFTAB        TO NEXT-GMT-IDRFTAB                       
111600                                                                          
111700         MOVE WS-FLNC           TO NEXT-GMT-FLNC                          
111800                                                                          
111900         MOVE WS-FLVR           TO NEXT-GMT-FLVR                          
112000                                                                          
112100         MOVE WS-KDORDING       TO NEXT-GMT-KDORDING                      
112200                                                                          
112300         MOVE WS-KVVECKOR-OB    TO NEXT-GMT-KVVECKOR-OB                   
112400                                                                          
112500         MOVE WS-FLURSRAP       TO NEXT-GMT-FLURSRAP                      
113300                                                                          
113400         MOVE WS-FLSAMFAK       TO NEXT-GMT-FLSAMFAK                      
113500                                                                          
113510         MOVE WS-FLSWCONS       TO NEXT-GMT-FLSWCONS                      
113520                                                                          
113600         MOVE WS-FLPRERS        TO NEXT-GMT-FLPRERS                       
113700                                                                          
113800         MOVE WS-FLFAKVKT       TO NEXT-GMT-FLFAKVKT                      
113900                                                                          
114000         MOVE WS-REAVDRAG       TO NEXT-GMT-REAVDRAG                      
114100                                                                          
114200         MOVE WS-FLFAKURS       TO NEXT-GMT-FLFAKURS                      
114300                                                                          
114400         MOVE WS-REEMBHNT       TO NEXT-GMT-REEMBHNT                      
114500                                                                          
114600         MOVE WS-KDSTATNR       TO NEXT-GMT-KDSTATNR                      
114700                                                                          
114800         MOVE WS-KDHBLKRV       TO NEXT-GMT-KDHBLKRV                      
114900                                                                          
115000         MOVE WS-KDSPRAK        TO NEXT-GMT-KDSPRAK                       
115100                                                                          
115200         MOVE WS-IDSKYLT        TO NEXT-GMT-IDSKYLT                       
115300                                                                          
115400         MOVE WS-IDPARTNR       TO NEXT-GMT-IDPARTNR                      
115401                                                                          
115600         MOVE WS-IDLEVNR        TO NEXT-GMT-IDLEVNR                       
115700                                                                          
115800         PERFORM IMS-REPL-WDB201-ALL                                      
115900                                                                          
116000         PERFORM IMS-GHN-WDB201                                           
116100       END-PERFORM                                                        
116200                                                                          
116300       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
116400       CALL WMEDKONV USING MED-WMEDAREA                                   
116500       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
116600       PERFORM MFS-FORM-ATTR                                              
116700       PERFORM MFS-RENSA-FAELT-IN                                         
116800*   * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                
116900*    END-IF                                                               
117000                                                                          
117100     .                                                                    
117200     EJECT                                                                
117300                                                                          
117400 HB-UPPDAT-ALLA-KUNDER SECTION.                                           
117500                                                                          
117600     MOVE ZERO TO W-IDKUNDNR-MIN                                          
117700     PERFORM IMS-GHN-WDB201-FIRST                                         
117800                                                                          
117900     IF SEGMENT-FINNS                                                     
118000       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
118100               BAS-SLUT                                                   
118200                                                                          
118300         MOVE WS-FLAUTORD       TO NEXT-GMT-FLAUTORD                      
118400                                                                          
118500         MOVE WS-IDRFTAB        TO NEXT-GMT-IDRFTAB                       
118600                                                                          
118700         MOVE WS-FLNC           TO NEXT-GMT-FLNC                          
118800                                                                          
118900         MOVE WS-FLVR           TO NEXT-GMT-FLVR                          
119000                                                                          
119100         MOVE WS-KDORDING       TO NEXT-GMT-KDORDING                      
119200                                                                          
119300         MOVE WS-KVVECKOR-OB    TO NEXT-GMT-KVVECKOR-OB                   
119400                                                                          
119500         MOVE WS-FLURSRAP       TO NEXT-GMT-FLURSRAP                      
120300                                                                          
120400         MOVE WS-FLSAMFAK       TO NEXT-GMT-FLSAMFAK                      
120500                                                                          
120510         MOVE WS-FLSWCONS       TO NEXT-GMT-FLSWCONS                      
120520                                                                          
120600         MOVE WS-FLPRERS        TO NEXT-GMT-FLPRERS                       
120700                                                                          
120800         IF NEXT-GMT-IDKUNDNR = SPAR-IDKUNDNR                             
120900           MOVE WS-FLFAKVKT     TO NEXT-GMT-FLFAKVKT                      
121000                                                                          
121100           MOVE WS-REAVDRAG     TO NEXT-GMT-REAVDRAG                      
121200                                                                          
121300           MOVE WS-FLFAKURS     TO NEXT-GMT-FLFAKURS                      
121400                                                                          
121500           MOVE WS-REEMBHNT     TO NEXT-GMT-REEMBHNT                      
121600                                                                          
121700           MOVE WS-KDSTATNR     TO NEXT-GMT-KDSTATNR                      
121800                                                                          
121900           MOVE WS-KDHBLKRV     TO NEXT-GMT-KDHBLKRV                      
122000                                                                          
122100           MOVE WS-KDSPRAK      TO NEXT-GMT-KDSPRAK                       
122200                                                                          
122300           MOVE WS-IDSKYLT      TO NEXT-GMT-IDSKYLT                       
122400                                                                          
122500           MOVE WS-IDPARTNR     TO NEXT-GMT-IDPARTNR                      
122501                                                                          
122700           MOVE WS-IDLEVNR      TO NEXT-GMT-IDLEVNR                       
122800         END-IF                                                           
122810                                                                          
122830         IF NEXT-GMT-IDKUNDNR = SPAR-IDKUNDNR-VTRANS                      
122831            MOVE WS-IDFTG       TO NEXT-GMT-IDFTG                         
122840         END-IF                                                           
122900                                                                          
123000         PERFORM IMS-REPL-WDB201-ALL                                      
123100                                                                          
123200         PERFORM IMS-GHN-WDB201                                           
123300       END-PERFORM                                                        
123400                                                                          
123500       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
123600       CALL WMEDKONV USING MED-WMEDAREA                                   
123700       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
123800       PERFORM MFS-FORM-ATTR                                              
123900       PERFORM MFS-RENSA-FAELT-IN                                         
124000*   * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                
124100     END-IF                                                               
124200                                                                          
124300     .                                                                    
124400     EJECT                                                                
124500                                                                          
124600 HC-UPPDAT-DISTR-INFO SECTION.                                            
124700                                                                          
124800     MOVE ZERO TO W-IDKUNDNR-MIN                                          
124900     PERFORM IMS-GHN-WDB201-FIRST                                         
125000                                                                          
125100     IF SEGMENT-FINNS                                                     
125200       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
125300               BAS-SLUT                                                   
125400                                                                          
125500         MOVE WS-FLAUTORD       TO NEXT-GMT-FLAUTORD                      
125600                                                                          
125700         MOVE WS-IDRFTAB        TO NEXT-GMT-IDRFTAB                       
125800                                                                          
125900         MOVE WS-FLNC           TO NEXT-GMT-FLNC                          
126000                                                                          
126100         MOVE WS-FLVR           TO NEXT-GMT-FLVR                          
126200                                                                          
126300         MOVE WS-KDORDING       TO NEXT-GMT-KDORDING                      
126400                                                                          
126500         MOVE WS-KVVECKOR-OB    TO NEXT-GMT-KVVECKOR-OB                   
126600                                                                          
126700         MOVE WS-FLURSRAP       TO NEXT-GMT-FLURSRAP                      
127500                                                                          
127600         MOVE WS-FLSAMFAK       TO NEXT-GMT-FLSAMFAK                      
127700                                                                          
127710         MOVE WS-FLSWCONS       TO NEXT-GMT-FLSWCONS                      
127720                                                                          
127800         MOVE WS-FLPRERS        TO NEXT-GMT-FLPRERS                       
127900                                                                          
128000         PERFORM IMS-REPL-WDB201-ALL                                      
128100                                                                          
128200         PERFORM IMS-GHN-WDB201                                           
128300       END-PERFORM                                                        
128400                                                                          
128500       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
128600       CALL WMEDKONV USING MED-WMEDAREA                                   
128700       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
128800       PERFORM MFS-RENSA-FAELT-IN                                         
128900       PERFORM MFS-FORM-ATTR                                              
129000* * * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                
129100     END-IF                                                               
129200                                                                          
129300     .                                                                    
129400     EJECT                                                                
129500                                                                          
129600 HD-UPPDAT-SAMFAK-ALL SECTION.                                            
129700                                                                          
129800     MOVE ZERO TO W-IDKUNDNR-MIN                                          
129900     PERFORM IMS-GHN-WDB201-FIRST                                         
130000     IF SEGMENT-FINNS                                                     
130100       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
130200               BAS-SLUT                                                   
130300                                                                          
130400         MOVE WS-FLFAKVKT       TO NEXT-GMT-FLFAKVKT                      
130500                                                                          
130600         MOVE WS-REAVDRAG       TO NEXT-GMT-REAVDRAG                      
130700                                                                          
130800         MOVE WS-FLFAKURS       TO NEXT-GMT-FLFAKURS                      
130900                                                                          
131000         MOVE WS-REEMBHNT       TO NEXT-GMT-REEMBHNT                      
131100                                                                          
131200         MOVE WS-KDSTATNR       TO NEXT-GMT-KDSTATNR                      
131300                                                                          
131400         MOVE WS-KDHBLKRV       TO NEXT-GMT-KDHBLKRV                      
131500                                                                          
131600         MOVE WS-KDSPRAK        TO NEXT-GMT-KDSPRAK                       
131700                                                                          
131800         MOVE WS-IDSKYLT        TO NEXT-GMT-IDSKYLT                       
131900                                                                          
132000         MOVE WS-IDPARTNR       TO NEXT-GMT-IDPARTNR                      
132001                                                                          
132200         MOVE WS-IDLEVNR        TO NEXT-GMT-IDLEVNR                       
132210                                                                          
132220         IF NEXT-GMT-IDKUNDNR = SPAR-IDKUNDNR-VTRANS                      
132230            MOVE WS-IDFTG       TO NEXT-GMT-IDFTG                         
132240         END-IF                                                           
132300                                                                          
132400         PERFORM IMS-REPL-WDB201-ALL                                      
132500                                                                          
132600         PERFORM IMS-GHN-WDB201                                           
132700       END-PERFORM                                                        
132800                                                                          
132900       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
133000       CALL WMEDKONV USING MED-WMEDAREA                                   
133100       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
133200       PERFORM MFS-FORM-ATTR                                              
133300       PERFORM MFS-RENSA-FAELT-IN                                         
133400*   * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                
133500     END-IF                                                               
133600     .                                                                    
133700     EJECT                                                                
133800                                                                          
133900 HE-UPPDAT-UNIK-KUND SECTION.                                             
134000                                                                          
134100     PERFORM IMS-GHU-WDB201                                               
134200                                                                          
134300     IF MID-FLFAKVKT NOT = ALL '+'                                        
134400       IF MID-FLFAKVKT = YES                                              
134500         MOVE JA                    TO GMTA-GMT-FLFAKVKT                  
134600       ELSE                                                               
134700         MOVE MID-FLFAKVKT          TO GMTA-GMT-FLFAKVKT                  
134800       END-IF                                                             
134900       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-FLFAKVKT-ATTR                  
135000     END-IF                                                               
135100                                                                          
135200     IF MID-REAVDRAG-IN NOT = ALL '+'                                     
135300       MOVE WS-REAVDRAG             TO GMTA-GMT-REAVDRAG                  
135400       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-REAVDRAG-IN-ATTR               
135500     END-IF                                                               
135600                                                                          
135700     IF MID-FLFAKURS NOT = ALL '+'                                        
135800       IF MID-FLFAKURS = YES                                              
135900         MOVE JA                    TO GMTA-GMT-FLFAKURS                  
136000       ELSE                                                               
136100         MOVE MID-FLFAKURS          TO GMTA-GMT-FLFAKURS                  
136200       END-IF                                                             
136300       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-FLFAKURS-ATTR                  
136400     END-IF                                                               
136500                                                                          
136600     IF MID-REEMBHNT-IN NOT = ALL '+'                                     
136700       MOVE WS-REEMBHNT             TO GMTA-GMT-REEMBHNT                  
136800       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-REEMBHNT-IN-ATTR               
136900     END-IF                                                               
137000                                                                          
137100     IF MID-KDSTATNR NOT = ALL '+'                                        
137200       MOVE MID-KDSTATNR            TO GMTA-GMT-KDSTATNR                  
137300       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KDSTATNR-ATTR                  
137400     END-IF                                                               
137500                                                                          
137600     IF MID-KDHBLKRV NOT = ALL '+'                                        
137700       MOVE MID-KDHBLKRV            TO GMTA-GMT-KDHBLKRV                  
137800       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KDHBLKRV-ATTR                  
137900     END-IF                                                               
138000                                                                          
138100     IF MID-KDSPRAK NOT = ALL '+'                                         
138200       MOVE MID-KDSPRAK             TO GMTA-GMT-KDSPRAK                   
138300       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-KDSPRAK-ATTR                   
138400     END-IF                                                               
138500                                                                          
138600     IF MID-IDSKYLT-IN NOT = ALL '+'                                      
138700       MOVE MID-IDSKYLT-IN          TO GMTA-GMT-IDSKYLT                   
138800       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDSKYLT-IN-ATTR                
138900     END-IF                                                               
139000                                                                          
139100     IF MID-IDPARTNR NOT = ALL '+'                                        
139200       MOVE MID-IDPARTNR            TO GMTA-GMT-IDPARTNR                  
139300       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDPARTNR-ATTR                  
139400     END-IF                                                               
139500                                                                          
139510     IF MID-IDFTG NOT = ALL '+'                                           
139520       MOVE MID-IDFTG               TO GMTA-GMT-IDFTG                     
139530       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDFTG-ATTR                     
139540     END-IF                                                               
139550                                                                          
139600     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
139700       MOVE MID-IDLEVNR-IN          TO GMTA-GMT-IDLEVNR                   
139800       MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-IDLEVNR-IN-ATTR                
139900     END-IF                                                               
140000                                                                          
140100     PERFORM IMS-REPL-WDB201                                              
140200                                                                          
140300     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
140400     CALL WMEDKONV USING MED-WMEDAREA                                     
140500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
140600     PERFORM MFS-FORM-ATTR                                                
140700     PERFORM MFS-RENSA-FAELT-IN                                           
140800*   * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                
140801     .                                                                    
140802 HF-UPPDAT-UNIK-KUND-TVSVOR SECTION.                                      
140803                                                                          
140805     PERFORM IMS-GHU-WDB201                                               
140806                                                                          
140807     MOVE +1 TO IDEX                                                      
140808     PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                                 
140809       IF MID-IDDC-TVSVOR (IDEX) NOT = ALL '+'                            
140810          MOVE MID-IDDC-TVSVOR (IDEX)                                     
140811                             TO GMTA-GMT-IDDC-TVSVOR (IDEX)               
140812        END-IF                                                            
140813        ADD +1 TO IDEX                                                    
140814     END-PERFORM                                                          
140820                                                                          
140830     PERFORM IMS-REPL-WDB201                                              
140840                                                                          
140850     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
140860     CALL WMEDKONV USING MED-WMEDAREA                                     
140870     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
140880     PERFORM MFS-FORM-ATTR                                                
140890     PERFORM MFS-RENSA-FAELT-IN                                           
140900     .                                                                    
141000     EJECT                                                                
141100                                                                          
141110 HG-UPPDAT-UNIK-KUND-PREPLAN SECTION.                                     
141120                                                                          
141130     PERFORM IMS-GHU-WDB201                                               
141140                                                                          
141150     MOVE +1 TO IDEX                                                      
141160     PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                                
141170       IF MID-IDDC-PREPLAN (IDEX) NOT = ALL '+'                           
141180          MOVE MID-IDDC-PREPLAN (IDEX)                                    
141190                             TO GMTA-GMT-IDDC-PREPLAN (IDEX)              
141191        END-IF                                                            
141192        ADD +1 TO IDEX                                                    
141193     END-PERFORM                                                          
141194                                                                          
141195     PERFORM IMS-REPL-WDB201                                              
141196                                                                          
141197     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
141198     CALL WMEDKONV USING MED-WMEDAREA                                     
141199     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
141200     PERFORM MFS-FORM-ATTR                                                
141201     PERFORM MFS-RENSA-FAELT-IN                                           
141202     .                                                                    
141203     EJECT                                                                
141204                                                                          
141210 S01-HAEMTA-DEF-UPPG-DISTR SECTION.                                       
141300                                                                          
141400     IF MID-FLAUTORD NOT = ALL '+'                                        
141500       IF MID-FLAUTORD = YES                                              
141600         MOVE JA                TO WS-FLAUTORD                            
141700       ELSE                                                               
141800         MOVE MID-FLAUTORD      TO WS-FLAUTORD                            
141900       END-IF                                                             
142000     ELSE                                                                 
142100       MOVE GMTA-GMT-FLAUTORD   TO WS-FLAUTORD                            
142200     END-IF                                                               
142300                                                                          
142400     IF MID-IDRFTAB NOT = ALL '+'                                         
142500       MOVE MID-IDRFTAB             TO WS-IDRFTAB                         
142600     ELSE                                                                 
142700       MOVE GMTA-GMT-IDRFTAB        TO WS-IDRFTAB                         
142800     END-IF                                                               
142900                                                                          
143000     IF MID-FLNC NOT = ALL '+'                                            
143100       IF MID-FLNC = YES                                                  
143200         MOVE JA                    TO WS-FLNC                            
143300       ELSE                                                               
143400         MOVE MID-FLNC              TO WS-FLNC                            
143500       END-IF                                                             
143600     ELSE                                                                 
143700       MOVE GMTA-GMT-FLNC           TO WS-FLNC                            
143800     END-IF                                                               
143900                                                                          
144000     IF MID-FLVR NOT = ALL '+'                                            
144100       IF MID-FLVR = YES                                                  
144200         MOVE JA                    TO WS-FLVR                            
144300       ELSE                                                               
144400         MOVE MID-FLVR              TO WS-FLVR                            
144500       END-IF                                                             
144600     ELSE                                                                 
144700       MOVE GMTA-GMT-FLVR           TO WS-FLVR                            
144800     END-IF                                                               
144900                                                                          
145000     IF MID-KDORDING NOT = ALL '+'                                        
145100       MOVE MID-KDORDING            TO WS-KDORDING                        
145200     ELSE                                                                 
145300       MOVE GMTA-GMT-KDORDING       TO WS-KDORDING                        
145400     END-IF                                                               
145500                                                                          
145600     IF MID-KVVECKOR-OB-IN NOT = ALL '+'                                  
145700       MOVE MID-KVVECKOR-OB-IN      TO WS-KVVECKOR-OB                     
145800     ELSE                                                                 
145900       MOVE GMTA-GMT-KVVECKOR-OB    TO WS-KVVECKOR-OB                     
146000     END-IF                                                               
146100                                                                          
146200     IF MID-FLURSRAP NOT = ALL '+'                                        
146300       IF MID-FLURSRAP = YES                                              
146400         MOVE JA                    TO WS-FLURSRAP                        
146500       ELSE                                                               
146600         MOVE MID-FLURSRAP          TO WS-FLURSRAP                        
146700       END-IF                                                             
146800     ELSE                                                                 
146900       MOVE GMTA-GMT-FLURSRAP       TO WS-FLURSRAP                        
147000     END-IF                                                               
147100                                                                          
148400     IF MID-FLSAMFAK NOT = ALL '+'                                        
148500       IF MID-FLSAMFAK = YES                                              
148600         MOVE JA                    TO WS-FLSAMFAK                        
148700       ELSE                                                               
148800         MOVE MID-FLSAMFAK          TO WS-FLSAMFAK                        
148900       END-IF                                                             
149000     ELSE                                                                 
149100       MOVE GMTA-GMT-FLSAMFAK       TO WS-FLSAMFAK                        
149200     END-IF                                                               
149300                                                                          
149310     IF MID-FLSWCONS NOT = ALL '+'                                        
149320       IF MID-FLSWCONS = YES                                              
149330         MOVE JA                    TO WS-FLSWCONS                        
149340       ELSE                                                               
149350         MOVE MID-FLSWCONS          TO WS-FLSWCONS                        
149360       END-IF                                                             
149370     ELSE                                                                 
149380       MOVE GMTA-GMT-FLSWCONS       TO WS-FLSWCONS                        
149390     END-IF                                                               
149391                                                                          
149400     IF MID-FLPRERS  NOT = ALL '+'                                        
149500       IF MID-FLPRERS  = YES                                              
149600         MOVE JA                TO WS-FLPRERS                             
149700       ELSE                                                               
149800         MOVE MID-FLPRERS       TO WS-FLPRERS                             
149900       END-IF                                                             
150000     ELSE                                                                 
150100       MOVE GMTA-GMT-FLPRERS    TO WS-FLPRERS                             
150200     END-IF                                                               
150300     .                                                                    
150400     EJECT                                                                
150500                                                                          
150600 S02-HAEMTA-DEF-UPPG-SAMFAK SECTION.                                      
150700                                                                          
150800     IF MID-FLFAKVKT NOT = ALL '+'                                        
150900       IF MID-FLFAKVKT = YES                                              
151000         MOVE JA                    TO WS-FLFAKVKT                        
151100       ELSE                                                               
151200         MOVE MID-FLFAKVKT          TO WS-FLFAKVKT                        
151300       END-IF                                                             
151400     ELSE                                                                 
151500       MOVE GMTA-GMT-FLFAKVKT       TO WS-FLFAKVKT                        
151600     END-IF                                                               
151700                                                                          
151800     IF MID-REAVDRAG-IN NOT = ALL '+'                                     
151900       CONTINUE                                                           
152000     ELSE                                                                 
152100       MOVE GMTA-GMT-REAVDRAG       TO WS-REAVDRAG                        
152200     END-IF                                                               
152300                                                                          
152400     IF MID-FLFAKURS NOT = ALL '+'                                        
152500       IF MID-FLFAKURS = YES                                              
152600         MOVE JA                    TO WS-FLFAKURS                        
152700       ELSE                                                               
152800         MOVE MID-FLFAKURS          TO WS-FLFAKURS                        
152900       END-IF                                                             
153000     ELSE                                                                 
153100       MOVE GMTA-GMT-FLFAKURS       TO WS-FLFAKURS                        
153200     END-IF                                                               
153300                                                                          
153400     IF MID-REEMBHNT-IN NOT = ALL '+'                                     
153500       CONTINUE                                                           
153600     ELSE                                                                 
153700       MOVE GMTA-GMT-REEMBHNT       TO WS-REEMBHNT                        
153800     END-IF                                                               
153900                                                                          
154000     IF MID-KDSTATNR NOT = ALL '+'                                        
154100       MOVE MID-KDSTATNR            TO WS-KDSTATNR                        
154200     ELSE                                                                 
154300       MOVE GMTA-GMT-KDSTATNR       TO WS-KDSTATNR                        
154400     END-IF                                                               
154500                                                                          
154600     IF MID-KDHBLKRV NOT = ALL '+'                                        
154700       MOVE MID-KDHBLKRV            TO WS-KDHBLKRV                        
154800     ELSE                                                                 
154900       MOVE GMTA-GMT-KDHBLKRV       TO WS-KDHBLKRV                        
155000     END-IF                                                               
155100                                                                          
155200     IF MID-KDSPRAK NOT = ALL '+'                                         
155300       MOVE MID-KDSPRAK             TO WS-KDSPRAK                         
155400     ELSE                                                                 
155500       MOVE GMTA-GMT-KDSPRAK        TO WS-KDSPRAK                         
155600     END-IF                                                               
155700                                                                          
155800     IF MID-IDSKYLT-IN NOT = ALL '+'                                      
155900       MOVE MID-IDSKYLT-IN          TO WS-IDSKYLT                         
156000     ELSE                                                                 
156100       MOVE GMTA-GMT-IDSKYLT        TO WS-IDSKYLT                         
156200     END-IF                                                               
156300                                                                          
156400     IF MID-IDPARTNR NOT = ALL '+'                                        
156500       MOVE MID-IDPARTNR            TO WS-IDPARTNR                        
156600     ELSE                                                                 
156700       MOVE GMTA-GMT-IDPARTNR       TO WS-IDPARTNR                        
156800     END-IF                                                               
156900                                                                          
157000     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
157100       MOVE MID-IDLEVNR-IN          TO WS-IDLEVNR                         
157200     ELSE                                                                 
157300       MOVE GMTA-GMT-IDLEVNR        TO WS-IDLEVNR                         
157400     END-IF                                                               
157500     .                                                                    
157600     EJECT                                                                
157700                                                                          
157710 S03-KOLLA-BETALARE SECTION.                                              
157720                                                                          
157730     PERFORM IMS-GHU-WDB201                                               
157740     IF SEGMENT-FINNS                                                     
157750       IF GMTA-GMT-IDFTG > ZERO                                           
157751         IF BETALARE-AENDRAD                                              
157752            MOVE WS-IDPARTNR       TO W-WDB1-IDPARTNR                     
157753         ELSE                                                             
157754            IF GMTA-GMT-IDPARTNR = SPACE                                  
157755               MOVE WS-IDPARTNR    TO W-WDB1-IDPARTNR                     
157756            ELSE                                                          
157760               MOVE GMTA-GMT-IDPARTNR TO W-WDB1-IDPARTNR                  
157761            END-IF                                                        
157762         END-IF                                                           
157763         IF FTG-AENDRAD                                                   
157770            MOVE WS-IDFTG          TO W-WDB1-IDFTG                        
157771         ELSE                                                             
157772            MOVE GMTA-GMT-IDFTG    TO W-WDB1-IDFTG                        
157773         END-IF                                                           
157780                                                                          
157790         PERFORM IMS-GU-WDB101                                            
157791         IF SEGMENT-SAKNAS                                                
157792            MOVE MFS-NUM-FAELT-FEL TO MOD-IDPARTNR-ATTR                   
157793            MOVE NEJ TO INDATA-SW                                         
157794         ELSE                                                             
157795            IF GMTA-GMT-TISTADAT > 0                                      
157796            AND GMTA-GMT-FLOKFAK-R = 'J'                                  
157797            AND (BETC-BET-IDPARTNR = SPACE                                
157798            OR BETC-BET-IDFTG   = SPACE)                                  
157799              MOVE MFS-NUM-FAELT-FEL TO MOD-IDPARTNR-ATTR                 
157800              MOVE NEJ TO INDATA-SW                                       
157801            END-IF                                                        
157802            IF BETC-BET-IDPROMR = SPACE                                   
157803            OR BETC-BET-KDVALISO = SPACE                                  
157804              MOVE MFS-NUM-FAELT-FEL TO MOD-IDPARTNR-ATTR                 
157805              MOVE NEJ TO INDATA-SW                                       
157806            END-IF                                                        
157807         END-IF                                                           
157808       ELSE                                                               
157809         IF BETALARE-AENDRAD                                              
157810            MOVE MFS-NUM-FAELT-FEL TO MOD-IDPARTNR-ATTR                   
157811            MOVE NEJ TO INDATA-SW                                         
157812         END-IF                                                           
157813       END-IF                                                             
157814     END-IF                                                               
157815     .                                                                    
157816                                                                          
157920     EJECT                                                                
157922                                                                          
157923 S04-HAEMTA-DEF-UPPG-KUND SECTION.                                        
157924                                                                          
157925     IF MID-FLFAKVKT NOT = ALL '+'                                        
157926       IF MID-FLFAKVKT = YES                                              
157927         MOVE JA                    TO WS-FLFAKVKT                        
157928       ELSE                                                               
157929         MOVE MID-FLFAKVKT          TO WS-FLFAKVKT                        
157930       END-IF                                                             
157931     ELSE                                                                 
157932       MOVE GMTA-GMT-FLFAKVKT       TO WS-FLFAKVKT                        
157933     END-IF                                                               
157934                                                                          
157935     IF MID-REAVDRAG-IN NOT = ALL '+'                                     
157936       CONTINUE                                                           
157937     ELSE                                                                 
157938       MOVE GMTA-GMT-REAVDRAG       TO WS-REAVDRAG                        
157939     END-IF                                                               
157940                                                                          
157941     IF MID-FLFAKURS NOT = ALL '+'                                        
157942       IF MID-FLFAKURS = YES                                              
157943         MOVE JA                    TO WS-FLFAKURS                        
157944       ELSE                                                               
157945         MOVE MID-FLFAKURS          TO WS-FLFAKURS                        
157946       END-IF                                                             
157947     ELSE                                                                 
157948       MOVE GMTA-GMT-FLFAKURS       TO WS-FLFAKURS                        
157949     END-IF                                                               
157950                                                                          
157951     IF MID-REEMBHNT-IN NOT = ALL '+'                                     
157952       CONTINUE                                                           
157953     ELSE                                                                 
157954       MOVE GMTA-GMT-REEMBHNT       TO WS-REEMBHNT                        
157955     END-IF                                                               
157956                                                                          
157957     IF MID-KDSTATNR NOT = ALL '+'                                        
157958       MOVE MID-KDSTATNR            TO WS-KDSTATNR                        
157959     ELSE                                                                 
157960       MOVE GMTA-GMT-KDSTATNR       TO WS-KDSTATNR                        
157961     END-IF                                                               
157962                                                                          
157963     IF MID-KDHBLKRV NOT = ALL '+'                                        
157964       MOVE MID-KDHBLKRV            TO WS-KDHBLKRV                        
157965     ELSE                                                                 
157966       MOVE GMTA-GMT-KDHBLKRV       TO WS-KDHBLKRV                        
157967     END-IF                                                               
157968                                                                          
157969     IF MID-KDSPRAK NOT = ALL '+'                                         
157970       MOVE MID-KDSPRAK             TO WS-KDSPRAK                         
157971     ELSE                                                                 
157972       MOVE GMTA-GMT-KDSPRAK        TO WS-KDSPRAK                         
157973     END-IF                                                               
157974                                                                          
157975     IF MID-IDSKYLT-IN NOT = ALL '+'                                      
157976       MOVE MID-IDSKYLT-IN          TO WS-IDSKYLT                         
157977     ELSE                                                                 
157978       MOVE GMTA-GMT-IDSKYLT        TO WS-IDSKYLT                         
157979     END-IF                                                               
157980                                                                          
157981     IF MID-IDPARTNR NOT = ALL '+'                                        
157982       MOVE MID-IDPARTNR            TO WS-IDPARTNR                        
157983     ELSE                                                                 
157984       MOVE GMTA-GMT-IDPARTNR       TO WS-IDPARTNR                        
157985     END-IF                                                               
157986                                                                          
157987     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
157988       MOVE MID-IDLEVNR-IN          TO WS-IDLEVNR                         
157989     ELSE                                                                 
157990       MOVE GMTA-GMT-IDLEVNR        TO WS-IDLEVNR                         
157991     END-IF                                                               
157992                                                                          
157993     .                                                                    
157994     EJECT                                                                
157995                                                                          
157996 MFS-RENSA-FAELT-UT SECTION.                                              
157997                                                                          
158000*    --- ALLA UTDATA-FÄLT                                                 
158100     MOVE MFS-RENSA-FAELT TO MOD-FLAUTORD                                 
158200                             MOD-IDRFTAB                                  
158300                             MOD-FLNC                                     
158400                             MOD-KDORDING                                 
158500                             MOD-FLVR                                     
158600                             MOD-KVVECKOR-OB-UT                           
158700                             MOD-FLURSRAP                                 
158800                             MOD-FLSAMFAK                                 
158810                             MOD-FLSWCONS                                 
158900                             MOD-FLPRERS                                  
159000                             MOD-FLFAKVKT                                 
159100                             MOD-REAVDRAG-UT                              
159200                             MOD-FLFAKURS                                 
159300                             MOD-REEMBHNT-UT                              
159400                             MOD-KDSTATNR                                 
159500                             MOD-KDHBLKRV                                 
159600                             MOD-KDSPRAK                                  
159700                             MOD-IDSKYLT-UT                               
159800                             MOD-IDPARTNR                                 
159810                             MOD-IDFTG                                    
159900                             MOD-IDLEVNR-UT                               
160000                                                                          
160100     MOVE +1 TO IDEX                                                      
160200     PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                                 
160300       MOVE MFS-RENSA-FAELT TO MOD-IDDC-TVSVOR (IDEX)                     
160400       ADD +1 TO IDEX                                                     
160500     END-PERFORM                                                          
160510     MOVE +1 TO IDEX                                                      
160520     PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                                
160530       MOVE MFS-RENSA-FAELT TO MOD-IDDC-PREPLAN (IDEX)                    
160540       ADD +1 TO IDEX                                                     
160550     END-PERFORM                                                          
160600     .                                                                    
160700     SKIP3                                                                
160800 MFS-RENSA-FAELT-IN SECTION.                                              
160900                                                                          
161000*    --- ALLA INDATA-FÄLT                                                 
161100     MOVE MFS-RENSA-FAELT TO MOD-FLAUTORD                                 
161200                             MOD-IDRFTAB                                  
161300                             MOD-FLNC                                     
161400                             MOD-KDORDING                                 
161500                             MOD-FLVR                                     
161600                             MOD-KVVECKOR-OB-IN                           
161700                             MOD-FLURSRAP                                 
161800                             MOD-FLSAMFAK                                 
161810                             MOD-FLSWCONS                                 
161900                             MOD-FLPRERS                                  
162000                             MOD-FLFAKVKT                                 
162100                             MOD-REAVDRAG-IN                              
162200                             MOD-FLFAKURS                                 
162300                             MOD-REEMBHNT-IN                              
162400                             MOD-KDSTATNR                                 
162500                             MOD-KDHBLKRV                                 
162600                             MOD-KDSPRAK                                  
162700                             MOD-IDSKYLT-IN                               
162800                             MOD-IDPARTNR                                 
162810                             MOD-IDFTG                                    
162900                             MOD-IDLEVNR-IN                               
163000                                                                          
163100     MOVE +1 TO IDEX                                                      
163200     PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                                 
163300       MOVE MFS-RENSA-FAELT TO MOD-IDDC-TVSVOR (IDEX)                     
163400       ADD +1 TO IDEX                                                     
163500     END-PERFORM                                                          
163510     MOVE +1 TO IDEX                                                      
163520     PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                                
163530       MOVE MFS-RENSA-FAELT TO MOD-IDDC-PREPLAN (IDEX)                    
163540       ADD +1 TO IDEX                                                     
163550     END-PERFORM                                                          
163600     .                                                                    
163700     EJECT                                                                
163800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
163900                                                                          
164000*    --- ALLA UTDATA-FÄLT                                                 
164100     MOVE MFS-ROER-EJ-FAELT TO MOD-FLAUTORD                               
164200                               MOD-IDRFTAB                                
164300                               MOD-FLNC                                   
164400                               MOD-KDORDING                               
164500                               MOD-FLVR                                   
164600                               MOD-KVVECKOR-OB-UT                         
164700                               MOD-FLURSRAP                               
164800                               MOD-FLSAMFAK                               
164810                               MOD-FLSWCONS                               
164900                               MOD-FLPRERS                                
165000                               MOD-FLFAKVKT                               
165100                               MOD-REAVDRAG-UT                            
165200                               MOD-FLFAKURS                               
165300                               MOD-REEMBHNT-UT                            
165400                               MOD-KDSTATNR                               
165500                               MOD-KDHBLKRV                               
165600                               MOD-KDSPRAK                                
165700                               MOD-IDSKYLT-UT                             
165800                               MOD-IDPARTNR                               
165810                               MOD-IDFTG                                  
165900                               MOD-IDLEVNR-UT                             
166000                                                                          
166100     MOVE +1 TO IDEX                                                      
166200     PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                                 
166300       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-TVSVOR (IDEX)                   
166400       ADD +1 TO IDEX                                                     
166500     END-PERFORM                                                          
166510     MOVE +1 TO IDEX                                                      
166520     PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                                
166530       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-PREPLAN (IDEX)                  
166540       ADD +1 TO IDEX                                                     
166550     END-PERFORM                                                          
166600     .                                                                    
166700     SKIP3                                                                
166800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
166900                                                                          
167000*    --- ALLA INDATA-FÄLT                                                 
167100     MOVE MFS-ROER-EJ-FAELT TO MOD-FLAUTORD                               
167200                               MOD-IDRFTAB                                
167300                               MOD-FLNC                                   
167400                               MOD-KDORDING                               
167500                               MOD-FLVR                                   
167600                               MOD-KVVECKOR-OB-IN                         
167700                               MOD-FLURSRAP                               
167800                               MOD-FLSAMFAK                               
167810                               MOD-FLSWCONS                               
167900                               MOD-FLPRERS                                
168000                               MOD-FLFAKVKT                               
168100                               MOD-REAVDRAG-IN                            
168200                               MOD-FLFAKURS                               
168300                               MOD-REEMBHNT-IN                            
168400                               MOD-KDSTATNR                               
168500                               MOD-KDHBLKRV                               
168600                               MOD-KDSPRAK                                
168700                               MOD-IDSKYLT-IN                             
168800                               MOD-IDPARTNR                               
168810                               MOD-IDFTG                                  
168900                               MOD-IDLEVNR-IN                             
169000                                                                          
169100     MOVE +1 TO IDEX                                                      
169200     PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                                 
169300       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-TVSVOR (IDEX)                   
169400       ADD +1 TO IDEX                                                     
169500     END-PERFORM                                                          
169510     MOVE +1 TO IDEX                                                      
169520     PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                                
169530       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-PREPLAN (IDEX)                  
169540       ADD +1 TO IDEX                                                     
169550     END-PERFORM                                                          
169600     .                                                                    
169700     EJECT                                                                
169800 MFS-FORM-ATTR SECTION.                                                   
169900                                                                          
170000*    --- ALLA INDATA-FÄLT                                                 
170100     MOVE MFS-FORMATETS-ATTR TO MOD-FLAUTORD-ATTR                         
170200                                MOD-IDRFTAB-ATTR                          
170300                                MOD-FLNC-ATTR                             
170400                                MOD-KDORDING-ATTR                         
170500                                MOD-FLVR-ATTR                             
170600                                MOD-KVVECKOR-OB-IN-ATTR                   
170700                                MOD-FLURSRAP-ATTR                         
170800                                MOD-FLSAMFAK-ATTR                         
170810                                MOD-FLSWCONS-ATTR                         
170900                                MOD-FLPRERS-ATTR                          
171000                                MOD-FLFAKVKT-ATTR                         
171100                                MOD-REAVDRAG-IN-ATTR                      
171200                                MOD-FLFAKURS-ATTR                         
171300                                MOD-REEMBHNT-IN-ATTR                      
171400                                MOD-KDSTATNR-ATTR                         
171500                                MOD-KDHBLKRV-ATTR                         
171600                                MOD-KDSPRAK-ATTR                          
171700                                MOD-IDSKYLT-IN-ATTR                       
171800                                MOD-IDPARTNR-ATTR                         
171810                                MOD-IDFTG-ATTR                            
171900                                MOD-IDLEVNR-IN-ATTR                       
172000                                                                          
172100     MOVE +1 TO IDEX                                                      
172200     PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                                 
172300       MOVE MFS-FORMATETS-ATTR TO MOD-IDDC-TVSVOR-ATTR (IDEX)             
172400       ADD +1 TO IDEX                                                     
172500     END-PERFORM                                                          
172510     MOVE +1 TO IDEX                                                      
172520     PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                                
172530       MOVE MFS-FORMATETS-ATTR TO MOD-IDDC-PREPLAN-ATTR (IDEX)            
172540       ADD +1 TO IDEX                                                     
172550     END-PERFORM                                                          
172600     .                                                                    
172700     EJECT                                                                
172800* --- IMS SEKTIONER ---                                                   
172900     SKIP3                                                                
173000 IMS-GET-MSG SECTION.                                                     
173100                                                                          
173200     MOVE '  QC' TO GODK-STATUSKODER                                      
173300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
173400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
173500     PERFORM IMS-STATUSKONTROLL                                           
173600     .                                                                    
173700     SKIP3                                                                
173800 IMS-INSERT-MSG SECTION.                                                  
173900                                                                          
174000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
174100       MOVE 'N' TO MFS-KDHUVOMR                                           
174200     END-IF                                                               
174300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
174400     MOVE SPACE TO GODK-STATUSKODER                                       
174500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
174600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
174700     PERFORM IMS-STATUSKONTROLL                                           
174800     .                                                                    
174900     EJECT                                                                
175000 IMS-GHU-WDB201 SECTION.                                                  
175100                                                                          
175200     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
175300          DELIMITED BY SIZE INTO SSA1                                     
175400     MOVE '  GE' TO GODK-STATUSKODER                                      
175500     CALL CBLTDLI USING GHU GMTA-PCB DLI-IO-WDB2 SSA1                     
175600     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
175700     PERFORM IMS-STATUSKONTROLL                                           
175800     .                                                                    
175900     SKIP3                                                                
176000 IMS-GHN-WDB201-FIRST SECTION.                                            
176100                                                                          
176200     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
176300                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
176400          DELIMITED BY SIZE INTO SSA1                                     
176500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
176600     CALL CBLTDLI USING GHU GMTA-PCB DLI-IO-AREA-WDB2-NEXT SSA1           
176700     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
176800     PERFORM IMS-STATUSKONTROLL                                           
176900     .                                                                    
177000     SKIP3                                                                
177100 IMS-GHN-WDB201 SECTION.                                                  
177200                                                                          
177300     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
177400                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
177500          DELIMITED BY SIZE INTO SSA1                                     
177600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
177700     CALL CBLTDLI USING GHN GMTA-PCB DLI-IO-AREA-WDB2-NEXT SSA1           
177800     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
177900     PERFORM IMS-STATUSKONTROLL                                           
178000     .                                                                    
178100     SKIP3                                                                
178200 IMS-REPL-WDB201 SECTION.                                                 
178300                                                                          
178400     MOVE '  ' TO GODK-STATUSKODER                                        
178500     CALL CBLTDLI USING REPL GMTA-PCB DLI-IO-WDB2                         
178600     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
178700     PERFORM IMS-STATUSKONTROLL                                           
178800     .                                                                    
178900     SKIP3                                                                
179000 IMS-REPL-WDB201-ALL SECTION.                                             
179100                                                                          
179200     MOVE '  ' TO GODK-STATUSKODER                                        
179300     CALL CBLTDLI USING REPL GMTA-PCB DLI-IO-AREA-WDB2-NEXT               
179400     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
179500     PERFORM IMS-STATUSKONTROLL                                           
179600     .                                                                    
179700     EJECT                                                                
179800 IMS-GU-XXKM11 SECTION.                                                   
179900                                                                          
180000     STRING 'WLXXKM01(WDGXKEY  =' W-WDGX4455-X ')'                        
180100          DELIMITED BY SIZE INTO SSA1                                     
180200     STRING 'WLXXKM11(WDGXKEY  =' W-WDGX4456-X ')'                        
180300          DELIMITED BY SIZE INTO SSA2                                     
180400     MOVE '  GE' TO GODK-STATUSKODER                                      
180500     CALL CBLTDLI USING GU XXKM-PCB DLI-IO-AREA-XXKM SSA1 SSA2            
180600     MOVE XXKM-STATUS-CODE TO STATUS-WS                                   
180700     PERFORM IMS-STATUSKONTROLL                                           
180800     .                                                                    
180900     SKIP3                                                                
181000 IMS-GU-WDB101 SECTION.                                                   
181100                                                                          
181200     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
181300          DELIMITED BY SIZE INTO SSA1                                     
181400     MOVE '  GE' TO GODK-STATUSKODER                                      
181500     CALL CBLTDLI USING GU BETC-PCB DLI-IO-AREA-WDB1 SSA1                 
181600     MOVE BETC-STATUS-CODE TO STATUS-WS                                   
181700     PERFORM IMS-STATUSKONTROLL                                           
181800     .                                                                    
181900     SKIP3                                                                
182000 IMS-GU-WDF101 SECTION.                                                   
182100                                                                          
182200     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
182300          DELIMITED BY SIZE INTO SSA1                                     
182400     MOVE '  GE' TO GODK-STATUSKODER                                      
182500     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA-WDF1 SSA1                 
182600     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
182700     PERFORM IMS-STATUSKONTROLL                                           
182800     .                                                                    
182900     SKIP3                                                                
183000 IMS-GU-GMTB-WDB301 SECTION.                                              
183100                                                                          
183200     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-DEF-X ')'                    
183300          DELIMITED BY SIZE INTO SSA1                                     
183400     MOVE '  GE'              TO GODK-STATUSKODER                         
183500     CALL CBLTDLI USING GU GMTB-PCB DLI-IO-AREA-B3 SSA1                   
183600     MOVE GMTB-STATUS-CODE    TO STATUS-WS                                
183700     PERFORM IMS-STATUSKONTROLL                                           
183800     .                                                                    
183900     SKIP3                                                                
184000 IMS-GU-WDB601    SECTION.                                                
184100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
184200          DELIMITED BY SIZE INTO SSA1                                     
184300     MOVE '  GE' TO GODK-STATUSKODER                                      
184400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
184500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
184600     PERFORM IMS-STATUSKONTROLL                                           
184700     IF SEGMENT-SAKNAS                                                    
184800         MOVE SPACE TO DCS-KDDC                                           
184900     END-IF                                                               
185000     .                                                                    
185010 IMS-GU-WDB617 SECTION.                                                   
185030     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
185040          DELIMITED BY SIZE INTO SSA1                                     
185050     STRING 'WDB617  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
185060          DELIMITED BY SIZE INTO SSA2                                     
185070     MOVE '  GE' TO GODK-STATUSKODER                                      
185080     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB617 SSA1 SSA2               
185090     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
185091     PERFORM IMS-STATUSKONTROLL                                           
185092     .                                                                    
185093     SKIP3                                                                
185100 IMS-STATUSKONTROLL SECTION.                                              
185200                                                                          
185300     SET STATUS-IX TO 1                                                   
185400     SEARCH GODK-STATUS                                                   
185500       AT END                                                             
185600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
185700         DELIMITED BY SIZE INTO FELTEXT                                   
185800         CALL FELLOG                                                      
185900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
186000         CONTINUE                                                         
186100     END-SEARCH                                                           
186200     .                                                                    

000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9020100.                                                
000300 AUTHOR.         RONNY STENHOLM                                           
000400 DATE-WRITTEN.   MARS -96                                                 
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*        TILLGÄNGLIGHETSFRÅGA - VDI.                                      
000900*        QW-90 STÄLLER PRISFRÅGOR.                                        
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSAKTION: W90201T                                             
001300*        MID:         W9I20101                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        MOD:         W9O20101                                            
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP3                                                                
002000 DATA DIVISION.                                                           
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77    IDPGM                     PIC X(8)    VALUE 'W9020100'.            
002700 77    JA                        PIC X       VALUE 'J'.                   
002800 77    NEJ                       PIC X       VALUE 'N'.                   
002900 77    STORT-UTTAG               PIC X(3)    VALUE '075'.                 
003000 77    IX                        PIC S9(3)   VALUE +0   COMP-3.           
003100 77    RAD-IX                    PIC S9(3)   VALUE +0   COMP-3.           
003200 77    MOD-IX                    PIC S9(3)   VALUE +0   COMP-3.           
003300 77    RAD-MAX-PLUS-1            PIC S9(3)   VALUE +50  COMP-3.           
003400 77    HUVUD-MOD-LAENGD          PIC S9(4)   VALUE +50  COMP SYNC.        
003500 77    SW-ALLT-OK                PIC X       VALUE 'J'.                   
003600 77    SPAR-ARTC-KDERS           PIC S9(3)   VALUE +0   COMP-3.           
003700 77    SPAR-REDIRLEV             PIC S9V9(2) VALUE +0   COMP-3.           
003800 77    W-DISP                    PIC S9(8)   VALUE +0   COMP-3.           
003900 77    W-DISP-NUM                PIC 9(6)    VALUE ZERO.                  
004000 77    MFS-IDMOD                 PIC X(8).                                
004100                                                                          
004200 77    ARTC01-STATUS             PIC X      VALUE 'N'.                    
004300   88  ARTC01-FINNS                         VALUE 'J'.                    
004400   88  ARTC01-SAKNAS                        VALUE 'N'.                    
004500                                                                          
004600 77    ARTC11-STATUS             PIC X      VALUE 'N'.                    
004700   88  ARTC11-FINNS                         VALUE 'J'.                    
004800   88  ARTC11-SAKNAS                        VALUE 'N'.                    
004900                                                                          
005000 77  WS-IDMFSMED                 PIC X(3)   VALUE SPACE.                  
005100   88  DLEV                                 VALUE '095'.                  
005200                                                                          
005300 01  WS-MED                      PIC 9(3).                                
005400     EJECT                                                                
005500*      --- VALID IDDC CODES                                               
005600*                                                                         
005700*01    -COPY WWDCKONS                                                     
005800       EJECT                                                              
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000   03  W335PRIS                  PIC X(8)   VALUE 'W335PRIS'.             
006100   03  CBLTDLI                   PIC X(8)   VALUE 'CBLTDLI '.             
006200   03  FELLOG                    PIC X(8)   VALUE 'FELLOG  '.             
006300     EJECT                                                                
006400*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
006500*                                                                         
006600 01  FILLER                  PIC X(16) VALUE 'PRISTILL-AREA'.             
006700*01  PRIS-AREA  -COPY W335PRIS                                            
006800     EJECT                                                                
006900 01    NYCKLAR-TILL-DLI.                                                  
007000   03    W-IDARTNR-X.                                                     
007100     05    W-IDARTNR             PIC S9(9)   VALUE ZERO  COMP-3.          
007200*                                                                         
007300   03  KEY-DIST-KUND.                                                     
007400     05  W-IDDISTR-X.                                                     
007500       07  W-IDDISTR             PIC S9(5)   VALUE ZERO  COMP-3.          
007600                                                                          
007700     05  W-IDKUNDNR-X.                                                    
007800       07  W-IDKUNDNR            PIC S9(7)   VALUE ZERO  COMP-3.          
007900                                                                          
008000   03  W-IDGMT-MAX-X.                                                     
008100     05  W-IDDISTR-B2-MAX        PIC S9(5)   VALUE ZERO COMP-3.           
008200     05  W-IDKUNDNR-B2-MAX       PIC S9(7)                                
008300                                      VALUE 9999999  COMP-3.              
008400                                                                          
008500   03  W-IDGMT-MIN-X.                                                     
008600     05  W-IDDISTR-B2-MIN        PIC S9(5)   VALUE ZERO COMP-3.           
008700     05  W-IDKUNDNR-B2-MIN       PIC S9(7)   VALUE ZERO COMP-3.           
008800                                                                          
008900                                                                          
009000   03    W-IDDC-X.                                                        
009100     05  W-IDDC                  PIC  X(2)   VALUE SPACE.                 
009200*                                                                         
009300     EJECT                                                                
009400******************************************************************        
009500*                                                                         
009600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
009700*                                                                         
009800 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
009900     SKIP3                                                                
010000*01    MID -COPY W9I20101.                                                
010100     EJECT                                                                
010200*01    -COPY WMSGSNUF                                                     
010300     EJECT                                                                
010400    03    MOD -COPY W9O20101 -RED MSG-AREA.                               
010500     EJECT                                                                
010600*01    -COPY WMSGSPAR                                                     
010700     EJECT                                                                
010800******************************************************************        
010900*                                                                         
011000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011100*                                                                         
011200 01    IMS-WS.                                                            
011300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
011400     SKIP3                                                                
011500*                        **** STATUS-KOD FRÅN IMS                         
011600   03    STATUS-WS               PIC XX.                                  
011700     88    SEGMENT-FINNS                     VALUE '  '.                  
011800     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
011900     SKIP3                                                                
012000   03    GODK-STATUSKODER.                                                
012100     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
012200     SKIP3                                                                
012300 01    SSA1                      PIC X(64).                               
012400 01    SSA2                      PIC X(64).                               
012500     EJECT                                                                
012600*                            IMS FUNKTIONSKODER                           
012700*01    -COPY W0003                                                        
012800*                                                                         
012900*                                                                         
013000******************************************************************        
013100     EJECT                                                                
013200******************************************************************        
013300*                                                                         
013400*        ARBETS-AREOR TILL IO-AREORNA                                     
013500*                                                                         
013600*    ---  DLI INPUT-OUTPUT AREA 1                                         
013700*    ---  DLI-IO-AREA                                                     
013800                                                                          
013900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC01'.            
014000 01  DLI-IO-ARTC01.                                                       
014100*  03    WLARTC01 -COPY WDK601                                            
014200     EJECT                                                                
014300                                                                          
014400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-ARTC11'.            
014500 01  DLI-IO-ARTC11.                                                       
014600*  03    WLARTC11 -COPY WDK611                                            
014700     EJECT                                                                
014800                                                                          
014900 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-GMTA01'.            
015000 01  DLI-IO-GMTA01.                                                       
015100*  03    WLGMTA01 -COPY WDB201                                            
015200     EJECT                                                                
015300                                                                          
015400                                                                          
015500 LINKAGE SECTION.                                                         
015600                                                                          
015700*01    -COPY W0009     -PRE MSG-                                          
015800     EJECT                                                                
015900*01    -COPY W0008     -PRE ARTC-                                         
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016110*01    -COPY W0008     -PRE WDK7-                                         
016120     05  FILLER                  PIC X.                                   
016130     EJECT                                                                
016200*01  -COPY W0008       -PRE GMTA-                                         
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500*01  -COPY W0008       -PRE BETA-                                         
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008       -PRE GPRIA-                                        
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100*01  -COPY W0008       -PRE GPRIB-                                        
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017400 01  PRIS-COST-WDK6-PCB          PIC X.                                   
017500 01  PRIS-COST-WDK7-PCB          PIC X.                                   
017600 01  PRIS-COST-WDF1-PCB          PIC X.                                   
017700 01  PRIS-COST-9305-PCB          PIC X.                                   
017800 01  PRIS-COST-WDK72-PCB         PIC X.                                   
017900 01  PRIS-COST-WDB6-PCB          PIC X.                                   
018000 PROCEDURE DIVISION  USING MSG-PCB    ARTC-PCB WDK7-PCB                   
018100                           GMTA-PCB   BETA-PCB                            
018200                           GPRIA-PCB  GPRIB-PCB                           
018300                           PRIS-COST-WDK6-PCB                             
018400                           PRIS-COST-WDK7-PCB                             
018500                           PRIS-COST-WDF1-PCB                             
018600                           PRIS-COST-9305-PCB                             
018700                           PRIS-COST-WDK72-PCB                            
018710                           PRIS-COST-WDB6-PCB.                            
018800 MAIN SECTION.                                                            
018900     ENTRY 'DLITCBL' USING MSG-PCB    ARTC-PCB WDK7-PCB                   
019000                           GMTA-PCB   BETA-PCB                            
019100                           GPRIA-PCB  GPRIB-PCB                           
019200                           PRIS-COST-WDK6-PCB                             
019300                           PRIS-COST-WDK7-PCB                             
019400                           PRIS-COST-WDF1-PCB                             
019500                           PRIS-COST-9305-PCB                             
019600                           PRIS-COST-WDK72-PCB                            
019610                           PRIS-COST-WDB6-PCB.                            
019700     PERFORM IMS-GET-MSG                                                  
019800     IF  SEGMENT-FINNS                                                    
019900                                                                          
020000       PERFORM A-INIT-SPARA-INPUT                                         
020100       MOVE +1 TO RAD-IX                                                  
020200                  MOD-IX                                                  
020300       PERFORM UNTIL RAD-IX = RAD-MAX-PLUS-1                              
020400         IF MID-RAD (RAD-IX) NOT = ALL '0'                                
020500           PERFORM B-BEHANDLA-RAD                                         
020600           MOVE JA    TO SW-ALLT-OK                                       
020700         END-IF                                                           
020800         ADD +1 TO RAD-IX                                                 
020900       END-PERFORM                                                        
021000                                                                          
021100                                                                          
021200                                                                          
021300     COMPUTE MSG-KVLL = LENGTH OF MOD-W9O20101 + 4                        
021400     PERFORM IMS-INSERT-MSG                                               
021500                                                                          
021600     END-IF                                                               
021700                                                                          
021800     MOVE ZERO                         TO RETURN-CODE                     
021900                                                                          
022000     GOBACK                                                               
022100     .                                                                    
022200     EJECT                                                                
022300 A-INIT-SPARA-INPUT SECTION.                                              
022400                                                                          
022500     MOVE MSG-INDATA                   TO MID-W9I20101                    
022600     MOVE '1'                          TO MSG-KDMFSFOR                    
022700     MOVE ' '                          TO MSG-KDTRTYP                     
022800     MOVE LOW-VALUE                    TO MSG-AREA                        
022900     MOVE 'W0O50901'                   TO MFS-IDMOD                       
023000     MOVE '9201'                       TO MSG-IDTRANS                     
023100     INITIALIZE MOD-W9O20101                                              
023200     .                                                                    
023300     EJECT                                                                
023400 B-BEHANDLA-RAD SECTION.                                                  
023500                                                                          
023600     MOVE ZERO                        TO MOD-IDMFSFEL(RAD-IX)             
023700     MOVE MID-IDDISTR(RAD-IX)         TO MOD-IDDISTR(RAD-IX)              
023800     MOVE MID-IDKUNDNR(RAD-IX)        TO MOD-IDKUNDNR(RAD-IX)             
023900     MOVE MID-IDARTNR(RAD-IX)         TO MOD-IDARTNR(RAD-IX)              
024000                                                                          
024100                                                                          
024200     IF MID-IDDISTR(RAD-IX) NUMERIC AND                                   
024300        MID-IDKUNDNR(RAD-IX) NUMERIC                                      
024400                                                                          
024500       MOVE MID-IDDISTR(RAD-IX)        TO W-IDDISTR-B2-MAX                
024600                                          W-IDDISTR-B2-MIN                
024700       MOVE MID-IDKUNDNR (RAD-IX)      TO W-IDKUNDNR-B2-MIN               
024800       PERFORM IMS-GU-GMTA01                                              
024900       IF SEGMENT-FINNS                                                   
025000         CONTINUE                                                         
025100       ELSE                                                               
025200         MOVE NEJ   TO SW-ALLT-OK                                         
025300         MOVE '412'                    TO MOD-IDMFSFEL(RAD-IX)            
025400       END-IF                                                             
025500     ELSE                                                                 
025600       MOVE NEJ   TO SW-ALLT-OK                                           
025700       MOVE '114'                      TO MOD-IDMFSFEL(RAD-IX)            
025800     END-IF                                                               
025900     IF SW-ALLT-OK    = JA                                                
026000     PERFORM BA-INITIERA-FAELT                                            
026100       PERFORM BB-PRISTILLAMPA                                            
026200       IF  PRIS-KDSVAR = ' '                                              
026300         CONTINUE                                                         
026400       ELSE                                                               
026500         IF  PRIS-KDSVAR = '1'                                            
026600           MOVE '769'   TO MOD-IDMFSFEL(RAD-IX)                           
026700           MOVE NEJ   TO SW-ALLT-OK                                       
026800         ELSE                                                             
026900           MOVE '412'   TO MOD-IDMFSFEL(RAD-IX)                           
027000           MOVE NEJ   TO SW-ALLT-OK                                       
027100         END-IF                                                           
027200       END-IF                                                             
027300     END-IF                                                               
027400     .                                                                    
027500     EJECT                                                                
027600 BA-INITIERA-FAELT SECTION.                                               
027700                                                                          
027800     MOVE ZERO    TO WS-IDMFSMED                                          
027900                                                                          
028000                                                                          
028100                                                                          
028200     MOVE NEJ     TO ARTC01-STATUS                                        
028300                     ARTC11-STATUS                                        
028400                                                                          
028500     INSPECT MID-IDARTNR(RAD-IX) REPLACING LEADING SPACE BY ZERO          
028600     INSPECT MID-IDDISTR(RAD-IX) REPLACING LEADING SPACE BY ZERO          
028700                                                                          
028800     IF MID-IDARTNR(RAD-IX) NUMERIC    AND                                
028900        MID-IDDISTR(RAD-IX) NUMERIC                                       
029000       CONTINUE                                                           
029100     ELSE                                                                 
029200       MOVE '020' TO WS-IDMFSMED                                          
029300       MOVE '020'   TO MOD-IDMFSFEL(RAD-IX)                               
029400       MOVE NEJ   TO SW-ALLT-OK                                           
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 BB-PRISTILLAMPA SECTION.                                                 
029900                                                                          
030000     MOVE 1                              TO PRIS-KDCALL                   
030010     MOVE IDPGM                          TO PRIS-IDPGM                    
030100     MOVE MID-IDARTNR(RAD-IX)            TO PRIS-IDARTNR                  
030200     MOVE MID-IDDISTR(RAD-IX)            TO PRIS-IDDISTR                  
030300     MOVE ZERO                           TO PRIS-IDKUNDNR                 
030400     MOVE WC-CDC-SE                      TO PRIS-IDDC                     
030500     MOVE +4                             TO PRIS-KDORDKL                  
030600     MOVE +1                             TO PRIS-KVBEART                  
030700     MOVE SPACE                          TO PRIS-FLINVEST                 
030800                                                                          
030900     CALL W335PRIS USING PRIS-AREA                                        
031000                         ARTC-PCB                                         
031010                         WDK7-PCB                                         
031100                         GMTA-PCB                                         
031200                         BETA-PCB                                         
031300                         GPRIA-PCB                                        
031400                         GPRIB-PCB                                        
031500                         PRIS-COST-WDK6-PCB                               
031600                         PRIS-COST-WDK7-PCB                               
031700                         PRIS-COST-WDF1-PCB                               
031800                         PRIS-COST-9305-PCB                               
031900                         PRIS-COST-WDK72-PCB                              
031910                         PRIS-COST-WDB6-PCB                               
032300                                                                          
032400     MOVE PRIS-PRARTNTO            TO MOD-PRARTNTO     (RAD-IX)           
032500     MOVE PRIS-KDPRTYP             TO MOD-KDPRTYP       (RAD-IX)          
032600     MOVE PRIS-PRBPRIS             TO MOD-PRARTBTO-MARK (RAD-IX)          
032700     .                                                                    
032800     EJECT                                                                
032900                                                                          
033000* IMS SEKTIONER                                                           
033100     SKIP3                                                                
033200 IMS-GET-MSG SECTION.                                                     
033300                                                                          
033400     MOVE '  QC' TO GODK-STATUSKODER                                      
033500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA-SNUF                       
033600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
033700     PERFORM IMS-STATUSKONTROLL                                           
033800     .                                                                    
033900     SKIP3                                                                
034000 IMS-INSERT-MSG SECTION.                                                  
034100                                                                          
034200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
034300     MOVE SPACE TO GODK-STATUSKODER                                       
034400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA-SNUF                     
034500*    CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA-SNUF MFS-IDMOD           
034600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034700     PERFORM IMS-STATUSKONTROLL                                           
034800     .                                                                    
034900     EJECT                                                                
035000 IMS-GU-GMTA01 SECTION.                                                   
035100                                                                          
035200     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
035300                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
035400            DELIMITED BY SIZE INTO SSA1                                   
035500     MOVE '  GE' TO GODK-STATUSKODER                                      
035600     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-GMTA01 SSA1                    
035700     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
035800     PERFORM IMS-STATUSKONTROLL                                           
035900                                                                          
036000     .                                                                    
036100                                                                          
036200 IMS-STATUSKONTROLL SECTION.                                              
036300                                                                          
036400     SET STATUS-IX TO 1                                                   
036500     SEARCH GODK-STATUS                                                   
036600       AT END                                                             
036700         CALL FELLOG                                                      
036800     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
036900       CONTINUE                                                           
037000     END-SEARCH                                                           
037100     .                                                                    

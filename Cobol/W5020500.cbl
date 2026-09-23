000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5020500.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   11/12/19.                                                
000500 DATE-COMPILED.                                                           
000600*                                                                         
000700*    FUNCTION:                                                            
000800*        PROGRAM TO DISPLAY INVENTORY VALUE, ACCOUNTING                   
000900*        FOR A GIVEN ARTIKEL.                                             
001000*        (COPY OF W5020300)                                               
001100*                                                                         
001200*        THE PROGRAM READS     WLARTC (WDK6V)                             
001300*                              WLARTS (WDK7V)                             
001400*                              WLBENA (WDD3)                              
001500*                              WDB6                                       
001600*                              WDG2 (MONTHLY RATE VIA W510CURR)           
001700*                         WDGX9305 +  WDGX9306 + WDGX9308                 
001800*                              W335PRIS                                   
001900*                                                                         
002000*    INDATA.                                                              
002100*        TRANSAKTION: W5T205                                              
002200*        MID:         W5I20501                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W5O20501                                            
002600*                                                                         
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200*    -COPY WY2000W1                                                       
003300*                      *************************************              
003400*                      *** INNEHÅLLER RADIO OCH BYTES OBJEKT              
003500*                      ***                                                
003600*                      *** INNEHÅLLER ENDAST OBJEKT                       
003700*                      *************************************              
003800*01  FILLER -COPY WWBYT20                                                 
003900*                                                                         
004000     SKIP3                                                                
004100 77  IDPGM                       PIC X(08)   VALUE 'W5020500'.            
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  YES                         PIC X       VALUE 'J'.                   
004400 77  NOO                         PIC X       VALUE 'N'.                   
004500 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  W-IDDC-SE                   PIC X(2)    VALUE '11'.                  
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300 77  WS-PRICE-SW                 PIC X       VALUE 'N'.                   
005400     88  PRICE-FOUND                         VALUE 'J'.                   
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '5205'.                
005700     88  GODK-MID                            VALUE '5201' '5202'          
005800                                                   '5203' '5204'          
005900                                                   '5205' '5206'          
006000                                                   '5207' '5208'          
006100                                                   '5209'.                
006200     88  HELP-MID                            VALUE '0551'.                
006300 77  WS-REGDATUM-AADDD           PIC 9(5).                                
006400 01  W-KVAKS-PAV-01              PIC S9(7)   VALUE ZERO COMP-3.           
006500 01  W-KVAKS-SDC-01              PIC S9(7)   VALUE ZERO COMP-3.           
006600 01  W-KVLS-01                   PIC S9(7)   VALUE ZERO COMP-3.           
006700 01  W-KVEFRS-01                 PIC S9(7)   VALUE ZERO COMP-3.           
006800 01  W-KVUTRS-01                 PIC S9(7)   VALUE ZERO COMP-3.           
006900 01  W-KVAKS-PAV-02              PIC S9(7)   VALUE ZERO COMP-3.           
007000 01  W-KVAKS-SDC-02              PIC S9(7)   VALUE ZERO COMP-3.           
007100 01  W-KVLS-02                   PIC S9(7)   VALUE ZERO COMP-3.           
007200 01  W-KVEFRS-02                 PIC S9(7)   VALUE ZERO COMP-3.           
007300 01  W-KVUTRS-02                 PIC S9(7)   VALUE ZERO COMP-3.           
007400 01  W-KVAKS-PAV-03              PIC S9(7)   VALUE ZERO COMP-3.           
007500 01  W-KVAKS-SDC-03              PIC S9(7)   VALUE ZERO COMP-3.           
007600 01  W-KVLS-03                   PIC S9(7)   VALUE ZERO COMP-3.           
007700 01  W-KVEFRS-03                 PIC S9(7)   VALUE ZERO COMP-3.           
007800 01  W-KVUTRS-03                 PIC S9(7)   VALUE ZERO COMP-3.           
007900 01  W-KVAKS-PAV-04              PIC S9(7)   VALUE ZERO COMP-3.           
008000 01  W-KVAKS-SDC-04              PIC S9(7)   VALUE ZERO COMP-3.           
008100 01  W-KVLS-04                   PIC S9(7)   VALUE ZERO COMP-3.           
008200 01  W-KVEFRS-04                 PIC S9(7)   VALUE ZERO COMP-3.           
008300 01  W-KVUTRS-04                 PIC S9(7)   VALUE ZERO COMP-3.           
008400 01  W-VALUTA-FINNS              PIC X       VALUE 'Y'.                   
008500 01  W-GE-WDK711                 PIC X.                                   
008600 01  W-DAPRLIST-9KOMPL           PIC 9(8).                                
008700 01  W-PRKURS                    PIC S9(6)V9(5) VALUE ZERO.               
008800 01  W-PRKURS2                   PIC S9(6)V9(5) VALUE ZERO.               
008900 01  W-PRARTBEL                  PIC S9(8)V9(3) VALUE ZERO.               
009000 01  W-PRARTBEL-INR              PIC S9(8)V9(3) VALUE ZERO.               
009100 01  W-PRARTBEL-02               PIC S9(7)V9(2) VALUE ZERO.               
009200 01  W-FIRST-PRKURS              PIC S9(6)V9(5) VALUE ZERO.               
009300 01  W-DATE-AAMM                 PIC 9(4)       VALUE ZERO.               
009400 01  WS-KDVALISO-HUV             PIC X(3)       VALUE 'SEK'.              
009500 01  W-BYTE-ART                  PIC X       VALUE 'N'.                   
009600 01  W-IDARTNR-CHECK.                                                     
009700     03  W-IDARTNR-4             PIC 9(4)    VALUE ZERO.                  
009800     03  W-IDARTNR-5             PIC 9(5)    VALUE ZERO.                  
009900     03  WS-IDLEVNR               PIC X(5)    VALUE SPACE.                
010000     EJECT                                                                
010100 01  FILLER                      PIC X(50)                                
010200     VALUE '*****     ARBETSAREOR     *****'.                             
010300*                                                                         
010400 01  DIVERSE.                                                             
010500     03  WS-KDVALISO         PIC X(3)       VALUE SPACE.                  
010600     03  W-REVALUTA          PIC S9(5)      VALUE +0   COMP-3.            
010700     03  W-RETULF            PIC S9(3)V9(4) VALUE +0.                     
010800     03  DAGENS-AAMMDD       PIC 9(6).                                    
010900     03  W-PRARTBEL          PIC S9(8)V9(5) VALUE +0.                     
011000     03  WS-PRARTBES-PR      PIC S9(7)V9(2) VALUE +0   COMP-3.            
011100                                                                          
011200     03  WS-DATE-YYMMDD            PIC 9(06).                             
011300     03  WS-DATE-FIRST REDEFINES WS-DATE-YYMMDD.                          
011400         05  WS-DATE-YYMM          PIC 9(04).                             
011500         05  WS-DATE-DD            PIC 9(02).                             
011600                                                                          
011700 77  NON-VCCS-SW                 PIC X     VALUE 'N'.                     
011800     88  WH-YES                            VALUE 'J'.                     
011900     88  WH-NO                             VALUE 'N'.                     
012000 01  DATUM                       PIC X(8).                                
012100 01  W-DATUM.                                                             
012200     03 W-AAR                    PIC X(2).                                
012300     03 W-MAN                    PIC X(2).                                
012400     03 W-DAG                    PIC X(2).                                
012500*                                                                         
012600 01  W-AAAAMMDD-NUM              PIC 9(8).                                
012700 01  W-AAAAMMDD.                                                          
012800     03 W-AA-SEKEL               PIC X(2)    VALUE SPACE.                 
012900     03 W-AAMMDD.                                                         
013000        05 W-AA                  PIC X(2)    VALUE SPACE.                 
013100        05 W-MM                  PIC X(2)    VALUE SPACE.                 
013200        05 W-DD                  PIC X(2)    VALUE SPACE.                 
013300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013400 01  GENERELLA-SUBPROGRAM.                                                
013500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013900     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
014000     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
014100     03  W510WHSU                PIC X(8)    VALUE 'W510WHSU'.            
014200     EJECT                                                                
014300*    --- VALID IDDC CODES                                                 
014400*01 -COPY WWDC99                                                          
014500*01 -COPY WWDCKONS                                                        
014600*01 -COPY WWIDFTG                                                         
014700     SKIP3                                                                
014800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
014900*01 -COPY WMEDAREA                                                        
015000     SKIP3                                                                
015100 01  MESSAGE-CODES.                                                       
015200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015300     03  INF-PART-MISSING        PIC X(3)    VALUE '017'.                 
015400     03  INF-PART-INFO-MISSING   PIC X(3)    VALUE '303'.                 
015500     03  WRONG-DC                PIC X(3)    VALUE '440'.                 
015600     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
015700     EJECT                                                                
015800*01  -COPY W335PRIS                                                       
015900     EJECT                                                                
016000*01  -COPY W510CURR                                                       
016100     EJECT                                                                
016200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016300*                                                                         
016400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016500     SKIP3                                                                
016600*01 -COPY WMSGINIT                                                        
016700 01  FILLER                    PIC X(24)   VALUE 'W510WHSU-AREA'.         
016800*01  -COPY W510WHSU                                                       
016900     EJECT                                                                
017000     EJECT                                                                
017100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017200*                                                                         
017300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017400     SKIP3                                                                
017500*01  MID -COPY W5I20501                                                   
017600     EJECT                                                                
017700 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
017800     SKIP3                                                                
017900*01  -COPY WMSGAREA                                                       
018000     EJECT                                                                
018100     03  MOD REDEFINES MSG-AREA.                                          
018200*      05  -COPY W5O20501                                                 
018300     EJECT                                                                
018400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018500     SKIP3                                                                
018600*01  -COPY WMFSAREA                                                       
018700     EJECT                                                                
018800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018900*                                                                         
019000     EJECT                                                                
019100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019200     SKIP3                                                                
019300 01  NYCKLAR-TILL-DLI.                                                    
019400     03  W-IDARTNR-X.                                                     
019500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019600     03  W-KDSEGKY-X.                                                     
019700         05  W-KDSEGKY           PIC S9(1)   VALUE ZERO COMP-3.           
019800     03  W-WDK621KY-X.                                                    
019900         05  W-WDK621KY          PIC X(5)    VALUE SPACE.                 
020000     03  W-IDDC-X.                                                        
020100         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
020200     03  W-IDSKYLT-X.                                                     
020300         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
020400     03  W-WDD3BSEQ-X.                                                    
020500         05  W-D3BSEQ-IDARTNR    PIC S9(9)   COMP-3.                      
020600     03  W-IDLEVNR-X.                                                     
020700         05    W-IDLEVNR     PIC X(5)    VALUE SPACE.                     
020800     03  W-IDLAND-X.                                                      
020900         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
021000     03  W-IDLANDX2-X.                                                    
021100         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
021200     SKIP2                                                                
021300 01  W-KVLS                      PIC S9(7)   VALUE ZERO.                  
021400 01  WS-KDPRODSL                 PIC 9(3)    VALUE ZERO.                  
021500 01  WS-KDPRODSL2                PIC X(3)    VALUE SPACE.                 
021600 01  W-PRAVCOST-01               PIC 9(7)V9(2) VALUE ZERO.                
021700 01  W-PRAVCOST-02               PIC 9(7)V9(2) VALUE ZERO.                
021800 01  W-PRAVCOST-03               PIC 9(7)V9(2) VALUE ZERO.                
021900 01  W-PRAVCOST-04               PIC 9(7)V9(2) VALUE ZERO.                
022000*    --- STATUS-KOD FRÅN IMS                                              
022100 01  STATUS-WS                   PIC XX.                                  
022200     88  SEGMENT-FINNS                       VALUE '  '.                  
022300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
022400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022500     SKIP2                                                                
022600 01  GODK-STATUSKODER.                                                    
022700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022800     SKIP3                                                                
022900 01  SSA1                        PIC X(128).                              
023000 01  SSA2                        PIC X(128).                              
023100 01  SSA3                        PIC X(64).                               
023200     EJECT                                                                
023300*    --- IMS FUNKTIONSKODER                                               
023400*01  -COPY W0003                                                          
023500     EJECT                                                                
023600*    ---  DLI INPUT-OUTPUT AREA                                           
023700 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDK601'.        
023800 01  DLI-IO-WDK601.                                                       
023900*    03  -COPY WDK601                                                     
024000     EJECT                                                                
024100 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDK611'.        
024200 01  DLI-IO-WDK611.                                                       
024300*    03  -COPY WDK611                                                     
024400     EJECT                                                                
024500 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDK621'.        
024600 01  DLI-IO-WDK621.                                                       
024700*    03  -COPY WDK621                                                     
024800 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDK701'.        
024900 01  DLI-IO-WDK701.                                                       
025000*    03  -COPY WDK701                                                     
025100     EJECT                                                                
025200 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDK711'.        
025300 01  DLI-IO-WDK711.                                                       
025400*    03  -COPY WDK711                                                     
025500 01  FILLER                      PIC X(24) VALUE 'DLI-IO-WDK712'.         
025600 01  DLI-IO-WDK712.                                                       
025700*    03  -COPY WDK712                                                     
025800     EJECT                                                                
025900 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDK724'.        
026000 01  DLI-IO-WDK724.                                                       
026100*    03  -COPY WDK724                                                     
026200 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDD301'.        
026300 01  DLI-IO-WDD301.                                                       
026400*    03  -COPY WDD301                                                     
026500     EJECT                                                                
026600 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDD311'.        
026700 01  DLI-IO-WDD311.                                                       
026800*    03  -COPY WDD311                                                     
026900 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDB601'.        
027000 01  DLI-IO-WDB601.                                                       
027100*    03  -COPY WDB601                                                     
027200     EJECT                                                                
027300 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDC301'.        
027400 01  DLI-IO-WDC301.                                                       
027500*    03  -COPY WDC301                                                     
027600 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDC311'.        
027700 01  DLI-IO-WDC311.                                                       
027800*    03  -COPY WDC311                                                     
027900     EJECT                                                                
028000 01  FILLER                    PIC X(16)  VALUE 'WDF101'.                 
028100*01  WLLEVA01 -COPY WDF101                                                
028200     EJECT                                                                
028300                                                                          
028400 01  FILLER                    PIC X(16)  VALUE 'WDF102'.                 
028500*01  WLLEVA11 -COPY WDF102 -PRE LEV-                                      
028600     EJECT                                                                
028700                                                                          
028800 LINKAGE SECTION.                                                         
028900*01  -COPY W0009   -PRE MSG-                                              
029000*01  -COPY W0008   -PRE USEA-                                             
029100     05  FILLER                  PIC X.                                   
029200     EJECT                                                                
029300*01  -COPY W0008  -PRE WDK6-                                              
029400     05  FILLER                  PIC X.                                   
029500     EJECT                                                                
029600*01  -COPY W0008  -PRE WDK7-                                              
029700     05  FILLER                  PIC X.                                   
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE WDD3-                                              
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200*01  -COPY W0008  -PRE WDG2-                                              
030300     05  FILLER                  PIC X.                                   
030400     EJECT                                                                
030500*01  -COPY W0008  -PRE WDB6-                                              
030600     05  FILLER                  PIC X.                                   
030700     EJECT                                                                
030800*01  -COPY W0008  -PRE WDC3-                                              
030900     05  FILLER                  PIC X.                                   
031000     EJECT                                                                
031100*01  -COPY W0008  -PRE WHSU-WDB6-                                         
031200     05  FILLER                  PIC X.                                   
031300     EJECT                                                                
031400 01  ARTC-P                      PIC X.                                   
031500 01  WDK7-P                      PIC X.                                   
031600 01  GMTA-P                      PIC X.                                   
031700 01  BETA-P                      PIC X.                                   
031800 01  PRIA-P                      PIC X.                                   
031900 01  PRIB-P                      PIC X.                                   
032000 01  PRIS-COST-WDK6-PCB          PIC X.                                   
032100 01  PRIS-COST-WDK7-PCB          PIC X.                                   
032200 01  PRIS-COST-WDF1-PCB          PIC X.                                   
032300 01  PRIS-COST-9305-PCB          PIC X.                                   
032400 01  PRIS-COST-WDK72-PCB         PIC X.                                   
032500 01  PRIS-COST-WDB6-PCB          PIC X.                                   
032600*01  -COPY W0008     -PRE LEV-                                            
032700     05  FILLER                  PIC X(5).                                
032800     EJECT                                                                
032900                                                                          
033000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK6-PCB WDK7-PCB             
033100     WDD3-PCB WDG2-PCB WDB6-PCB WDC3-PCB WHSU-WDB6-PCB                    
033200     ARTC-P WDK7-P GMTA-P BETA-P PRIA-P PRIB-P                            
033300     PRIS-COST-WDK6-PCB                                                   
033400     PRIS-COST-WDK7-PCB                                                   
033500     PRIS-COST-WDF1-PCB                                                   
033600     PRIS-COST-9305-PCB                                                   
033700     PRIS-COST-WDK72-PCB                                                  
033800     PRIS-COST-WDB6-PCB                                                   
033900     LEV-PCB.                                                             
034000                                                                          
034100 MAIN SECTION.                                                            
034200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK6-PCB WDK7-PCB             
034300     WDD3-PCB WDG2-PCB WDB6-PCB WDC3-PCB WHSU-WDB6-PCB                    
034400     ARTC-P WDK7-P GMTA-P BETA-P PRIA-P PRIB-P                            
034500     PRIS-COST-WDK6-PCB                                                   
034600     PRIS-COST-WDK7-PCB                                                   
034700     PRIS-COST-WDF1-PCB                                                   
034800     PRIS-COST-9305-PCB                                                   
034900     PRIS-COST-WDK72-PCB                                                  
035000     PRIS-COST-WDB6-PCB                                                   
035100     LEV-PCB.                                                             
035200*                                                                         
035300     PERFORM IMS-GET-MSG                                                  
035400     IF SEGMENT-FINNS                                                     
035500       PERFORM A-INIT                                                     
035600       PERFORM B-KOLLA-NYCKLAR                                            
035700       IF NYCKLAR-OK                                                      
035800         PERFORM F-LAES-VISA-INFO                                         
035900       END-IF                                                             
036000       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O20501 + 4                      
036100       PERFORM IMS-INSERT-MSG                                             
036200     END-IF                                                               
036300     MOVE ZERO                        TO RETURN-CODE                      
036400     GOBACK                                                               
036500     .                                                                    
036600     EJECT                                                                
036700                                                                          
036800 A-INIT SECTION.                                                          
036900     IF MSG-DUBBLA-TRANSKODER                                             
037000       MOVE MSG-INDATA-MINUS-2-TRANSKODER  TO MID-W5I20501                
037100       MOVE MSG-IDTRANS-2             TO MFS-IDTRANS                      
037200       MOVE MSG-KDMFSFOR-2            TO MFS-KDMFSFOR                     
037300     ELSE                                                                 
037400       MOVE MSG-INDATA-MINUS-1-TRANSKOD    TO MID-W5I20501                
037500       MOVE MSG-IDTRANS-1             TO MFS-IDTRANS                      
037600       MOVE MSG-KDMFSFOR-1            TO MFS-KDMFSFOR                     
037700     END-IF                                                               
037800*                                                                         
037900     MOVE MSG-KDTRTYP                 TO MFS-KDTRTYP                      
038000     MOVE MSG-IDPFK                   TO MFS-IDPFK                        
038100     MOVE MFS-IDTRANS                 TO W-IDTRANS                        
038200     MOVE LOW-VALUE                   TO MSG-AREA                         
038300     MOVE 'W5O205N1'                  TO MFS-IDMOD                        
038400     MOVE '5205'                      TO MOD-IDTRANS                      
038500     MOVE MFS-RENSA-FAELT             TO MOD-TEMFSFEL                     
038600                                         MOD-TEMFSINF                     
038700     IF EGEN-MID OR HELP-MID                                              
038800       CONTINUE                                                           
038900     ELSE                                                                 
039000       MOVE SPACE                     TO MFS-KDTRTYP                      
039100       MOVE '7'                       TO MFS-IDPFK                        
039200     END-IF                                                               
039300*                                                                         
039400     MOVE FUNCTION CURRENT-DATE (1:8) TO W-AAAAMMDD                       
039500     MOVE W-AAAAMMDD                  TO W-AAAAMMDD-NUM                   
039600     MOVE W-AAAAMMDD-NUM(3:2)         TO W-DATE-AAMM(1:2)                 
039700     MOVE W-AAAAMMDD-NUM(5:2)         TO W-DATE-AAMM(3:2)                 
039800*    >> CHANGE THE FOLLOWING COMPUTE MANUALLY                             
039900     COMPUTE W-DAPRLIST-9KOMPL  = 99999999 - W-AAAAMMDD-NUM               
040000     MOVE FUNCTION CURRENT-DATE (3:4) TO WS-DATE-YYMM                     
040100     MOVE 01                          TO WS-DATE-DD                       
040200     MOVE FUNCTION CURRENT-DATE (3:6) TO DAGENS-AAMMDD                    
040300     .                                                                    
040400     EJECT                                                                
040500                                                                          
040600 B-KOLLA-NYCKLAR SECTION.                                                 
040700     MOVE ALL '+'                     TO MSGI-WMSGINIT                    
040800     MOVE '001'                       TO MSGI-KDCALL                      
040900     MOVE MSG-LTERM-NAME              TO MSGI-IDLTERM-USER                
041000     MOVE MSG-SIGNON-USERID           TO MSGI-IDUSER                      
041100     MOVE '5205'                      TO MSGI-IDTRANS                     
041200     IF EGEN-MID                                                          
041300         MOVE MID-IDARTNR-IN          TO MSGI-IDARTNR                     
041400         MOVE MID-IDDC-IN             TO MSGI-IDDC-KEY                    
041500     END-IF                                                               
041600*                                                                         
041700     CALL W005INIT                 USING MSGI-WMSGINIT                    
041800                                         USEA-PCB                         
041900*                                                                         
042000     MOVE JA                          TO NYCKLAR-SW                       
042100     MOVE SPACE                       TO MOD-TEMFSFEL                     
042200*    -- KONTROLL AV IDARTNR IDDC                                          
042300     MOVE MFS-RENSA-FAELT             TO MOD-IDARTNR-IN                   
042400                                         MOD-IDDC-IN                      
042500                                                                          
042600     IF MID-IDARTNR-IN NOT = ALL '+' AND                                  
042700        MID-IDDC-IN NOT = ALL '+'                                         
042800       MOVE '7'                       TO MFS-IDPFK                        
042900       MOVE SPACE                     TO MFS-KDTRTYP                      
043000     END-IF                                                               
043100     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
043200     IF MSGI-IDARTNR NUMERIC                                              
043300       MOVE MSGI-IDARTNR              TO W-IDARTNR                        
043400     ELSE                                                                 
043500       MOVE NEJ                       TO NYCKLAR-SW                       
043600       MOVE ERR-WRONG-KEY             TO MED-IDMFSFEL                     
043700     END-IF                                                               
043800                                                                          
043900     MOVE MSGI-IDDC-KEY               TO WS-IDDC                          
044000                                                                          
044100     IF NDC-NA OR XDC-NON-VCC-OWNED                                       
044200       MOVE MSGI-IDDC-KEY             TO W-IDDC                           
044300     ELSE                                                                 
044400       MOVE NEJ TO NYCKLAR-SW                                             
044500       MOVE ERR-WRONG-KEY             TO MED-IDMFSFEL                     
044600     END-IF                                                               
044700                                                                          
044800     IF MSGI-IDFTG   NUMERIC AND MSGI-IDFTG   > ZERO                      
044900       MOVE MSGI-IDFTG                TO MOD-IDFTG-UT                     
045000                                         WS-IDFTG                         
045100     END-IF                                                               
045200                                                                          
045300     IF GODK-MID OR NYCKLAR-OK                                            
045400       MOVE MSGI-IDARTNR              TO MOD-IDARTNR-UT                   
045500       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
045600       MOVE MSGI-IDDC-KEY             TO MOD-IDDC-UT                      
045700     ELSE                                                                 
045800       MOVE MFS-RENSA-FAELT           TO MOD-IDARTNR-UT                   
045900                                         MOD-IDDC-UT                      
046000     END-IF                                                               
046100* USER CAN ONLY SEE THEIR IDFTG'S DC                                      
046200     PERFORM IMS-GU-WDB601                                                
046300     IF  NYCKLAR-OK                                                       
046400       IF XDC-NON-VCC-OWNED                                               
046500         IF WS-IDFTG = DCS-IDFTG                                          
046600           CONTINUE                                                       
046700         ELSE                                                             
046800           IF WS-IDFTG = '57'                                             
046900             CONTINUE                                                     
047000           ELSE                                                           
047100             MOVE NEJ                     TO NYCKLAR-SW                   
047200             MOVE ERR-NOT-AUTHORIZED      TO MED-IDMFSFEL                 
047300           END-IF                                                         
047400         END-IF                                                           
047500       ELSE                                                               
047600         IF NDC-NA                                                        
047700           IF (IDFTG-US OR IDFTG-CA)                                      
047800             CONTINUE                                                     
047900           ELSE                                                           
048000             IF WS-IDFTG = '57'                                           
048100               CONTINUE                                                   
048200             ELSE                                                         
048300               MOVE NEJ                     TO NYCKLAR-SW                 
048400               MOVE ERR-NOT-AUTHORIZED      TO MED-IDMFSFEL               
048500             END-IF                                                       
048600           END-IF                                                         
048700         END-IF                                                           
048800       END-IF                                                             
048900     END-IF                                                               
049000     IF NYCKLAR-FEL                                                       
049100       MOVE 'GB'                      TO MED-IDSKYLT                      
049200       CALL WMEDKONV               USING MED-WMEDAREA                     
049300       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
049400       PERFORM MFS-RENSA-FAELT-IN                                         
049500       PERFORM MFS-RENSA-FAELT-UT                                         
049600     END-IF                                                               
049700     .                                                                    
049800     EJECT                                                                
049900                                                                          
050000 F-LAES-VISA-INFO SECTION.                                                
050100     PERFORM FA-LAES-GRUNDDATA                                            
050200     IF SEGMENT-SAKNAS                                                    
050300        MOVE INF-PART-MISSING         TO MED-IDMFSFEL                     
050400        MOVE 'GB'                     TO MED-IDSKYLT                      
050500        CALL WMEDKONV              USING MED-WMEDAREA                     
050600        MOVE MED-MFSFEL               TO MOD-TEMFSFEL                     
050700        PERFORM MFS-RENSA-FAELT-UT                                        
050800     ELSE                                                                 
050900        PERFORM FB-LAES-RADDATA                                           
051000     END-IF                                                               
051100     .                                                                    
051200     EJECT                                                                
051300                                                                          
051400 FA-LAES-GRUNDDATA SECTION.                                               
051500     PERFORM IMS-GU-WDK601                                                
051600     IF SEGMENT-FINNS                                                     
051700       MOVE ART-KDPRODSL          TO WS-KDPRODSL                          
051800       MOVE WS-KDPRODSL           TO WS-KDPRODSL2                         
051900       MOVE WS-KDPRODSL2(2:2)     TO MOD-KDPRODSL                         
052000       PERFORM IMS-GNP-WDK611                                             
052100       IF SEGMENT-FINNS                                                   
052200         MOVE CLAG-KDPSLLOC       TO MOD-KDPSLLOC                         
052300       END-IF                                                             
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700                                                                          
052800 FB-LAES-RADDATA SECTION.                                                 
052900     MOVE NOO                         TO NON-VCCS-SW                      
053000     MOVE ART-IDFKNGRP                TO MOD-IDFKNGRP                     
053100     MOVE W-IDDC                      TO MOD-IDDC-01                      
053200     MOVE W-IDARTNR                   TO W-D3BSEQ-IDARTNR                 
053300     MOVE 'USA'                       TO W-IDSKYLT                        
053400     PERFORM IMS-GU-WDK701                                                
053500     IF SEGMENT-FINNS                                                     
053600       PERFORM IMS-GU-WDB601                                              
053700       IF SEGMENT-FINNS                                                   
053800         MOVE DCS-IDLANDX2            TO W-IDLAND                         
053900         PERFORM IMS-GU-WDK712                                            
054000         IF SEGMENT-FINNS                                                 
054100          MOVE LART-PRMATRL           TO MOD-PRMATRL-01                   
054200         END-IF                                                           
054300       END-IF                                                             
054400     END-IF                                                               
054500     PERFORM IMS-GU-WDD301                                                
054600     IF SEGMENT-SAKNAS                                                    
054700       MOVE SPACE                     TO MOD-BEART                        
054800     ELSE                                                                 
054900       MOVE TEXT-BEART                TO MOD-BEART                        
055000     END-IF                                                               
055100     PERFORM IMS-GU-WDK711                                                
055200     IF SEGMENT-SAKNAS                                                    
055300       MOVE INF-PART-INFO-MISSING     TO MED-IDMFSFEL                     
055400       MOVE 'GB'                      TO MED-IDSKYLT                      
055500       CALL WMEDKONV               USING MED-WMEDAREA                     
055600       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
055700     ELSE                                                                 
055800       PERFORM IMS-GU-WDB601                                              
055900       IF SEGMENT-SAKNAS                                                  
056000         MOVE INF-PART-INFO-MISSING   TO MED-IDMFSFEL                     
056100         MOVE 'GB'                    TO MED-IDSKYLT                      
056200         CALL WMEDKONV             USING MED-WMEDAREA                     
056300         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
056400       ELSE                                                               
056500         MOVE SLAG-KVAKS-PAV          TO W-KVAKS-PAV-01                   
056600         MOVE SLAG-KVAKS-SDC          TO W-KVAKS-SDC-01                   
056700         MOVE SLAG-KVLS               TO W-KVLS-01                        
056800         MOVE SLAG-KVEFRS             TO W-KVEFRS-01                      
056900         MOVE SLAG-KVUTRS             TO W-KVUTRS-01                      
057000         MOVE SLAG-PRAVCOST           TO MOD-PRAVCOST-01                  
057100         MOVE SLAG-PRAVCOST           TO W-PRAVCOST-01                    
057200         MOVE SLAG-KVLS               TO W-KVLS                           
057300         COMPUTE MOD-SUAVCOST-01 ROUNDED =                                
057400                 W-PRAVCOST-01 * (W-KVLS-01 + W-KVEFRS-01)                
057500                                                                          
057600         PERFORM FBF-GET-WDK724-PRICE                                     
057700         IF PRICE-FOUND                                                   
057800* IF PRICE IS OBTAINED FROM WDK724                                        
057900            CONTINUE                                                      
058000         ELSE                                                             
058100* IF NO WDK724 OCCURRENCE FOUND                                           
058200           MOVE SLAG-IDLEVNR            TO MOD-IDLEVNR-01                 
058300           MOVE SLAG-IDLEVNR            TO W-IDLEVNR                      
058400           IF W-IDLEVNR = '1441 ' OR 'BP2TW'                              
058500             MOVE W-IDDC-SE             TO PRIS-IDDC                      
058600             MOVE DCS-IDDISTR-REFILL    TO PRIS-IDDISTR                   
058700             PERFORM FBA-CALL-W335PRIS                                    
058800             MOVE PRIS-PRARTNTO         TO MOD-PRARTBEL-01                
058900             MOVE PRIS-PRARTNTO         TO W-PRARTBEL                     
059000           ELSE                                                           
059100** CALL SUBPROGRAM W510WHSU TO READ ALL SUPPLIER NUMBER FROM WDB6         
059200             MOVE W-IDLEVNR        TO WHSU-IDLEVNR                        
059300             CALL W510WHSU USING WHSU-W510WHSU WHSU-WDB6-PCB              
059400             IF WHSU-KDSVAR = ' '                                         
059500               MOVE YES                   TO NON-VCCS-SW                  
059600               MOVE W-IDDC-SE             TO PRIS-IDDC                    
059700               MOVE DCS-IDDISTR-REFILL    TO PRIS-IDDISTR                 
059800               PERFORM FBA-CALL-W335PRIS                                  
059900               MOVE PRIS-PRARTNTO         TO MOD-PRARTBEL-01              
060000               MOVE PRIS-PRARTNTO         TO W-PRARTBEL                   
060100             ELSE                                                         
060200               MOVE ZERO                  TO MOD-PRARTBEL-01              
060300               PERFORM FBB-WDK621-PRIS                                    
060400               IF STATUS-WS NOT = 'GE'                                    
060500                 MOVE PRL-PRARTBEL-PR     TO MOD-PRARTBEL-01              
060600                 MOVE PRL-PRARTBEL-PR     TO W-PRARTBEL                   
060700               END-IF                                                     
060800             END-IF                                                       
060900           END-IF                                                         
061000           IF W-IDLEVNR = '1441 ' OR 'BP2TW' OR                           
061100              PRL-KDVALISO = 'SEK' OR WH-YES                              
061200             MOVE DCS-KDVALISO          TO CURR-KDVALISO-ROW              
061300           ELSE                                                           
061400             MOVE PRL-KDVALISO          TO CURR-KDVALISO-ROW              
061500           END-IF                                                         
061600           PERFORM FBC-READ-VALUTA                                        
061700           IF W-VALUTA-FINNS = 'Y'                                        
061800             MOVE CURR-PRKURS-NEW       TO W-PRKURS                       
061900             IF IDFTG-IN AND W-IDLEVNR = '1441'                           
062000               MOVE 1.00000             TO MOD-PRKURS-01                  
062100                                           W-PRKURS2                      
062200               COMPUTE W-PRARTBEL-INR ROUNDED =                           
062300                     W-PRARTBEL / W-PRKURS                                
062400               MOVE W-PRARTBEL-INR      TO MOD-PRARTBEL-01                
062500                                           W-PRARTBEL                     
062600             ELSE                                                         
062700               IF W-IDLEVNR = '1441 ' OR 'BP2TW' OR                       
062800                  PRL-KDVALISO = 'SEK' OR WH-YES                          
062900                 MOVE W-PRKURS          TO MOD-PRKURS-01                  
063000                 MOVE W-PRKURS          TO W-PRKURS2                      
063100               ELSE                                                       
063200                 MOVE W-PRKURS          TO W-FIRST-PRKURS                 
063300                 MOVE DCS-KDVALISO      TO CURR-KDVALISO-ROW              
063400                 PERFORM FBC-READ-VALUTA                                  
063500                 IF W-VALUTA-FINNS = 'Y'                                  
063600                   COMPUTE W-PRKURS2 ROUNDED =                            
063700                           W-PRKURS / W-FIRST-PRKURS                      
063800                   MOVE W-PRKURS2       TO MOD-PRKURS-01                  
063900                 ELSE                                                     
064000                   MOVE ZERO            TO MOD-PRKURS-01                  
064100                                           W-PRKURS2                      
064200                 END-IF                                                   
064300               END-IF                                                     
064400             END-IF                                                       
064500             IF W-PRKURS2 = ZERO                                          
064600               MOVE ZERO                TO MOD-PRARTBEL-02                
064700             ELSE                                                         
064800               COMPUTE W-PRARTBEL-02 ROUNDED =                            
064900                       W-PRARTBEL / W-PRKURS2                             
065000               MOVE W-PRARTBEL-02       TO MOD-PRARTBEL-02                
065100             END-IF                                                       
065200           ELSE                                                           
065300             MOVE ZERO                  TO MOD-PRKURS-01                  
065400                                           MOD-PRARTBEL-02                
065500           END-IF                                                         
065600         END-IF                                                           
065700         IF XDC-NON-VCC-OWNED                                             
065800           IF NDC-IN                                                      
065900             MOVE 'IN' TO W-IDLANDX2                                      
066000             PERFORM IMS-GU-WDC311                                        
066100             IF SEGMENT-FINNS                                             
066200               MOVE SRP-PRARTBTO-SC TO MOD-PRARTBTO-SC                    
066300             ELSE                                                         
066400               MOVE ZERO            TO MOD-PRARTBTO-SC                    
066500             END-IF                                                       
066600           END-IF                                                         
066700         ELSE                                                             
066800            MOVE MFS-RENSA-FAELT TO MOD-PRARTBTO-SC                       
066900         END-IF                                                           
067000**** DC 92 SHOULD NOT HANDLE THE CORE PART AS THE OTHER DC'S              
067100         IF NDC-US-BAT                                                    
067200           CONTINUE                                                       
067300         ELSE                                                             
067400           PERFORM FBD-CHECK-IF-BYTES                                     
067500         END-IF                                                           
067600         IF W-BYTE-ART = 'Y'                                              
067700           PERFORM FBE-READ-AND-ADD-SALDO                                 
067800         ELSE                                                             
067900           MOVE W-KVAKS-PAV-01        TO MOD-KVAKS-PAV-01                 
068000           MOVE W-KVAKS-SDC-01        TO MOD-KVAKS-SDC-01                 
068100           MOVE W-KVLS-01             TO MOD-KVLS-01                      
068200           MOVE W-KVEFRS-01           TO MOD-KVEFRS-01                    
068300           MOVE W-KVUTRS-01           TO MOD-KVUTRS-01                    
068400         END-IF                                                           
068500       END-IF                                                             
068600     END-IF                                                               
068700     .                                                                    
068800     EJECT                                                                
068900                                                                          
069000 FBA-CALL-W335PRIS SECTION.                                               
069100     MOVE 1                           TO PRIS-KDCALL                      
069200     MOVE 1                           TO PRIS-KVBEART                     
069300     MOVE 0                           TO PRIS-IDKUNDNR                    
069400     MOVE 4                           TO PRIS-KDORDKL                     
069500     MOVE 'W5020500'                  TO PRIS-IDPGM                       
069600     MOVE SPACE                       TO PRIS-FLINVEST                    
069700     MOVE W-IDARTNR                   TO PRIS-IDARTNR                     
069800                                                                          
069900     CALL W335PRIS                 USING PRIS-W335PRIS                    
070000                                         ARTC-P WDK7-P                    
070100                                         GMTA-P BETA-P                    
070200                                         PRIA-P PRIB-P                    
070300                                         PRIS-COST-WDK6-PCB               
070400                                         PRIS-COST-WDK7-PCB               
070500                                         PRIS-COST-WDF1-PCB               
070600                                         PRIS-COST-9305-PCB               
070700                                         PRIS-COST-WDK72-PCB              
070800                                         PRIS-COST-WDB6-PCB               
070900     .                                                                    
071000     EJECT                                                                
071100                                                                          
071200 FBB-WDK621-PRIS SECTION.                                                 
071300     PERFORM IMS-GU-WDK601                                                
071400     PERFORM IMS-GNP-WDK611                                               
071500     IF SEGMENT-SAKNAS                                                    
071600        MOVE INF-PART-MISSING         TO MED-IDMFSFEL                     
071700        MOVE 'GB'                     TO MED-IDSKYLT                      
071800        CALL WMEDKONV              USING MED-WMEDAREA                     
071900        MOVE MED-MFSFEL               TO MOD-TEMFSFEL                     
072000     ELSE                                                                 
072100        PERFORM IMS-GNP-WDK621                                            
072200     END-IF                                                               
072300     IF SEGMENT-SAKNAS                                                    
072400        MOVE INF-PART-MISSING         TO MED-IDMFSFEL                     
072500        MOVE 'GB'                     TO MED-IDSKYLT                      
072600        CALL WMEDKONV              USING MED-WMEDAREA                     
072700        MOVE MED-MFSFEL               TO MOD-TEMFSFEL                     
072800     ELSE                                                                 
072900       PERFORM UNTIL (PRL-IDLEVNR = SLAG-IDLEVNR                          
073000                     AND PRL-KDSTATUS-PR > 0                              
073100                     AND W-DAPRLIST-9KOMPL <= PRL-DAPRLIST-9KOMPL)        
073200                     OR  STATUS-WS = 'GE'                                 
073300         PERFORM IMS-GNP-WDK621                                           
073400       END-PERFORM                                                        
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800                                                                          
073900 FBC-READ-VALUTA SECTION.                                                 
074000     MOVE 'Y'                         TO W-VALUTA-FINNS                   
074100                                                                          
074200     MOVE WS-KDVALISO-HUV             TO CURR-KDVALISO-HUV                
074300     MOVE W-DATE-AAMM                 TO CURR-TIAAMM                      
074400     MOVE 'M'                         TO CURR-KDVALTYP                    
074500                                                                          
074600     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
074700     IF CURR-KDSVAR = ' '                                                 
074800       MOVE CURR-PRKURS-NEW           TO W-PRKURS                         
074900     ELSE                                                                 
075000       MOVE INF-PART-MISSING          TO MED-IDMFSFEL                     
075100       MOVE 'GB'                      TO MED-IDSKYLT                      
075200       CALL WMEDKONV               USING MED-WMEDAREA                     
075300       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
075400       MOVE 'N'                       TO W-VALUTA-FINNS                   
075500       MOVE ZERO                      TO W-PRKURS                         
075600     END-IF                                                               
075700     .                                                                    
075800     EJECT                                                                
075900                                                                          
076000 FBD-CHECK-IF-BYTES SECTION.                                              
076100     MOVE W-IDARTNR                   TO BYT20-IDARTNR                    
076200     IF BYT20-BYTES                                                       
076300       SUBTRACT +6000               FROM W-IDARTNR                        
076400       MOVE 'Y'                       TO W-BYTE-ART                       
076500     ELSE                                                                 
076600       IF BYT20-RADIO                                                     
076700         SUBTRACT +1000             FROM W-IDARTNR                        
076800         MOVE 'Y'                     TO W-BYTE-ART                       
076900       ELSE                                                               
077000         MOVE 'N'                     TO W-BYTE-ART                       
077100       END-IF                                                             
077200     END-IF                                                               
077300     .                                                                    
077400     EJECT                                                                
077500                                                                          
077600 FBE-READ-AND-ADD-SALDO SECTION.                                          
077700     PERFORM IMS-GU-WDK711                                                
077800     IF SEGMENT-SAKNAS                                                    
077900       MOVE 'GB'                      TO MED-IDSKYLT                      
078000       CALL WMEDKONV               USING MED-WMEDAREA                     
078100       MOVE MED-MFSFEL                TO MOD-TEMFSFEL                     
078200       MOVE 'N'                       TO W-BYTE-ART                       
078300     ELSE                                                                 
078400       ADD SLAG-KVAKS-PAV             TO W-KVAKS-PAV-01                   
078500                                  GIVING MOD-KVAKS-PAV-01                 
078600       ADD SLAG-KVAKS-SDC             TO W-KVAKS-SDC-01                   
078700                                  GIVING MOD-KVAKS-SDC-01                 
078800       ADD SLAG-KVLS                  TO W-KVLS-01                        
078900       MOVE W-KVLS-01                 TO MOD-KVLS-01                      
079000       ADD SLAG-KVEFRS                TO W-KVEFRS-01                      
079100       MOVE W-KVEFRS-01               TO MOD-KVEFRS-01                    
079200       ADD SLAG-KVUTRS                TO W-KVUTRS-01                      
079300                                  GIVING MOD-KVUTRS-01                    
079400       COMPUTE MOD-SUAVCOST-01   ROUNDED =                                
079500               W-PRAVCOST-01 * (W-KVLS-01 + W-KVEFRS-01)                  
079600     END-IF                                                               
079700     .                                                                    
079800     EJECT                                                                
079900                                                                          
080000 FBF-GET-WDK724-PRICE SECTION.                                            
080100* GET FIRST OCCURRENCE OF WDK724 WHERE SPRL-SUINLEV-PR IS                 
080200* GREATER THAN 0                                                          
080300     MOVE NEJ                     TO WS-PRICE-SW                          
080400     PERFORM IMS-GNP-WDK724-FIRST                                         
080500     PERFORM UNTIL SEGMENT-SAKNAS OR PRICE-FOUND                          
080600       IF SEGMENT-FINNS                                                   
080700          IF SPRL-SUINLEV-PR > 0                                          
080800             MOVE JA              TO WS-PRICE-SW                          
080900             PERFORM FBFA-MOVE-PRICE-VALUES                               
081000          END-IF                                                          
081100       END-IF                                                             
081200       PERFORM IMS-GNP-WDK724-NEXT                                        
081300     END-PERFORM                                                          
081400                                                                          
081500* IF NOT FOUND, GET FIRST OCCURRENCE OF WDK724                            
081600     IF PRICE-FOUND                                                       
081700        CONTINUE                                                          
081800     ELSE                                                                 
081900        PERFORM IMS-GU-WDK711                                             
082000        PERFORM IMS-GNP-WDK724-FIRST                                      
082100        IF SEGMENT-FINNS                                                  
082200           MOVE JA                TO WS-PRICE-SW                          
082300           PERFORM FBFA-MOVE-PRICE-VALUES                                 
082400        END-IF                                                            
082500     END-IF                                                               
082600     .                                                                    
082700     EJECT                                                                
082800                                                                          
082900 FBFA-MOVE-PRICE-VALUES SECTION.                                          
083000     MOVE SPRL-IDLEVNR-PR          TO MOD-IDLEVNR-01                      
083100     MOVE SPRL-IDLEVNR-PR          TO W-IDLEVNR                           
083200     MOVE SPRL-PRARTBEL-PR         TO MOD-PRARTBEL-01                     
083300     MOVE SPRL-PRARTBEL-PR         TO W-PRARTBEL                          
083400                                                                          
083500* GET EXCHANGE RATE                                                       
083600     PERFORM FBGA-READ-VALUTA                                             
083700     MOVE W-PRKURS                 TO MOD-PRKURS-01                       
083800     MOVE WS-PRARTBES-PR           TO MOD-PRARTBEL-02                     
083900     .                                                                    
084000     EJECT                                                                
084100                                                                          
084200 FBGA-READ-VALUTA SECTION.                                                
084300**** RECOUNT PRICEROWS WITH CORRECT MONTH RATE                            
084400     MOVE DCS-KDVALISO         TO CURR-KDVALISO-HUV                       
084500     MOVE SPRL-KDVALISO        TO CURR-KDVALISO-ROW                       
084600     IF CURR-KDVALISO-HUV = CURR-KDVALISO-ROW                             
084700       MOVE 1 TO W-PRKURS                                                 
084800       MOVE 1 TO W-REVALUTA                                               
084900     ELSE                                                                 
085000       MOVE W-DATE-AAMM        TO CURR-TIAAMM                             
085100       MOVE 'M'                TO CURR-KDVALTYP                           
085200       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
085300       IF CURR-KDSVAR = ' '                                               
085400         MOVE CURR-PRKURS-NEW  TO W-PRKURS                                
085500         MOVE CURR-REVALUTA-TO TO W-REVALUTA                              
085600       ELSE                                                               
085700         MOVE 1                TO W-PRKURS                                
085800         MOVE 1                TO W-REVALUTA                              
085900       END-IF                                                             
086000     END-IF                                                               
086100     MOVE SPRL-IDLEVNR-PR      TO W-IDLEVNR                               
086200     PERFORM IMS-GU-WLLEVA01                                              
086300     IF SEGMENT-SAKNAS                                                    
086400       MOVE 1 TO W-RETULF                                                 
086500     ELSE                                                                 
086600       MOVE DCS-IDLANDX2 TO W-IDLAND                                      
086700       PERFORM IMS-GNP-WLLEVA11                                           
086800       IF SEGMENT-FINNS                                                   
086900         IF LEV-TULL-TITULF < DAGENS-AAMMDD                               
087000           MOVE LEV-TULL-RETULF-1 TO W-RETULF                             
087100         ELSE                                                             
087200           MOVE LEV-TULL-RETULF-2 TO W-RETULF                             
087300         END-IF                                                           
087400       ELSE                                                               
087500         MOVE 1 TO W-RETULF                                               
087600       END-IF                                                             
087700     END-IF                                                               
087800     COMPUTE WS-PRARTBES-PR ROUNDED = W-PRARTBEL * W-RETULF               
087900                                    * W-PRKURS                            
088000     .                                                                    
088100     EJECT                                                                
088200                                                                          
088300 MFS-RENSA-FAELT-IN SECTION.                                              
088400*    --- ALLA INDATA-FÄLT                                                 
088500     MOVE MFS-RENSA-FAELT             TO MOD-IDARTNR-IN                   
088600                                         MOD-IDDC-IN                      
088700     .                                                                    
088800     EJECT                                                                
088900                                                                          
089000 MFS-RENSA-FAELT-UT  SECTION.                                             
089100*    --- ALLA UTDATA-FÄLT                                                 
089200     MOVE MFS-RENSA-FAELT             TO                                  
089300                                         MOD-BEART                        
089400                                         MOD-IDFKNGRP                     
089500                                         MOD-KDPSLLOC                     
089600                                         MOD-IDDC-01                      
089700                                         MOD-KVAKS-PAV-01                 
089800                                         MOD-KVAKS-SDC-01                 
089900                                         MOD-KVLS-01                      
090000                                         MOD-KVEFRS-01                    
090100                                         MOD-KVUTRS-01                    
090200                                         MOD-PRAVCOST-01                  
090300                                         MOD-SUAVCOST-01                  
090400                                         MOD-IDLEVNR-01                   
090500                                         MOD-PRARTBEL-01                  
090600                                         MOD-PRKURS-01                    
090700                                         MOD-PRARTBTO-SC                  
090800     .                                                                    
090900     SKIP3                                                                
091000* --- IMS SEKTIONER ---                                                   
091100     SKIP3                                                                
091200                                                                          
091300 IMS-GET-MSG SECTION.                                                     
091400     MOVE '  QC'                      TO GODK-STATUSKODER                 
091500     CALL CBLTDLI                  USING GU                               
091600                                         MSG-PCB                          
091700                                         MSG-IO-AREA                      
091800     MOVE MSG-STATUS-CODE             TO STATUS-WS                        
091900     PERFORM IMS-STATUSKONTROLL                                           
092000     .                                                                    
092100     SKIP3                                                                
092200                                                                          
092300 IMS-INSERT-MSG SECTION.                                                  
092400     MOVE LOW-VALUE                   TO MSG-KDZ1                         
092500                                         MSG-KDZ2                         
092600     MOVE SPACE                       TO GODK-STATUSKODER                 
092700     CALL CBLTDLI                  USING ISRT                             
092800                                         MSG-PCB                          
092900                                         MSG-IO-AREA                      
093000                                         MFS-IDMOD                        
093100     MOVE MSG-STATUS-CODE             TO STATUS-WS                        
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     .                                                                    
093400     EJECT                                                                
093500                                                                          
093600 IMS-GU-WDK601     SECTION.                                               
093700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
093800       DELIMITED BY SIZE INTO SSA1                                        
093900     MOVE '  GE'                      TO GODK-STATUSKODER                 
094000     CALL CBLTDLI                  USING GU                               
094100                                         WDK6-PCB                         
094200                                         DLI-IO-WDK601                    
094300                                         SSA1                             
094400     MOVE WDK6-STATUS-CODE            TO STATUS-WS                        
094500     PERFORM IMS-STATUSKONTROLL                                           
094600     .                                                                    
094700     EJECT                                                                
094800                                                                          
094900 IMS-GNP-WDK611    SECTION.                                               
095000     MOVE 'WDK611  '                  TO SSA1                             
095100     MOVE '  GE'                      TO GODK-STATUSKODER                 
095200     CALL CBLTDLI                  USING GNP                              
095300                                         WDK6-PCB                         
095400                                         DLI-IO-WDK611                    
095500                                         SSA1                             
095600     MOVE WDK6-STATUS-CODE            TO STATUS-WS                        
095700     PERFORM IMS-STATUSKONTROLL                                           
095800     .                                                                    
095900     EJECT                                                                
096000                                                                          
096100 IMS-GNP-WDK621    SECTION.                                               
096200     MOVE 'WDK621  '                  TO SSA1                             
096300     MOVE '  GE'                      TO GODK-STATUSKODER                 
096400     CALL CBLTDLI                  USING GNP                              
096500                                         WDK6-PCB                         
096600                                         DLI-IO-WDK621                    
096700                                         SSA1                             
096800     MOVE WDK6-STATUS-CODE            TO STATUS-WS                        
096900     PERFORM IMS-STATUSKONTROLL                                           
097000     .                                                                    
097100     EJECT                                                                
097200                                                                          
097300 IMS-GU-WDK701 SECTION.                                                   
097400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
097500     DELIMITED  BY SIZE INTO SSA1                                         
097600     MOVE '  GE' TO GODK-STATUSKODER                                      
097700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
097800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
097900     PERFORM IMS-STATUSKONTROLL                                           
098000     .                                                                    
098100     SKIP3                                                                
098200                                                                          
098300 IMS-GU-WDK711    SECTION.                                                
098400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
098500       DELIMITED BY SIZE INTO SSA1                                        
098600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
098700       DELIMITED BY SIZE INTO SSA2                                        
098800     MOVE '  GE'                      TO GODK-STATUSKODER                 
098900     CALL CBLTDLI                  USING GU                               
099000                                         WDK7-PCB                         
099100                                         DLI-IO-WDK711                    
099200                                         SSA1                             
099300                                         SSA2                             
099400     MOVE WDK7-STATUS-CODE            TO STATUS-WS                        
099500     PERFORM IMS-STATUSKONTROLL                                           
099600     .                                                                    
099700     EJECT                                                                
099800                                                                          
099900 IMS-GNP-WDK724-FIRST SECTION.                                            
100000     MOVE 'WDK724  *F' TO SSA1                                            
100100     MOVE '  GE' TO GODK-STATUSKODER                                      
100200     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
100300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
100400     PERFORM IMS-STATUSKONTROLL                                           
100500     .                                                                    
100600     SKIP3                                                                
100700                                                                          
100800 IMS-GNP-WDK724-NEXT SECTION.                                             
100900     MOVE 'WDK724   ' TO SSA1                                             
101000     MOVE '  GE' TO GODK-STATUSKODER                                      
101100     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
101200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
101300     PERFORM IMS-STATUSKONTROLL                                           
101400     .                                                                    
101500     EJECT                                                                
101600                                                                          
101700 IMS-GU-WDK712   SECTION.                                                 
101800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
101900          DELIMITED BY SIZE INTO SSA1                                     
102000     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
102100          DELIMITED BY SIZE INTO SSA2                                     
102200     MOVE '  GE' TO GODK-STATUSKODER                                      
102300     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712                         
102400          SSA1 SSA2                                                       
102500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
102600     PERFORM IMS-STATUSKONTROLL                                           
102700     .                                                                    
102800     EJECT                                                                
102900                                                                          
103000 IMS-GU-WDD301    SECTION.                                                
103100     STRING 'WDD301  (WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
103200       DELIMITED BY SIZE INTO SSA1                                        
103300     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
103400       DELIMITED BY SIZE INTO SSA2                                        
103500     MOVE '  GE'                      TO GODK-STATUSKODER                 
103600     CALL CBLTDLI                  USING GU                               
103700                                         WDD3-PCB                         
103800                                         DLI-IO-WDD311                    
103900                                         SSA1                             
104000                                         SSA2                             
104100     MOVE WDD3-STATUS-CODE            TO STATUS-WS                        
104200     PERFORM IMS-STATUSKONTROLL                                           
104300     .                                                                    
104400     EJECT                                                                
104500                                                                          
104600 IMS-GU-WDB601    SECTION.                                                
104700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
104800       DELIMITED BY SIZE INTO SSA1                                        
104900     MOVE '  GE'                      TO GODK-STATUSKODER                 
105000     CALL CBLTDLI                  USING GU                               
105100                                         WDB6-PCB                         
105200                                         DLI-IO-WDB601                    
105300                                         SSA1                             
105400     MOVE WDB6-STATUS-CODE            TO STATUS-WS                        
105500     PERFORM IMS-STATUSKONTROLL                                           
105600     .                                                                    
105700     EJECT                                                                
105800                                                                          
105900 IMS-GU-WDC311    SECTION.                                                
106000     STRING 'WDC301  (IDARTNR  =' W-IDARTNR-X ')'                         
106100       DELIMITED BY SIZE INTO SSA1                                        
106200     STRING 'WDC311  (IDLANDX2 =' W-IDLANDX2-X ')'                        
106300          DELIMITED BY SIZE INTO SSA2                                     
106400     MOVE '  GE'                      TO GODK-STATUSKODER                 
106500     CALL CBLTDLI                  USING GU                               
106600                                         WDC3-PCB                         
106700                                         DLI-IO-WDC311                    
106800                                         SSA1 SSA2                        
106900     MOVE WDC3-STATUS-CODE            TO STATUS-WS                        
107000     PERFORM IMS-STATUSKONTROLL                                           
107100     .                                                                    
107200     EJECT                                                                
107300                                                                          
107400 IMS-GU-WLLEVA01 SECTION.                                                 
107500     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
107600     DELIMITED BY SIZE INTO SSA1                                          
107700     MOVE '  GE' TO GODK-STATUSKODER                                      
107800     CALL CBLTDLI USING GU LEV-PCB WLLEVA01 SSA1                          
107900     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
108000     PERFORM IMS-STATUSKONTROLL                                           
108100     .                                                                    
108200     SKIP3                                                                
108300                                                                          
108400 IMS-GNP-WLLEVA11 SECTION.                                                
108500     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
108600     DELIMITED BY SIZE INTO SSA1                                          
108700     MOVE '  GE' TO GODK-STATUSKODER                                      
108800     CALL CBLTDLI USING GNP LEV-PCB LEV-WLLEVA11 SSA1                     
108900     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
109000     PERFORM IMS-STATUSKONTROLL                                           
109100     .                                                                    
109200     EJECT                                                                
109300                                                                          
109400 IMS-STATUSKONTROLL SECTION.                                              
109500     SET STATUS-IX                    TO 1                                
109600     SEARCH GODK-STATUS                                                   
109700       AT END                                                             
109800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
109900           DELIMITED BY SIZE INTO FELTEXT                                 
110000         CALL FELLOG                                                      
110100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
110200         CONTINUE                                                         
110300     END-SEARCH                                                           
110400     .                                                                    

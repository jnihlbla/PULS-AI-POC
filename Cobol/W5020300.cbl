000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5020300.                                                
000300 AUTHOR.         SHARON ABRAHAMSON                                        
000400 DATE-WRITTEN.   97/01/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PRORAMMET VISAS INVENTORY VALUE, ACCOUNTING                      
000900*        FÖR EN GIVEN ARTIKEL.                                            
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLARTC (WDK6V)                             
001200*        PROGRAMMET LÄSER      WLARTS (WDK7V)                             
001300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001400*        PROGRAMMET LÄSER   MÅNADSKURS WDG2(THRU W510CURR)                
001500*                           WDGX9305 +  WDGX9306 + WDG9308                
001600*        PROGRAMMET LÄSER      W335PRIS                                   
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W5T203                                              
002000*        MID:         W5I20301                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W5O20301                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000*    -COPY WY2000W1                                                       
003100*                      *************************************              
003200*                      *** INNEHÅLLER RADIO OCH BYTES OBJEKT              
003300*                      ***                                                
003400*                      *** INNEHÅLLER ENDAST OBJEKT                       
003500*                      *************************************              
003600*01  FILLER -COPY WWBYT20                                                 
003700*                                                                         
003800     SKIP3                                                                
003900 77  IDPGM                       PIC X(08)   VALUE 'W5020300'.            
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600                                                                          
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800                                                                          
004900 77  WS-PRICE-SW                 PIC X       VALUE 'N'.                   
005000     88  PRICE-FOUND                         VALUE 'J'.                   
005100                                                                          
005200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005300     88  NYCKLAR-OK                          VALUE 'J'.                   
005400     88  NYCKLAR-FEL                         VALUE 'N'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '5203'.                
005800     88  GODK-MID                            VALUE '5201' '5202'          
005900                                                   '5203' '5204'          
006000                                                   '5205' '5206'          
006100                                                   '5207' '5208'          
006200                                                   '5209'.                
006300     88  HELP-MID                            VALUE '0551'.                
006400                                                                          
006500 01       WS-KLOCKAN-LOK.                                                 
006600   03   WS-TIHHMMSS-LOK         PIC 9(6).                                 
006700   03   FILLER                  PIC X(2).                                 
006800                                                                          
006900 77       WS-DIRLEV-PRC          PIC X(4)   VALUE '9998'.                 
007000 77       WS-SEKELTAL-19         PIC X(2)   VALUE '19'.                   
007100 77       WS-SEKELTAL-20         PIC X(2)   VALUE '20'.                   
007200 77       WS-DATUM               PIC 9(6).                                
007300 77       WS-DATUM-LOK           PIC 9(6).                                
007400 77       WS-DATUM-9KOMPL        PIC 9(8).                                
007500 77       WS-DATUM-AADDD         PIC 9(5).                                
007600 77       WS-DATUM-AADDD-LOK     PIC 9(5).                                
007700 77       WS-REGDATUM-AADDD      PIC 9(5).                                
007800 77       WS-ANTAL-DAGAR         PIC 9(5).                                
007901 01  W-REVALUTA                  PIC S9(5) VALUE +0   COMP-3.             
008000 01  W-KVAKS-PAV-41              PIC S9(7) VALUE ZERO COMP-3.             
008100 01  W-KVAKS-SDC-41              PIC S9(7) VALUE ZERO COMP-3.             
008200 01  W-KVLS-41                   PIC S9(7) VALUE ZERO COMP-3.             
008300 01  W-KVEFRS-41                 PIC S9(7) VALUE ZERO COMP-3.             
008400 01  W-KVUTRS-41                 PIC S9(7) VALUE ZERO COMP-3.             
008500 01  W-KVAKS-PAV-42              PIC S9(7) VALUE ZERO COMP-3.             
008600 01  W-KVAKS-SDC-42              PIC S9(7) VALUE ZERO COMP-3.             
008700 01  W-KVLS-42                   PIC S9(7) VALUE ZERO COMP-3.             
008800 01  W-KVEFRS-42                 PIC S9(7) VALUE ZERO COMP-3.             
008900 01  W-KVUTRS-42                 PIC S9(7) VALUE ZERO COMP-3.             
009000 01  W-KVAKS-PAV-43              PIC S9(7) VALUE ZERO COMP-3.             
009100 01  W-KVAKS-SDC-43              PIC S9(7) VALUE ZERO COMP-3.             
009200 01  W-KVLS-43                   PIC S9(7) VALUE ZERO COMP-3.             
009300 01  W-KVEFRS-43                 PIC S9(7) VALUE ZERO COMP-3.             
009400 01  W-KVUTRS-43                 PIC S9(7) VALUE ZERO COMP-3.             
009500 01  W-KVAKS-PAV-51              PIC S9(7) VALUE ZERO COMP-3.             
009600 01  W-KVAKS-SDC-51              PIC S9(7) VALUE ZERO COMP-3.             
009700 01  W-KVLS-51                   PIC S9(7) VALUE ZERO COMP-3.             
009800 01  W-KVEFRS-51                 PIC S9(7) VALUE ZERO COMP-3.             
009900 01  W-KVUTRS-51                 PIC S9(7) VALUE ZERO COMP-3.             
010000 01  W-VALUTA-FINNS              PIC X      VALUE 'Y'.                    
010100 01  W-GE-WDK711                 PIC X.                                   
010200 01  W-DAPRLIST-9KOMPL           PIC 9(8).                                
010302 01  W-PRKURS                    PIC S9(6)V9(5) VALUE ZERO.               
010402 01  W-PRKURS2                   PIC S9(6)V9(5) VALUE ZERO.               
010500 01  W-PRARTBEL                  PIC S9(8)V9(3) VALUE ZERO.               
010600 01  W-PRARTBEU                  PIC S9(5)V9(2) VALUE ZERO.               
010702 01  W-FIRST-PRKURS              PIC S9(6)V9(5) VALUE ZERO.               
010703 01  W-DATE-AAMM                 PIC 9(4)       VALUE ZERO.               
010704 01  WS-KDVALISO-HUV             PIC X(3)       VALUE 'SEK'.              
010800 01  K-19-SEKEL              PIC X(2)      VALUE '19'.                    
010900 01  K-20-SEKEL              PIC X(2)      VALUE '20'.                    
011000 01  W-BYTE-ART              PIC X         VALUE 'N'.                     
011100 01  W-IDARTNR-CHECK.                                                     
011200     03  W-IDARTNR-4         PIC 9(4)   VALUE ZERO.                       
011300     03  W-IDARTNR-5         PIC 9(5)   VALUE ZERO.                       
011400 01  W-RETULF            PIC S9(3)V9(4) VALUE +0.                         
011500 01  DAGENS-AAMMDD       PIC 9(6).                                        
011600     EJECT                                                                
011700 01  FILLER                  PIC X(50)                                    
011800     VALUE '*****     ARBETSAREOR     *****'.                             
011900*                                                                         
012000 01  WS-DATE-YYMMDD            PIC 9(06).                                 
012100 01  WS-DATE-FIRST REDEFINES WS-DATE-YYMMDD.                              
012200     03  WS-DATE-YYMM          PIC 9(04).                                 
012300     03  WS-DATE-DD            PIC 9(02).                                 
012400 01  DATUM                   PIC X(8).                                    
012500 01  W-DATUM.                                                             
012600     03 W-AAR                PIC X(2).                                    
012700     03 W-MAN                PIC X(2).                                    
012800     03 W-DAG                PIC X(2).                                    
012900*                                                                         
013000 01  W-AAAAMMDD-NUM              PIC 9(8).                                
013100 01  W-AAAAMMDD.                                                          
013200     03 W-AA-SEKEL               PIC X(2)       VALUE SPACE.              
013300     03 W-AAMMDD.                                                         
013400        05 W-AA                  PIC X(2)       VALUE SPACE.              
013500        05 W-MM                  PIC X(2)       VALUE SPACE.              
013600        05 W-DD                  PIC X(2)       VALUE SPACE.              
013700                                                                          
013800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
013900 01  GENERELLA-SUBPROGRAM.                                                
014000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
014100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
014200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014400     03  W335PRIS                PIC X(8)    VALUE 'W335PRIS'.            
014410     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
014500     EJECT                                                                
014600*    --- VALID IDDC CODES                                                 
014700*01 -COPY WWDC99                                                          
014800*01 -COPY WWDCKONS                                                        
014900     SKIP3                                                                
015000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
015100*01 -COPY WMEDAREA                                                        
015200     SKIP3                                                                
015300 01  MESSAGE-CODES.                                                       
015400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015500     03  INF-PART-MISSING        PIC X(3)    VALUE '017'.                 
015600     03  INF-PART-INFO-MISSING   PIC X(3)    VALUE '303'.                 
015700     EJECT                                                                
015800*01  -COPY W335PRIS                                                       
015900     EJECT                                                                
015910*01  -COPY W510CURR                                                       
015920     EJECT                                                                
016000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
016100*                                                                         
016200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016300     SKIP3                                                                
016400*01 -COPY WMSGINIT                                                        
016500     EJECT                                                                
016600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016700*                                                                         
016800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016900     SKIP3                                                                
017000*01  MID -COPY W5I20301                                                   
017100     EJECT                                                                
017200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017300     SKIP3                                                                
017400*01  -COPY WMSGAREA                                                       
017500     EJECT                                                                
017600     03  MOD REDEFINES MSG-AREA.                                          
017700*      05  -COPY W5O20301                                                 
017800     EJECT                                                                
017900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
018000     SKIP3                                                                
018100*01  -COPY WMFSAREA                                                       
018200     EJECT                                                                
018300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018400*                                                                         
018500     EJECT                                                                
018600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018700     SKIP3                                                                
018800 01  NYCKLAR-TILL-DLI.                                                    
018900     03  W-IDARTNR-X.                                                     
019000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019100     03  W-KDSEGKY-X.                                                     
019200         05  W-KDSEGKY           PIC S9(1)   VALUE ZERO COMP-3.           
019300     03  W-WDK621KY-X.                                                    
019400         05  W-WDK621KY          PIC X(5)    VALUE SPACE.                 
019500     03  W-IDDC-X.                                                        
019600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
019700     03  W-IDSKYLT-X.                                                     
019800         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
019900     03  W-WDD3BSEQ-X.                                                    
020000         05  W-D3BSEQ-IDARTNR    PIC S9(9) COMP-3.                        
021600     03  W-IDLAND-X.                                                      
021700         05    W-IDLAND      PIC X(2)    VALUE SPACE.                     
021800                                                                          
021900     03  W-IDLEVNR-X.                                                     
022000         05    W-IDLEVNR     PIC X(5)    VALUE SPACE.                     
022601     SKIP2                                                                
022701 01  W-KVLS                      PIC S9(7)   VALUE ZERO.                  
022801 01  W-IDLEVNR                   PIC X(5)    VALUE SPACE.                 
022901 01  W-PRAVCOST-41               PIC 9(7)V9(2) VALUE ZERO.                
023001 01  W-PRAVCOST-42               PIC 9(7)V9(2) VALUE ZERO.                
023101 01  W-PRAVCOST-43               PIC 9(7)V9(2) VALUE ZERO.                
023201 01  W-PRAVCOST-51               PIC 9(7)V9(2) VALUE ZERO.                
023301*    --- STATUS-KOD FRÅN IMS                                              
023401 01  STATUS-WS                   PIC XX.                                  
023501     88  SEGMENT-FINNS                       VALUE '  '.                  
023601     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023701     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023801     SKIP2                                                                
023901 01  GODK-STATUSKODER.                                                    
024001     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024101     SKIP3                                                                
024201 01  SSA1                        PIC X(64).                               
024301 01  SSA2                        PIC X(64).                               
024401 01  SSA3                        PIC X(64).                               
024501     EJECT                                                                
024601*    --- IMS FUNKTIONSKODER                                               
024701*01  -COPY W0003                                                          
024801     EJECT                                                                
024901*    ---  DLI INPUT-OUTPUT AREA                                           
025001                                                                          
025101 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
025201 01  DLI-IO-WDK601.                                                       
025301*    03  -COPY WDK601                                                     
025401     EJECT                                                                
025501 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
025601 01  DLI-IO-WDK611.                                                       
025701*    03  -COPY WDK611                                                     
025801     EJECT                                                                
025901 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK621'.                      
026001 01  DLI-IO-WDK621.                                                       
026101*    03  -COPY WDK621                                                     
026201 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK701'.                      
026301 01  DLI-IO-WDK701.                                                       
026401*    03  -COPY WDK701                                                     
026501     EJECT                                                                
026601 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711'.                      
026701 01  DLI-IO-WDK711.                                                       
026801*    03  -COPY WDK711                                                     
026901 01  FILLER                      PIC X(24)  VALUE 'DLI-IO-WDK724'.        
027001 01  DLI-IO-WDK724.                                                       
027101*    03  -COPY WDK724                                                     
027201 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD301'.                      
027301 01  DLI-IO-WDD301.                                                       
027401*    03  -COPY WDD301                                                     
027501     EJECT                                                                
027601 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD311'.                      
027701 01  DLI-IO-WDD311.                                                       
027801*    03  -COPY WDD311                                                     
029001 01  FILLER                    PIC X(16)  VALUE 'WDF101'.                 
029101*01  WLLEVA01 -COPY WDF101                                                
029201*                                                                         
029301 01  FILLER                    PIC X(16)  VALUE 'WDF102'.                 
029401*01  WLLEVA11 -COPY WDF102 -PRE LEV-                                      
029501*                                                                         
029601 LINKAGE SECTION.                                                         
029701*01  -COPY W0009   -PRE MSG-                                              
029801*01  -COPY W0008   -PRE USEA-                                             
029901     05  FILLER                  PIC X.                                   
030001     EJECT                                                                
030101*01  -COPY W0008  -PRE WDK6-                                              
030201     05  FILLER                  PIC X.                                   
030301     EJECT                                                                
030401*01  -COPY W0008  -PRE WDK7-                                              
030501     05  FILLER                  PIC X.                                   
030601     EJECT                                                                
030701*01  -COPY W0008  -PRE WDD3-                                              
030801     05  FILLER                  PIC X.                                   
030901     EJECT                                                                
031001*01  -COPY W0008  -PRE WDG2-                                              
031101     05  FILLER                  PIC X.                                   
031201     EJECT                                                                
031301 01  ARTC-P                      PIC X.                                   
031401 01  WDK7-P                      PIC X.                                   
031501 01  GMTA-P                      PIC X.                                   
031601 01  BETA-P                      PIC X.                                   
031701 01  PRIA-P                      PIC X.                                   
031801 01  PRIB-P                      PIC X.                                   
031901 01  PRIS-COST-WDK6-PCB          PIC X.                                   
032001 01  PRIS-COST-WDK7-PCB          PIC X.                                   
032101 01  PRIS-COST-WDF1-PCB          PIC X.                                   
032201 01  PRIS-COST-9305-PCB          PIC X.                                   
032301 01  PRIS-COST-WDK72-PCB         PIC X.                                   
032401 01  PRIS-COST-WDB6-PCB          PIC X.                                   
032601*01  -COPY W0008     -PRE LEV-                                            
032701     05  FILLER                  PIC X(5).                                
032801     EJECT                                                                
033201 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDK6-PCB WDK7-PCB             
033301     WDD3-PCB WDG2-PCB                                                    
033401     ARTC-P WDK7-P GMTA-P BETA-P PRIA-P PRIB-P                            
033501     PRIS-COST-WDK6-PCB                                                   
033601     PRIS-COST-WDK7-PCB                                                   
033701     PRIS-COST-WDF1-PCB                                                   
033801     PRIS-COST-9305-PCB                                                   
033901     PRIS-COST-WDK72-PCB                                                  
034001     PRIS-COST-WDB6-PCB                                                   
034201     LEV-PCB.                                                             
034301 MAIN SECTION.                                                            
034401     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDK6-PCB WDK7-PCB             
034501     WDD3-PCB WDG2-PCB                                                    
034601     ARTC-P WDK7-P GMTA-P BETA-P PRIA-P PRIB-P                            
034701     PRIS-COST-WDK6-PCB                                                   
034801     PRIS-COST-WDK7-PCB                                                   
034901     PRIS-COST-WDF1-PCB                                                   
035001     PRIS-COST-9305-PCB                                                   
035101     PRIS-COST-WDK72-PCB                                                  
035201     PRIS-COST-WDB6-PCB                                                   
035401     LEV-PCB.                                                             
035501                                                                          
035601*---- FLYTTA SUBPROGRAM CALL TILL RÄTT STÄLLE --                          
035701                                                                          
035801                                                                          
035901*------------------------                                                 
036001     PERFORM IMS-GET-MSG                                                  
036101     IF SEGMENT-FINNS                                                     
036201       PERFORM A-INIT                                                     
036301       PERFORM B-KOLLA-NYCKLAR                                            
036401       IF NYCKLAR-OK                                                      
036501         PERFORM F-LAES-VISA-INFO                                         
036601       END-IF                                                             
036701       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O20301 + 4                      
036801       PERFORM IMS-INSERT-MSG                                             
036901     END-IF                                                               
037001                                                                          
037101     MOVE ZERO TO RETURN-CODE                                             
037201     GOBACK                                                               
037301     .                                                                    
037401     EJECT                                                                
037501 A-INIT SECTION.                                                          
037601                                                                          
037701     IF MSG-DUBBLA-TRANSKODER                                             
037801       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I20301                 
037901       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
038001       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
038101     ELSE                                                                 
038201       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I20301                  
038301       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
038401       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
038501     END-IF                                                               
038601                                                                          
038701     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
038801     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
038901     MOVE MFS-IDTRANS TO W-IDTRANS                                        
039001                                                                          
039101     MOVE LOW-VALUE TO MSG-AREA                                           
039201     MOVE 'W5O203N1' TO MFS-IDMOD                                         
039301     MOVE '5203' TO MOD-IDTRANS                                           
039401     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
039501                                                                          
039601     IF EGEN-MID OR HELP-MID                                              
039701       CONTINUE                                                           
039801     ELSE                                                                 
039901       MOVE SPACE TO MFS-KDTRTYP                                          
040001       MOVE '7' TO MFS-IDPFK                                              
040101     END-IF                                                               
040201                                                                          
040301*                                                                         
040401     MOVE FUNCTION CURRENT-DATE (1:8) TO W-AAAAMMDD                       
040501     MOVE W-AAAAMMDD TO W-AAAAMMDD-NUM                                    
040601*    >> CHANGE THE FOLLOWING COMPUTE MANUALLY                             
040701     COMPUTE W-DAPRLIST-9KOMPL  = 99999999 - W-AAAAMMDD-NUM               
040702                                                                          
040703     MOVE W-AAAAMMDD-NUM(3:2)         TO W-DATE-AAMM(1:2)                 
040704     MOVE W-AAAAMMDD-NUM(5:2)         TO W-DATE-AAMM(3:2)                 
041001     .                                                                    
041101     EJECT                                                                
041201 B-KOLLA-NYCKLAR SECTION.                                                 
041301                                                                          
041401     MOVE ALL '+'           TO MSGI-WMSGINIT                              
041501     MOVE '001'             TO MSGI-KDCALL                                
041601     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
041701     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
041801     MOVE '5203'            TO MSGI-IDTRANS                               
041901     IF GODK-MID                                                          
042001         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
042201     END-IF                                                               
042301                                                                          
042701                                                                          
042801     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042901                                                                          
043001     MOVE JA TO NYCKLAR-SW                                                
043101                                                                          
043201                                                                          
043301*    -- KONTROLL AV IDARTNR                                               
043401     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
043501                                                                          
043601     IF MID-IDARTNR-IN NOT = ALL '+'                                      
043701       MOVE '7'         TO MFS-IDPFK                                      
043801       MOVE SPACE       TO MFS-KDTRTYP                                    
043901     END-IF                                                               
044001     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
044101     IF MSGI-IDARTNR NUMERIC                                              
044201       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
044301     ELSE                                                                 
044401       MOVE NEJ TO NYCKLAR-SW                                             
044501     END-IF                                                               
044601                                                                          
044701                                                                          
044801     IF GODK-MID OR NYCKLAR-OK                                            
044901       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
045001       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
045101     ELSE                                                                 
045201       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
045301     END-IF                                                               
045401                                                                          
045501     IF NYCKLAR-FEL                                                       
045601       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
045701       MOVE 'GB'              TO MED-IDSKYLT                              
045801       CALL WMEDKONV USING MED-WMEDAREA                                   
045901       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
046001       PERFORM MFS-RENSA-FAELT-IN                                         
046101       PERFORM MFS-RENSA-FAELT-UT                                         
046201     END-IF                                                               
046301     .                                                                    
046401     EJECT                                                                
046501 F-LAES-VISA-INFO SECTION.                                                
046601                                                                          
046701     PERFORM FA-LAES-GRUNDDATA                                            
046801                                                                          
046901     IF SEGMENT-SAKNAS                                                    
047001        MOVE INF-PART-MISSING TO MED-IDMFSFEL                             
047101        MOVE 'GB'             TO MED-IDSKYLT                              
047201        CALL WMEDKONV USING MED-WMEDAREA                                  
047301        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
047501        PERFORM MFS-RENSA-FAELT-UT                                        
047601     ELSE                                                                 
047701        PERFORM FB-LAES-RADDATA                                           
047801     END-IF                                                               
047901                                                                          
048001     .                                                                    
048101     EJECT                                                                
048201 FA-LAES-GRUNDDATA SECTION.                                               
048301                                                                          
048401     PERFORM IMS-GU-WDK601                                                
048501                                                                          
048601     IF SEGMENT-FINNS                                                     
048701        PERFORM IMS-GNP-WDK611                                            
048801        IF SEGMENT-FINNS                                                  
048901           MOVE CLAG-KDPSLLOC TO MOD-KDPSLLOC                             
049001        END-IF                                                            
049101     END-IF                                                               
049201     .                                                                    
049301     EJECT                                                                
049401 FB-LAES-RADDATA SECTION.                                                 
049501                                                                          
049601*    PERFORM IMS-GNP-WDK611                                               
049701*                                                                         
049801*    IF SEGMENT-SAKNAS                                                    
049901*       MOVE SPACE         TO MOD-KDPSLLOC                                
050001*    ELSE                                                                 
050101*       MOVE CLAG-KDPSLLOC TO MOD-KDPSLLOC                                
050201*    END-IF                                                               
050301                                                                          
050401     MOVE ART-IDFKNGRP TO MOD-IDFKNGRP                                    
050501                                                                          
050601     MOVE W-IDARTNR       TO W-D3BSEQ-IDARTNR                             
050701     MOVE 'USA'           TO W-IDSKYLT                                    
050801                                                                          
050901     PERFORM IMS-GU-WDD301                                                
051101                                                                          
051201     IF SEGMENT-SAKNAS                                                    
051301        MOVE SPACE      TO MOD-BEART                                      
051401     ELSE                                                                 
051501        MOVE TEXT-BEART TO MOD-BEART                                      
051601     END-IF                                                               
051701     PERFORM IMS-GU-WDK701                                                
051801                                                                          
051901     IF SEGMENT-SAKNAS                                                    
052001        MOVE INF-PART-INFO-MISSING TO MED-IDMFSFEL                        
052101        MOVE 'GB'             TO MED-IDSKYLT                              
052201        CALL WMEDKONV USING MED-WMEDAREA                                  
052301        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
052401     ELSE                                                                 
052501      MOVE 'N' TO W-GE-WDK711                                             
052601      PERFORM UNTIL NDC-CA OR W-GE-WDK711 = 'Y'                           
052701        PERFORM IMS-GNP-WDK711                                            
052801        IF SEGMENT-SAKNAS                                                 
052901          MOVE INF-PART-INFO-MISSING TO MED-IDMFSFEL                      
053001          MOVE 'GB'           TO MED-IDSKYLT                              
053101          CALL WMEDKONV USING MED-WMEDAREA                                
053201          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
053301          MOVE 'Y' TO W-GE-WDK711                                         
053401        ELSE                                                              
053501          MOVE SLAG-IDDC      TO WS-IDDC                                  
053601                                 W-IDDC                                   
053701          IF NDC-US-RU                                                    
053801            MOVE SLAG-KVAKS-PAV TO W-KVAKS-PAV-41                         
053901            MOVE SLAG-KVAKS-SDC TO W-KVAKS-SDC-41                         
054001            MOVE SLAG-KVLS    TO W-KVLS-41                                
054101            MOVE SLAG-KVEFRS  TO W-KVEFRS-41                              
054201            MOVE SLAG-KVUTRS  TO W-KVUTRS-41                              
054301            MOVE SLAG-PRAVCOST TO MOD-PRAVCOST-41                         
054401            MOVE SLAG-PRAVCOST TO W-PRAVCOST-41                           
054501            MOVE SLAG-KVLS    TO W-KVLS                                   
054601            COMPUTE MOD-SUAVCOST-41 ROUNDED =                             
054701                W-PRAVCOST-41 * (W-KVLS-41 + W-KVEFRS-41)                 
054801            PERFORM FBF-GET-WDK724-PRICE                                  
054901           IF PRICE-FOUND                                                 
055001* IF PRICE IS OBTAINED FROM WDK724                                        
055101              CONTINUE                                                    
055201           ELSE                                                           
055301* IF NO WDK724 OCCURRENCE FOUND                                           
055401            MOVE SLAG-IDLEVNR TO MOD-IDLEVNR-41                           
055501            MOVE SLAG-IDLEVNR TO W-IDLEVNR                                
055601            IF W-IDLEVNR = '1441 ' OR 'BP2TW'                             
055701               MOVE 8141 TO PRIS-IDDISTR                                  
055801               MOVE WC-NDC-US-RU TO PRIS-IDDC                             
055901               PERFORM FBA-CALL-W335PRIS                                  
056001               MOVE PRIS-PRARTNTO TO MOD-PRARTBEL-41                      
056101               MOVE PRIS-PRARTNTO TO W-PRARTBEL                           
056201            ELSE                                                          
056301               PERFORM FBB-WDK621-PRIS                                    
056401               IF STATUS-WS NOT = 'GE'                                    
056501                  MOVE PRL-PRARTBEL-PR TO MOD-PRARTBEL-41                 
056601                  MOVE PRL-PRARTBEL-PR TO W-PRARTBEL                      
056701               END-IF                                                     
056801            END-IF                                                        
056901            IF W-IDLEVNR = '1441 ' OR 'BP2TW' OR                          
057001                  PRL-KDVALISO = 'SEK'                                    
057101              MOVE 'USD'          TO CURR-KDVALISO-ROW                    
057201            ELSE                                                          
057301              MOVE PRL-KDVALISO TO CURR-KDVALISO-ROW                      
057401            END-IF                                                        
057501            PERFORM FBC-READ-VALUTA                                       
057601            IF W-VALUTA-FINNS = 'Y'                                       
057701              MOVE CURR-PRKURS-NEW  TO W-PRKURS                           
057801              IF W-IDLEVNR = '1441 ' OR 'BP2TW' OR                        
057901                 PRL-KDVALISO = 'SEK'                                     
058001                MOVE W-PRKURS       TO MOD-PRKURS-41                      
058101                MOVE W-PRKURS       TO W-PRKURS2                          
058201              ELSE                                                        
058301                MOVE W-PRKURS       TO W-FIRST-PRKURS                     
058401                MOVE 'USD'          TO CURR-KDVALISO-ROW                  
058501                PERFORM FBC-READ-VALUTA                                   
058601                IF W-VALUTA-FINNS = 'Y'                                   
058701                  COMPUTE W-PRKURS2 ROUNDED =                             
058801                                       W-PRKURS / W-FIRST-PRKURS          
058901                  MOVE W-PRKURS2    TO MOD-PRKURS-41                      
059001                ELSE                                                      
059101                  MOVE ZERO         TO MOD-PRKURS-41                      
059201                                       W-PRKURS2                          
059301                END-IF                                                    
059401              END-IF                                                      
059501              IF W-PRKURS2 = ZERO                                         
059601                MOVE ZERO           TO MOD-PRARTBEU-41                    
059701              ELSE                                                        
059801                COMPUTE W-PRARTBEU ROUNDED =                              
059901                                      W-PRARTBEL / W-PRKURS2              
060001                MOVE W-PRARTBEU     TO MOD-PRARTBEU-41                    
060101              END-IF                                                      
060201            ELSE                                                          
060301              MOVE ZERO             TO MOD-PRKURS-41                      
060401                                       MOD-PRARTBEU-41                    
060501            END-IF                                                        
060601           END-IF                                                         
060701          ELSE                                                            
067201            IF NDC-US-LA                                                  
067301             MOVE SLAG-KVAKS-PAV TO W-KVAKS-PAV-43                        
067401             MOVE SLAG-KVAKS-SDC TO W-KVAKS-SDC-43                        
067501             MOVE SLAG-KVLS TO W-KVLS-43                                  
067601             MOVE SLAG-KVEFRS TO W-KVEFRS-43                              
067701             MOVE SLAG-KVUTRS TO W-KVUTRS-43                              
067801             MOVE SLAG-PRAVCOST TO MOD-PRAVCOST-43                        
067901             MOVE SLAG-PRAVCOST TO W-PRAVCOST-43                          
068001             MOVE SLAG-KVLS TO W-KVLS                                     
068101             COMPUTE MOD-SUAVCOST-43 ROUNDED =                            
068201               W-PRAVCOST-43 * (W-KVLS-43 + W-KVEFRS-43)                  
068301             PERFORM FBF-GET-WDK724-PRICE                                 
068401             IF PRICE-FOUND                                               
068501**IF PRICE IS OBTAINED FROM WDK724                                        
068601              CONTINUE                                                    
068701             ELSE                                                         
068801**IF NO WDK724 OCCURRENCE FOUND                                           
068901              MOVE SLAG-IDLEVNR TO MOD-IDLEVNR-43                         
069001              MOVE SLAG-IDLEVNR TO W-IDLEVNR                              
069101              IF W-IDLEVNR = '1441 ' OR 'BP2TW'                           
069201                 MOVE 8143 TO PRIS-IDDISTR                                
069301                 MOVE WC-NDC-US-LA TO PRIS-IDDC                           
069401                 PERFORM FBA-CALL-W335PRIS                                
069501                 MOVE PRIS-PRARTNTO TO MOD-PRARTBEL-43                    
069601                 MOVE PRIS-PRARTNTO TO W-PRARTBEL                         
069701              ELSE                                                        
069801                 PERFORM FBB-WDK621-PRIS                                  
070001                 IF STATUS-WS NOT = 'GE'                                  
070101                    MOVE PRL-PRARTBEL-PR TO MOD-PRARTBEL-43               
070201                    MOVE PRL-PRARTBEL-PR TO W-PRARTBEL                    
070301                 END-IF                                                   
070401              END-IF                                                      
070601              IF W-IDLEVNR = '1441 ' OR 'BP2TW'                           
070701                 OR PRL-KDVALISO = 'SEK'                                  
070801                MOVE 'USD'      TO CURR-KDVALISO-ROW                      
070901              ELSE                                                        
071001                MOVE PRL-KDVALISO TO CURR-KDVALISO-ROW                    
071101              END-IF                                                      
071201              PERFORM FBC-READ-VALUTA                                     
071301              IF W-VALUTA-FINNS = 'Y'                                     
071401                MOVE CURR-PRKURS-NEW TO W-PRKURS                          
071501                IF W-IDLEVNR = '1441 ' OR 'BP2TW'                         
071601                   OR PRL-KDVALISO = 'SEK'                                
071701                  MOVE W-PRKURS TO MOD-PRKURS-43                          
071801                  MOVE W-PRKURS TO W-PRKURS2                              
071901                ELSE                                                      
072001                  MOVE W-PRKURS TO W-FIRST-PRKURS                         
072101                  MOVE 'USD'    TO CURR-KDVALISO-ROW                      
072201                  PERFORM FBC-READ-VALUTA                                 
072301                  IF W-VALUTA-FINNS = 'Y'                                 
072401                    COMPUTE W-PRKURS2 ROUNDED =                           
072501                                       W-PRKURS / W-FIRST-PRKURS          
072601                    MOVE W-PRKURS2 TO MOD-PRKURS-43                       
072701                  ELSE                                                    
072801                    MOVE ZERO      TO MOD-PRKURS-43                       
072901                                      W-PRKURS2                           
073001                  END-IF                                                  
073101                END-IF                                                    
073201                IF W-PRKURS2 = ZERO                                       
073301                  MOVE ZERO        TO MOD-PRARTBEU-43                     
073401                ELSE                                                      
073501                  COMPUTE W-PRARTBEU ROUNDED =                            
073601                                      W-PRARTBEL / W-PRKURS2              
073701                  MOVE  W-PRARTBEU TO MOD-PRARTBEU-43                     
073801                END-IF                                                    
073901              ELSE                                                        
074001                MOVE ZERO          TO MOD-PRKURS-43                       
074101                                      MOD-PRARTBEU-43                     
074201              END-IF                                                      
074301             END-IF                                                       
074401              ELSE                                                        
074501                IF NDC-CA                                                 
074601                  MOVE SLAG-KVAKS-PAV TO W-KVAKS-PAV-51                   
074701                  MOVE SLAG-KVAKS-SDC TO W-KVAKS-SDC-51                   
074801                  MOVE SLAG-KVLS TO W-KVLS-51                             
074901                  MOVE SLAG-KVEFRS TO W-KVEFRS-51                         
075001                  MOVE SLAG-KVUTRS TO W-KVUTRS-51                         
075101                  MOVE SLAG-PRAVCOST TO MOD-PRAVCOST-51                   
075201                  MOVE SLAG-PRAVCOST TO W-PRAVCOST-51                     
075301                  MOVE SLAG-KVLS TO W-KVLS                                
075401                  COMPUTE MOD-SUAVCOST-51 ROUNDED =                       
075501                    W-PRAVCOST-51 * (W-KVLS-51 + W-KVEFRS-51)             
075601                  MOVE SLAG-IDLEVNR TO MOD-IDLEVNR-51                     
075701                  MOVE SLAG-IDLEVNR TO W-IDLEVNR                          
075801                  IF W-IDLEVNR = '1441 ' OR 'BP2TW'                       
075901                     MOVE 8151 TO PRIS-IDDISTR                            
076201                     MOVE WC-NDC-CA TO PRIS-IDDC                          
076301                     PERFORM FBA-CALL-W335PRIS                            
076401                     MOVE PRIS-PRARTNTO TO MOD-PRARTBEL-51                
076501                     MOVE PRIS-PRARTNTO TO W-PRARTBEL                     
076601                  ELSE                                                    
076701                     PERFORM FBB-WDK621-PRIS                              
076801                     IF STATUS-WS NOT = 'GE'                              
076901                        MOVE PRL-PRARTBEL-PR TO MOD-PRARTBEL-51           
077001                        MOVE PRL-PRARTBEL-PR TO W-PRARTBEL                
077101                     END-IF                                               
077201                  END-IF                                                  
077601                  IF W-IDLEVNR = '1441 ' OR 'BP2TW'                       
077701                     OR PRL-KDVALISO = 'SEK'                              
077801                    MOVE 'CAD'    TO CURR-KDVALISO-ROW                    
077901                  ELSE                                                    
078001                    MOVE PRL-KDVALISO TO CURR-KDVALISO-ROW                
078101                  END-IF                                                  
078201                  PERFORM FBC-READ-VALUTA                                 
078301                  IF W-VALUTA-FINNS = 'Y'                                 
078401                     MOVE CURR-PRKURS-NEW TO W-PRKURS                     
078501                    IF W-IDLEVNR = '1441 ' OR 'BP2TW'                     
078601                       OR PRL-KDVALISO = 'SEK'                            
078701                       MOVE W-PRKURS TO MOD-PRKURS-51                     
078801                       MOVE W-PRKURS TO W-PRKURS2                         
078901                     ELSE                                                 
079001                       MOVE W-PRKURS TO W-FIRST-PRKURS                    
079101                       MOVE 'CAD' TO CURR-KDVALISO-ROW                    
079201                       PERFORM FBC-READ-VALUTA                            
079301                       IF W-VALUTA-FINNS = 'Y'                            
079401                          COMPUTE W-PRKURS2 ROUNDED =                     
079501                                         W-PRKURS / W-FIRST-PRKURS        
079601                          MOVE W-PRKURS2 TO MOD-PRKURS-51                 
079701                       ELSE                                               
079801                          MOVE ZERO  TO MOD-PRKURS-51                     
079901                                        W-PRKURS2                         
080001                       END-IF                                             
080101                     END-IF                                               
080201                     IF W-PRKURS2 = ZERO                                  
080301                       MOVE ZERO       TO MOD-PRARTBEU-51                 
080401                     ELSE                                                 
080501                       COMPUTE W-PRARTBEU ROUNDED =                       
080601                                            W-PRARTBEL / W-PRKURS2        
080701                       MOVE W-PRARTBEU TO MOD-PRARTBEU-51                 
080801                     END-IF                                               
080901                  ELSE                                                    
081001                    MOVE ZERO TO MOD-PRKURS-51                            
081101                    MOVE ZERO TO MOD-PRARTBEU-51                          
081201                  END-IF                                                  
081301                END-IF                                                    
081401              END-IF                                                      
081601          END-IF                                                          
081701        END-IF                                                            
081801      END-PERFORM                                                         
081901      PERFORM FBD-CHECK-IF-BYTES                                          
082001      IF W-BYTE-ART = 'Y'                                                 
082101         PERFORM FBE-READ-AND-ADD-SALDO                                   
082201      ELSE                                                                
082301         MOVE W-KVAKS-PAV-41 TO MOD-KVAKS-PAV-41                          
082401         MOVE W-KVAKS-SDC-41 TO MOD-KVAKS-SDC-41                          
082501         MOVE W-KVLS-41      TO MOD-KVLS-41                               
082601         MOVE W-KVEFRS-41    TO MOD-KVEFRS-41                             
082701         MOVE W-KVUTRS-41    TO MOD-KVUTRS-41                             
082801         MOVE W-KVAKS-PAV-42 TO MOD-KVAKS-PAV-42                          
082901         MOVE W-KVAKS-SDC-42 TO MOD-KVAKS-SDC-42                          
083001         MOVE W-KVLS-42      TO MOD-KVLS-42                               
083101         MOVE W-KVEFRS-42    TO MOD-KVEFRS-42                             
083201         MOVE W-KVUTRS-42    TO MOD-KVUTRS-42                             
083301         MOVE W-KVAKS-PAV-43 TO MOD-KVAKS-PAV-43                          
083401         MOVE W-KVAKS-SDC-43 TO MOD-KVAKS-SDC-43                          
083501         MOVE W-KVLS-43      TO MOD-KVLS-43                               
083601         MOVE W-KVEFRS-43    TO MOD-KVEFRS-43                             
083701         MOVE W-KVUTRS-43    TO MOD-KVUTRS-43                             
083801         MOVE W-KVAKS-PAV-51 TO MOD-KVAKS-PAV-51                          
083901         MOVE W-KVAKS-SDC-51 TO MOD-KVAKS-SDC-51                          
084001         MOVE W-KVLS-51      TO MOD-KVLS-51                               
084101         MOVE W-KVEFRS-51    TO MOD-KVEFRS-51                             
084201         MOVE W-KVUTRS-51    TO MOD-KVUTRS-51                             
084301      END-IF                                                              
084401     END-IF                                                               
084501     .                                                                    
084601     EJECT                                                                
084701                                                                          
084801 FBA-CALL-W335PRIS SECTION.                                               
084901     MOVE 1          TO PRIS-KDCALL                                       
085001     MOVE 1          TO PRIS-KVBEART                                      
085101     MOVE 0          TO PRIS-IDKUNDNR                                     
085201*  JUST FOR TESTING!!!!!!!!!!!!!!!!  WITH KUNDNR 235                      
085301*    MOVE 235        TO PRIS-IDKUNDNR                                     
085401     MOVE 4          TO PRIS-KDORDKL                                      
085501     MOVE 'W5020300' TO PRIS-IDPGM                                        
085601     MOVE SPACE      TO PRIS-FLINVEST                                     
085701     MOVE W-IDARTNR  TO PRIS-IDARTNR                                      
085801                                                                          
085901     CALL W335PRIS USING PRIS-W335PRIS                                    
086001                         ARTC-P WDK7-P GMTA-P BETA-P                      
086101                         PRIA-P PRIB-P                                    
086201                         PRIS-COST-WDK6-PCB                               
086301                         PRIS-COST-WDK7-PCB                               
086401                         PRIS-COST-WDF1-PCB                               
086501                         PRIS-COST-9305-PCB                               
086601                         PRIS-COST-WDK72-PCB                              
086701                         PRIS-COST-WDB6-PCB                               
086901                                                                          
087001     .                                                                    
087101     EJECT                                                                
087201                                                                          
087301 FBB-WDK621-PRIS SECTION.                                                 
087401     PERFORM IMS-GU-WDK601                                                
087501     PERFORM IMS-GNP-WDK611                                               
087601     IF SEGMENT-SAKNAS                                                    
087701        MOVE INF-PART-MISSING TO MED-IDMFSFEL                             
087801        MOVE 'GB'             TO MED-IDSKYLT                              
087901        CALL WMEDKONV USING MED-WMEDAREA                                  
088001        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
088201     ELSE                                                                 
088301        PERFORM IMS-GNP-WDK621                                            
088401     END-IF                                                               
088501                                                                          
088601     IF SEGMENT-SAKNAS                                                    
088701        MOVE INF-PART-MISSING TO MED-IDMFSFEL                             
088801        MOVE 'GB'             TO MED-IDSKYLT                              
088901        CALL WMEDKONV USING MED-WMEDAREA                                  
089001        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
089201     ELSE                                                                 
089301       PERFORM UNTIL (PRL-IDLEVNR = SLAG-IDLEVNR AND                      
089401         PRL-KDSTATUS-PR > 0 AND                                          
089501         W-DAPRLIST-9KOMPL <= PRL-DAPRLIST-9KOMPL) OR                     
089601         STATUS-WS = 'GE'                                                 
089701                                                                          
089801          PERFORM IMS-GNP-WDK621                                          
089901       END-PERFORM                                                        
090001     END-IF                                                               
090101                                                                          
090201     .                                                                    
090301     EJECT                                                                
090401                                                                          
092601 FBC-READ-VALUTA SECTION.                                                 
092701                                                                          
093201     MOVE 'Y' TO W-VALUTA-FINNS                                           
093202                                                                          
093203     MOVE WS-KDVALISO-HUV            TO CURR-KDVALISO-HUV                 
093204     MOVE W-DATE-AAMM                TO CURR-TIAAMM                       
093205     MOVE 'M'                        TO CURR-KDVALTYP                     
093206                                                                          
093301     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
093401     IF CURR-KDSVAR = ' '                                                 
093402        MOVE CURR-PRKURS-NEW         TO W-PRKURS                          
093403     ELSE                                                                 
093501        MOVE INF-PART-MISSING        TO MED-IDMFSFEL                      
093601        MOVE 'GB'                    TO MED-IDSKYLT                       
093701        CALL WMEDKONV USING MED-WMEDAREA                                  
093801        MOVE MED-MFSFEL              TO MOD-TEMFSFEL                      
093901        MOVE 'N'                     TO W-VALUTA-FINNS                    
094001        MOVE ZERO                    TO W-PRKURS                          
094301     END-IF                                                               
094401     .                                                                    
094501     EJECT                                                                
094601 FBD-CHECK-IF-BYTES SECTION.                                              
094701     MOVE W-IDARTNR TO BYT20-IDARTNR                                      
094801       IF BYT20-BYTES                                                     
094901             SUBTRACT +6000 FROM   W-IDARTNR                              
095001             MOVE 'Y'  TO   W-BYTE-ART                                    
095101       ELSE                                                               
095201         IF BYT20-RADIO                                                   
095301             SUBTRACT +1000 FROM   W-IDARTNR                              
095401             MOVE 'Y'  TO   W-BYTE-ART                                    
095501         ELSE                                                             
095601             MOVE 'N'  TO   W-BYTE-ART                                    
095701         END-IF                                                           
095801       END-IF                                                             
095901                                                                          
096001     .                                                                    
096101     EJECT                                                                
096201 FBE-READ-AND-ADD-SALDO SECTION.                                          
096301     PERFORM IMS-GU-WDK701                                                
096401                                                                          
096501     IF SEGMENT-SAKNAS                                                    
096601        MOVE 'GB'             TO MED-IDSKYLT                              
096701        CALL WMEDKONV USING MED-WMEDAREA                                  
096801        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
096901        MOVE 'N'  TO   W-BYTE-ART                                         
097001     ELSE                                                                 
097101      MOVE 'N' TO W-GE-WDK711                                             
097201      MOVE ZERO TO SLAG-IDDC                                              
097301                   WS-IDDC                                                
097401      PERFORM UNTIL NDC-CA OR W-GE-WDK711 = 'Y'                           
097501        PERFORM IMS-GNP-WDK711                                            
097601        IF SEGMENT-SAKNAS                                                 
097701          MOVE 'GB'           TO MED-IDSKYLT                              
097801          CALL WMEDKONV USING MED-WMEDAREA                                
097901          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
098001          MOVE 'Y' TO W-GE-WDK711                                         
098101        ELSE                                                              
098201          MOVE SLAG-IDDC   TO WS-IDDC                                     
098301          EVALUATE TRUE                                                   
098401            WHEN NDC-US-RU                                                
098501              ADD SLAG-KVAKS-PAV TO W-KVAKS-PAV-41                        
098601                               GIVING MOD-KVAKS-PAV-41                    
098701              ADD SLAG-KVAKS-SDC TO W-KVAKS-SDC-41                        
098801                               GIVING MOD-KVAKS-SDC-41                    
098901              ADD SLAG-KVLS    TO W-KVLS-41                               
099001              MOVE W-KVLS-41   TO MOD-KVLS-41                             
099101              ADD SLAG-KVEFRS  TO W-KVEFRS-41                             
099201              MOVE W-KVEFRS-41 TO MOD-KVEFRS-41                           
099301              ADD SLAG-KVUTRS  TO W-KVUTRS-41                             
099401                           GIVING MOD-KVUTRS-41                           
099501              COMPUTE MOD-SUAVCOST-41 ROUNDED =                           
099601                W-PRAVCOST-41 * (W-KVLS-41 + W-KVEFRS-41)                 
101001            WHEN NDC-US-LA                                                
101101              ADD SLAG-KVAKS-PAV TO W-KVAKS-PAV-43                        
101201                               GIVING MOD-KVAKS-PAV-43                    
101301              ADD SLAG-KVAKS-SDC TO W-KVAKS-SDC-43                        
101401                               GIVING MOD-KVAKS-SDC-43                    
101501              ADD SLAG-KVLS   TO W-KVLS-43                                
101601              MOVE W-KVLS-43    TO MOD-KVLS-43                            
101701              ADD SLAG-KVEFRS  TO W-KVEFRS-43                             
101801              MOVE W-KVEFRS-43 TO MOD-KVEFRS-43                           
101901              ADD SLAG-KVUTRS TO W-KVUTRS-43                              
102001                               GIVING MOD-KVUTRS-43                       
102101              COMPUTE MOD-SUAVCOST-43 ROUNDED =                           
102201                W-PRAVCOST-43 * (W-KVLS-43 + W-KVEFRS-43)                 
102301            WHEN NDC-CA                                                   
102401              ADD SLAG-KVAKS-PAV TO W-KVAKS-PAV-51                        
102501                               GIVING MOD-KVAKS-PAV-51                    
102601              ADD SLAG-KVAKS-SDC TO W-KVAKS-SDC-51                        
102701                               GIVING MOD-KVAKS-SDC-51                    
102801              ADD SLAG-KVLS      TO W-KVLS-51                             
102901              MOVE W-KVLS-51     TO MOD-KVLS-51                           
103001              ADD SLAG-KVEFRS    TO W-KVEFRS-51                           
103101              MOVE W-KVEFRS-51   TO MOD-KVEFRS-51                         
103201              ADD SLAG-KVUTRS    TO W-KVUTRS-51                           
103301                               GIVING MOD-KVUTRS-51                       
103401              COMPUTE MOD-SUAVCOST-51 ROUNDED =                           
103501                W-PRAVCOST-51 * (W-KVLS-51 + W-KVEFRS-51)                 
103601          END-EVALUATE                                                    
103701        END-IF                                                            
103801      END-PERFORM                                                         
103901     END-IF                                                               
104001     .                                                                    
104101     EJECT                                                                
104201 FBF-GET-WDK724-PRICE SECTION.                                            
104301                                                                          
104401* GET FIRST OCCURRENCE OF WDK724 WHERE SPRL-SUINLEV-PR IS                 
104501* GREATER THAN 0                                                          
104601     MOVE NEJ                 TO WS-PRICE-SW                              
104701     PERFORM IMS-GNP-WDK724-FIRST                                         
104801     PERFORM UNTIL SEGMENT-SAKNAS OR PRICE-FOUND                          
104901      IF SEGMENT-FINNS                                                    
105001       IF SPRL-SUINLEV-PR > 0                                             
105101        MOVE JA                  TO WS-PRICE-SW                           
105201        PERFORM FBFA-MOVE-PRICE-VALUES                                    
105301       END-IF                                                             
105401      END-IF                                                              
105501      PERFORM IMS-GNP-WDK724-NEXT                                         
105601     END-PERFORM                                                          
105701                                                                          
105801* IF NOT FOUND, GET FIRST OCCURRENCE OF WDK724                            
105901     IF PRICE-FOUND                                                       
106001        CONTINUE                                                          
106101     ELSE                                                                 
106201        PERFORM IMS-GNP-WDK724-FIRST                                      
106301        IF SEGMENT-FINNS                                                  
106401          MOVE JA                    TO WS-PRICE-SW                       
106501          PERFORM FBFA-MOVE-PRICE-VALUES                                  
106601        END-IF                                                            
106701     END-IF                                                               
106801     .                                                                    
106901     EJECT                                                                
107001*                                                                         
107101 FBFA-MOVE-PRICE-VALUES SECTION.                                          
107201     IF NDC-US-RU                                                         
107301        MOVE SPRL-IDLEVNR-PR          TO MOD-IDLEVNR-41                   
107401                                         W-IDLEVNR                        
107501        MOVE SPRL-PRARTBEL-PR         TO MOD-PRARTBEL-41                  
107601                                         W-PRARTBEL                       
107701        MOVE SPRL-KDVALISO            TO CURR-KDVALISO-ROW                
107801*  GET EXCHANGE RATE                                                      
107901        PERFORM FBFB-READ-VALUTA                                          
108001        MOVE W-PRKURS                 TO MOD-PRKURS-41                    
108101        MOVE W-PRARTBEU               TO MOD-PRARTBEU-41                  
108201     ELSE                                                                 
108301        IF NDC-US-LA                                                      
108401           MOVE SPRL-IDLEVNR-PR          TO MOD-IDLEVNR-43                
108501                                            W-IDLEVNR                     
108601           MOVE SPRL-PRARTBEL-PR         TO MOD-PRARTBEL-43               
108701                                            W-PRARTBEL                    
108801           MOVE SPRL-KDVALISO            TO CURR-KDVALISO-ROW             
108901*  GET EXCHANGE RATE                                                      
109001           PERFORM FBFB-READ-VALUTA                                       
109101           MOVE W-PRKURS                 TO MOD-PRKURS-43                 
109201           MOVE W-PRARTBEU               TO MOD-PRARTBEU-43               
109301        END-IF                                                            
109401     END-IF                                                               
109501     .                                                                    
109601     EJECT                                                                
109701*                                                                         
109801 FBFB-READ-VALUTA SECTION.                                                
109901**** RÄKNA OM PRISRADER MED AKTUELL MÅNADSKURS                            
110001     IF SPRL-KDVALISO = 'USD'                                             
110101       MOVE 1 TO W-PRKURS                                                 
110201       MOVE 1 TO W-REVALUTA                                               
110301     ELSE                                                                 
110302       MOVE WS-KDVALISO-HUV     TO CURR-KDVALISO-HUV                      
110303       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
110304       MOVE 'M'                 TO CURR-KDVALTYP                          
110701       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
110801       IF CURR-KDSVAR = ' '                                               
110802         MOVE CURR-PRKURS-NEW  TO W-PRKURS                                
110803         MOVE CURR-REVALUTA-TO TO W-REVALUTA                              
110804       ELSE                                                               
110901         MOVE 1                TO W-PRKURS                                
111001         MOVE 1                TO W-REVALUTA                              
111401       END-IF                                                             
111501     END-IF                                                               
111601     PERFORM IMS-GU-WLLEVA01                                              
111701     IF SEGMENT-SAKNAS                                                    
111801       MOVE 1 TO W-RETULF                                                 
111901     ELSE                                                                 
112001       MOVE 'US'         TO W-IDLAND                                      
112101       PERFORM IMS-GNP-WLLEVA11                                           
112201       IF SEGMENT-FINNS                                                   
112301         IF LEV-TULL-TITULF < DAGENS-AAMMDD                               
112401           MOVE LEV-TULL-RETULF-1 TO W-RETULF                             
112501         ELSE                                                             
112601           MOVE LEV-TULL-RETULF-2 TO W-RETULF                             
112701         END-IF                                                           
112801       ELSE                                                               
112901         MOVE 1 TO W-RETULF                                               
113001       END-IF                                                             
113101     END-IF                                                               
113201     COMPUTE W-PRARTBEU     ROUNDED = W-PRARTBEL * W-RETULF               
113301                                    * W-PRKURS / W-REVALUTA               
113401****                                                                      
113501     .                                                                    
113601     EJECT                                                                
113701                                                                          
113801 MFS-RENSA-FAELT-IN SECTION.                                              
113901                                                                          
114001*    --- ALLA INDATA-FÄLT                                                 
114101     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
114201                                                                          
114301     .                                                                    
114401     EJECT                                                                
114501 MFS-RENSA-FAELT-UT  SECTION.                                             
114601                                                                          
114701*    --- ALLA UTDATA-FÄLT                                                 
114801     MOVE MFS-RENSA-FAELT TO                                              
114901        MOD-BEART                                                         
115001        MOD-IDFKNGRP                                                      
115101        MOD-KDPSLLOC                                                      
115201        MOD-KVAKS-PAV-41                                                  
115301        MOD-KVAKS-PAV-42                                                  
115401        MOD-KVAKS-PAV-43                                                  
115501        MOD-KVAKS-PAV-51                                                  
115601        MOD-KVAKS-SDC-41                                                  
115701        MOD-KVAKS-SDC-42                                                  
115801        MOD-KVAKS-SDC-43                                                  
115901        MOD-KVAKS-SDC-51                                                  
116001        MOD-KVLS-41                                                       
116101        MOD-KVLS-42                                                       
116201        MOD-KVLS-43                                                       
116301        MOD-KVLS-51                                                       
116401        MOD-KVEFRS-41                                                     
116501        MOD-KVEFRS-42                                                     
116601        MOD-KVEFRS-43                                                     
116701        MOD-KVEFRS-51                                                     
116801        MOD-KVUTRS-41                                                     
116901        MOD-KVUTRS-42                                                     
117001        MOD-KVUTRS-43                                                     
117101        MOD-KVUTRS-51                                                     
117201        MOD-PRAVCOST-41                                                   
117301        MOD-PRAVCOST-42                                                   
117401        MOD-PRAVCOST-43                                                   
117501        MOD-PRAVCOST-51                                                   
117601        MOD-SUAVCOST-41                                                   
117701        MOD-SUAVCOST-42                                                   
117801        MOD-SUAVCOST-43                                                   
117901        MOD-SUAVCOST-51                                                   
118001        MOD-IDLEVNR-41                                                    
118101        MOD-IDLEVNR-42                                                    
118201        MOD-IDLEVNR-43                                                    
118301        MOD-IDLEVNR-51                                                    
118401        MOD-PRARTBEL-41                                                   
118501        MOD-PRARTBEL-42                                                   
118601        MOD-PRARTBEL-43                                                   
118701        MOD-PRARTBEL-51                                                   
118801        MOD-PRKURS-41                                                     
118901        MOD-PRKURS-42                                                     
119001        MOD-PRKURS-43                                                     
119101        MOD-PRKURS-51                                                     
119201                                                                          
119301     .                                                                    
119401     SKIP3                                                                
119501* --- IMS SEKTIONER ---                                                   
119601     SKIP3                                                                
119701 IMS-GET-MSG SECTION.                                                     
119801                                                                          
119901     MOVE '  QC' TO GODK-STATUSKODER                                      
120001     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
120101     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
120201     PERFORM IMS-STATUSKONTROLL                                           
120301     .                                                                    
120401     SKIP3                                                                
120501 IMS-INSERT-MSG SECTION.                                                  
120601                                                                          
120701     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
120801       MOVE '0' TO MFS-KDHUVOMR                                           
120901     END-IF                                                               
121001     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
121101     MOVE SPACE TO GODK-STATUSKODER                                       
121201     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
121301     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
121401     PERFORM IMS-STATUSKONTROLL                                           
121501     .                                                                    
121601     EJECT                                                                
122901 IMS-GU-WDK601     SECTION.                                               
123001                                                                          
123201     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
123301          DELIMITED BY SIZE INTO SSA1                                     
123401     MOVE '  GE' TO GODK-STATUSKODER                                      
123501     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
123601     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
123701     PERFORM IMS-STATUSKONTROLL                                           
123801     .                                                                    
123901     EJECT                                                                
124001 IMS-GNP-WDK611    SECTION.                                               
124101                                                                          
124201     MOVE 'WDK611  '            TO SSA1                                   
124301     MOVE '  GE' TO GODK-STATUSKODER                                      
124401     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
124501     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
124601     PERFORM IMS-STATUSKONTROLL                                           
124701     .                                                                    
124801     EJECT                                                                
124901 IMS-GNP-WDK621    SECTION.                                               
125001                                                                          
125201     MOVE 'WDK621  '            TO SSA1                                   
125301     MOVE '  GE' TO GODK-STATUSKODER                                      
125401     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
125501     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
125601     PERFORM IMS-STATUSKONTROLL                                           
125701     .                                                                    
125801     EJECT                                                                
125901 IMS-GU-WDK701    SECTION.                                                
126001                                                                          
126101     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
126201          DELIMITED BY SIZE INTO SSA1                                     
126301     MOVE '  GE' TO GODK-STATUSKODER                                      
126401     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
126501     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
126601     PERFORM IMS-STATUSKONTROLL                                           
126701     .                                                                    
126801     EJECT                                                                
126901 IMS-GNP-WDK711    SECTION.                                               
127001                                                                          
127201     MOVE 'WDK711  '           TO SSA1                                    
127301     MOVE '  GE' TO GODK-STATUSKODER                                      
127401     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
127501     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
127601     PERFORM IMS-STATUSKONTROLL                                           
127701     .                                                                    
127801     EJECT                                                                
127901 IMS-GU-WDD301    SECTION.                                                
128001                                                                          
128101     STRING 'WDD301  (WDD3BSEQ =' W-WDD3BSEQ-X ')'                        
128201          DELIMITED BY SIZE INTO SSA1                                     
128301     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
128401          DELIMITED BY SIZE INTO SSA2                                     
128501     MOVE '  GE' TO GODK-STATUSKODER                                      
128601     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
128701     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
128801     PERFORM IMS-STATUSKONTROLL                                           
128901     .                                                                    
129001     EJECT                                                                
129101 IMS-GNP-WDK724-FIRST SECTION.                                            
129201     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
129301          DELIMITED BY SIZE INTO SSA1                                     
129401     STRING 'WDK711  (IDDC     =' W-IDDC-X     ')'                        
129501          DELIMITED BY SIZE INTO SSA2                                     
129601     MOVE 'WDK724  *F' TO SSA3                                            
129701     MOVE '  GE' TO GODK-STATUSKODER                                      
129801     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1 SSA2 SSA3         
129901     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
130001     PERFORM IMS-STATUSKONTROLL                                           
130101     .                                                                    
130201     SKIP3                                                                
130301                                                                          
130401 IMS-GNP-WDK724-NEXT SECTION.                                             
130501     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
130601          DELIMITED BY SIZE INTO SSA1                                     
130701     STRING 'WDK711  (IDDC     =' W-IDDC-X     ')'                        
130801          DELIMITED BY SIZE INTO SSA2                                     
130901     MOVE 'WDK724   ' TO SSA3                                             
131001     MOVE '  GE' TO GODK-STATUSKODER                                      
131101     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1 SSA2 SSA3         
131201     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
131301     PERFORM IMS-STATUSKONTROLL                                           
131401     .                                                                    
131501     EJECT                                                                
133001 IMS-GU-WLLEVA01 SECTION.                                                 
133101     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
133201     DELIMITED BY SIZE INTO SSA1                                          
133301     MOVE '  GE' TO GODK-STATUSKODER                                      
133401     CALL CBLTDLI USING GU LEV-PCB WLLEVA01 SSA1                          
133501     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
133601     PERFORM IMS-STATUSKONTROLL                                           
133701     .                                                                    
133801     EJECT                                                                
133901                                                                          
134001 IMS-GNP-WLLEVA11 SECTION.                                                
134101     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
134201     DELIMITED BY SIZE INTO SSA1                                          
134301     MOVE '  GE' TO GODK-STATUSKODER                                      
134401     CALL CBLTDLI USING GNP LEV-PCB LEV-WLLEVA11 SSA1                     
134501     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
134601     PERFORM IMS-STATUSKONTROLL                                           
134701     .                                                                    
134801     EJECT                                                                
134901                                                                          
135001 IMS-STATUSKONTROLL SECTION.                                              
135101                                                                          
135201     SET STATUS-IX TO 1                                                   
135301     SEARCH GODK-STATUS                                                   
135401       AT END                                                             
135501         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
135601         DELIMITED BY SIZE INTO FELTEXT                                   
135701         CALL FELLOG                                                      
135801       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
135901         CONTINUE                                                         
136001     END-SEARCH                                                           
137001     .                                                                    

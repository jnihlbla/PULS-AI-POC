000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6018500.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   99/10/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR LEVNR/ARTNR PER EMBALLAGE ARTNR                            
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDK6                                       
001100*                              WDD1                                       
001200*                              WDP3                                       
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W6T185                                              
001600*        MID:         W6I18501                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W6O18501                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W6018500'.            
002900 77  LNG-P-TO-P-PREFIX           PIC S9(4)   VALUE +17 COMP SYNC.         
003000 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300 77  WS-TYP                      PIC X       VALUE SPACE.                 
003400 77  WS-SUM                      PIC X       VALUE SPACE.                 
003500 77  WS-CMD                      PIC X       VALUE SPACE.                 
003600 77  WS-IDPERSON                 PIC X(3)    VALUE SPACE.                 
003700 77  RAD-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
003800 77  RAD-IX-MAX                  PIC S9(3)   VALUE +13  COMP-3.           
003900 77  ARTNR-SOEK                  PIC X       VALUE 'N'.                   
004000 77  ARTNR-LEVNR-SOEK            PIC X       VALUE 'N'.                   
004100 77  TRAEFF-SW                   PIC X       VALUE 'N'.                   
004200 77  VISA-RENSAD-RAD             PIC X       VALUE 'N'.                   
004300 77  VISA-RAD                    PIC X       VALUE 'N'.                   
004400 77  SW-RAKNA-TOT                PIC X       VALUE 'N'.                   
004500 77  WS-TOT-ART-MED              PIC S9(7)   VALUE ZERO.                  
004600 77  WS-TOT-ART-UTAN             PIC S9(7)   VALUE ZERO.                  
004700 77  WS-FINNS-IDFPINST           PIC S9(7)   VALUE ZERO.                  
004800 77  WS-SAKNAR-IDFPINST          PIC S9(7)   VALUE ZERO.                  
004900 77  WS-IDARTNR-SPAR             PIC S9(9)   VALUE ZERO.                  
005000 77  SPAR-TAB-IDFPINST           PIC S9(7)   VALUE ZERO.                  
005100 77  TAB-IX                      PIC S9(7)   VALUE ZERO  COMP-3.          
005200 77  TAB-IX-MAX                  PIC S9(7)   VALUE 50000 COMP-3.          
005300                                                                          
005400 77  HOPP-SW                     PIC X       VALUE 'N'.                   
005500     88  HOPP                                VALUE 'J'.                   
005600     88  EJ-HOPP                             VALUE 'N'.                   
005700                                                                          
005800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005900     88  INDATA-OK                           VALUE 'J'.                   
006000     88  INDATA-FEL                          VALUE 'N'.                   
006100                                                                          
006200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006300     88  NYCKLAR-OK                          VALUE 'J'.                   
006400     88  NYCKLAR-FEL                         VALUE 'N'.                   
006500                                                                          
006600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006700     88  EGEN-MID                            VALUE '6185'.                
006800     88  GODK-MID                            VALUE '6181' '6182'          
006900                                                   '6183' '6184'          
007000                                                   '6185'.                
007100     88  HELP-MID                            VALUE '0551'.                
007200                                                                          
007300 01  IN-KOLL-IDLEVNR.                                                     
007400     03  POS-RESTEN              PIC X(4).                                
007500     03  POS-5                   PIC X.                                   
007600                                                                          
007700 01  TABENTRY-PARM.                                                       
007800     03  STEGLAANGD              PIC S9(9) COMP.                          
007900     03  ANTAL                   PIC S9(9) COMP.                          
008000     03  NYCKELLAANGD            PIC S9(9) COMP.                          
008100 01  SORT-TABELL.                                                         
008200     03  TAB-RAD OCCURS 50000.                                            
008300        05  TAB-SORT-BEGREPP.                                             
008400            07  TAB-IDFPINST    PIC S9(7) COMP-3.                         
008500     EJECT                                                                
008600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008700 01  GENERELLA-SUBPROGRAM.                                                
008800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR '.            
009300     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
009400     EJECT                                                                
009500*01 -COPY W006PRT                                                         
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009800*01 -COPY WMEDAREA                                                        
009900     SKIP3                                                                
010000 01  MESSAGE-CODES.                                                       
010100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010200     03  URVAL-SAKNAS            PIC X(3)    VALUE '005'.                 
010300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010600     03  INF-PRINTING-STARTED    PIC X(3)    VALUE '202'.                 
010700     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
010800     EJECT                                                                
010900 01  FILLER                   PIC X(16) VALUE 'P-TO-P-AREA-PRT'.          
011000 01  PROG-TO-PROG-SW.                                                     
011100*    03  -COPY WMSGSOP                                                    
011200*                                                                         
011300 01  WS-PARAMETRAR.                                                       
011400     03  WS-STYRDATA.                                                     
011410*        05 -COPY W61310  -PRE STYR-                                      
011800     03  WS-PRINTER.                                                      
011900         05  STYR-IDPRINTER      PIC X(8) VALUE SPACE.                    
012000*                                                                         
012100 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
012200 01  P-TO-P-AREA.                                                         
012300     03  P-TO-P-LL               PIC S9(4)            COMP SYNC.          
012400     03  P-TO-P-Z1               PIC  X(1)   VALUE LOW-VALUE.             
012500     03  P-TO-P-Z2               PIC  X(1)   VALUE LOW-VALUE.             
012600     03  P-TO-P-TRANSKOD         PIC  X(7).                               
012700     03  FILLER                  PIC  X(1)   VALUE SPACE.                 
012800     03  P-TO-P-FROM-MID         PIC  X(4).                               
012900     03  P-TO-P-KDMFSFOR         PIC  X(1).                               
013000     03  P-TO-P-DATA             PIC  X(1000).                            
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)  VALUE 'P-TO-P-AREA-2'.        
013300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013600     SKIP3                                                                
013700*01 -COPY WMSGINIT                                                        
013800     EJECT                                                                
013900*                                                                         
014000 01  SPAR-AREA.                                                           
014100     03  SPAR-IDTRANS             PIC X(4)    VALUE '6185'.               
014200     03  SPAR-IDARTNR-ENTER       PIC S9(9)   COMP-3.                     
014300     03  SPAR-IDARTNR-NEXT        PIC S9(9)   COMP-3.                     
014400     03  SPAR-IDFPINST-ENTER      PIC S9(7)   COMP-3.                     
014500     03  SPAR-IDFPINST-NEXT       PIC S9(7)   COMP-3.                     
014600     03  SPAR-KDEMBKEY-ENTER      PIC X(3).                               
014700     03  SPAR-KDEMBKEY-NEXT       PIC X(3).                               
014800     03  SPAR-TOT-IDFPINST        PIC S9(7)   COMP-3.                     
014900     03  SPAR-TOT-IDART           PIC S9(7)   COMP-3.                     
015000     EJECT                                                                
015100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015200*                                                                         
015300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
015400     SKIP3                                                                
015500*01  MID -COPY W6I18501                                                   
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)   VALUE '6183-MID'.            
015800     SKIP3                                                                
015900*01  MID -COPY W6I18301  -PRE 6183-                                       
016000     EJECT                                                                
016100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016200     SKIP3                                                                
016300*01  -COPY WMSGAREA                                                       
016400     EJECT                                                                
016500     03  MOD REDEFINES MSG-AREA.                                          
016600*      05  -COPY W6O18501                                                 
016700     EJECT                                                                
016800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016900     SKIP3                                                                
017000*01  -COPY WMFSAREA                                                       
017100     EJECT                                                                
017200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017300*                                                                         
017400     EJECT                                                                
017500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017600     SKIP3                                                                
017700 01  NYCKLAR-TILL-DLI.                                                    
017800     03  W-IDLEVNR-X.                                                     
017900         05  W-IDLEVNR     PIC  X(5) VALUE SPACE.                         
018000     03  W-IDARTNR-X.                                                     
018100         05  W-IDARTNR     PIC S9(9) VALUE ZERO COMP-3.                   
018200     03  W-IDFPINST-X.                                                    
018300         05  W-IDFPINST    PIC S9(7) VALUE ZERO COMP-3.                   
018400     03  W-IDPERSON-X.                                                    
018500         05  W-IDPERSON    PIC S9(3) VALUE ZERO COMP-3.                   
018600     03  W-WDK6C1KY-MIN.                                                  
018700         05  W-SEQC-IDARTNR-EMB-MIN  PIC S9(9)  VALUE ZERO COMP-3.        
018800         05  W-SEQC-IDARTNR-MIN      PIC S9(9)  VALUE ZERO COMP-3.        
018900         05  W-SEQC-KDEMBKEY-MIN     PIC X(3)   VALUE LOW-VALUE.          
019000     03  W-WDK6C1KY-MAX.                                                  
019100         05  W-SEQC-IDARTNR-EMB-MAX  PIC S9(9)  VALUE 999999999           
019200                                                           COMP-3.        
019300         05  W-SEQC-IDARTNR-MAX   PIC S9(9)  VALUE 9999999 COMP-3.        
019400         05  W-SEQC-KDEMBKEY-MAX     PIC X(3)   VALUE HIGH-VALUE.         
019500     03  W-WDD1A1KY-MIN.                                                  
019600         05  W-SEQA-IDARTNR-MIN   PIC S9(9)  VALUE ZERO COMP-3.           
019700         05  W-SEQA-IDFPINST-MIN  PIC S9(7)  VALUE ZERO COMP-3.           
019800     03  W-WDD1A1KY-MAX.                                                  
019900         05  W-SEQA-IDARTNR-MAX   PIC S9(9)  VALUE 999999999              
020000                                                           COMP-3.        
020100         05  W-SEQA-IDFPINST-MAX  PIC S9(7)  VALUE 9999999 COMP-3.        
020200     03  W-KDARBTYP-X.                                                    
020400         05    W-KDARBTYP    PIC X(8)    VALUE 'QUAL    '.                
020600     EJECT                                                                
020700*    --- STATUS-KOD FRÅN IMS                                              
020800 01  STATUS-WS                   PIC XX.                                  
020900     88  SEGMENT-FINNS                       VALUE '  '.                  
021000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021200     SKIP2                                                                
021300 01  GODK-STATUSKODER.                                                    
021400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021500     SKIP3                                                                
021600 01  SSA1                        PIC X(64).                               
021700 01  SSA2                        PIC X(64).                               
021800     EJECT                                                                
021900*    --- IMS FUNKTIONSKODER                                               
022000*01  -COPY W0003                                                          
022100     EJECT                                                                
022200*    ---  DLI INPUT-OUTPUT AREA                                           
022300                                                                          
022400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
022500 01  DLI-IO-WDK601.                                                       
022600*    03  -COPY WDK601                                                     
022700     EJECT                                                                
022800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK6C'.                       
022900 01  DLI-IO-WDK6C.                                                        
023000*    03  -COPY WDK6C1                                                     
023100     EJECT                                                                
023200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
023300 01  DLI-IO-WDK611.                                                       
023400*    03  -COPY WDK611                                                     
023500     EJECT                                                                
023600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD101'.                      
023700 01  DLI-IO-WDD101.                                                       
023800*    03  -COPY WDD101                                                     
023900     EJECT                                                                
024000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD1A'.                       
024100 01  DLI-IO-WDD1A.                                                        
024200*    03  -COPY WDD1A1                                                     
024300     EJECT                                                                
024400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
024500 01  DLI-IO-WDP311.                                                       
024600*    03  -COPY WDP311                                                     
024700     EJECT                                                                
024800 LINKAGE SECTION.                                                         
024900*01  -COPY W0009   -PRE MSG-                                              
025000     EJECT                                                                
025100*01  -COPY W0009   -PRE PRT-                                              
025200*01  -COPY W0009   -PRE ALT-                                              
025300     EJECT                                                                
025400*01  -COPY W0008   -PRE USEA-                                             
025500     05  FILLER                  PIC X.                                   
025600     EJECT                                                                
025700*01  -COPY W0008   -PRE WDK6-                                             
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026000*01  -COPY W0008   -PRE WDK6C-                                            
026100     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
026300*01  -COPY W0008   -PRE WDD1A-                                            
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026600*01  -COPY W0008   -PRE WDD1-                                             
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900*01  -COPY W0008   -PRE WDP3-                                             
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027200 PROCEDURE DIVISION  USING MSG-PCB PRT-PCB ALT-PCB USEA-PCB               
027300                           WDK6-PCB WDK6C-PCB WDD1-PCB                    
027400                           WDD1A-PCB WDP3-PCB.                            
027500 MAIN SECTION.                                                            
027600     ENTRY 'DLITCBL' USING MSG-PCB PRT-PCB ALT-PCB USEA-PCB               
027700                           WDK6-PCB WDK6C-PCB WDD1-PCB                    
027800                           WDD1A-PCB WDP3-PCB.                            
027900                                                                          
028000     PERFORM IMS-GET-MSG                                                  
028100     IF SEGMENT-FINNS                                                     
028200       PERFORM A-INIT                                                     
028300       PERFORM B-KOLLA-NYCKLAR                                            
028400       IF NYCKLAR-OK                                                      
028500          IF MFS-FIRST                                                    
028600            PERFORM C-FOERSTA-SIDA                                        
028700          ELSE                                                            
028800            IF MFS-NEXT                                                   
028900              PERFORM D-NAESTA-SIDA                                       
029000            ELSE                                                          
029100               IF MFS-PRINT                                               
029110                  IF MID-SUM-UT = 'J'                                     
029200                     PERFORM L-PRINT                                      
029210                  ELSE                                                    
029211                     MOVE 'SUMMERA INNAN UTSKRIFT'                        
029212                                TO MOD-TEMFSFEL                           
029220                  END-IF                                                  
029300               ELSE                                                       
029400                  PERFORM E-SAMMA-SIDA                                    
029500               END-IF                                                     
029600            END-IF                                                        
029700          END-IF                                                          
029800          IF EJ-HOPP                                                      
029900             PERFORM F-LAES-VISA-INFO                                     
030000          END-IF                                                          
030100       END-IF                                                             
030200                                                                          
030300       IF HOPP                                                            
030400          COMPUTE P-TO-P-LL =  LNG-P-TO-P-PREFIX +                        
030500                               LENGTH OF 6183-MID-W6I18301                
030600          MOVE LOW-VALUE      TO P-TO-P-Z1                                
030700          MOVE LOW-VALUE      TO P-TO-P-Z2                                
030800          MOVE 'W6T183 '      TO P-TO-P-TRANSKOD                          
030900          MOVE '6185'         TO P-TO-P-FROM-MID                          
031000          MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR                          
031100                                                                          
031200          PERFORM I-EDIT-MID-W6I18301                                     
031300          MOVE 6183-MID-W6I18301 TO P-TO-P-DATA                           
031400          PERFORM IMS-ISRT-ALT-PCB                                        
031500       ELSE                                                               
031600          COMPUTE MSG-KVLL = LENGTH OF MOD-W6O18501 + 4                   
031700          PERFORM IMS-INSERT-MSG                                          
031800       END-IF                                                             
031900     END-IF                                                               
032000                                                                          
032100     MOVE ZERO TO RETURN-CODE                                             
032200     GOBACK                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 A-INIT SECTION.                                                          
032600                                                                          
032700     IF MSG-DUBBLA-TRANSKODER                                             
032800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I18501                 
032900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
033000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
033100     ELSE                                                                 
033200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I18501                  
033300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
033400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
033500     END-IF                                                               
033600                                                                          
033700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
033800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
033900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
034000                                                                          
034100     MOVE LOW-VALUE TO MSG-AREA                                           
034200     MOVE 'W6O185N1' TO MFS-IDMOD                                         
034300     MOVE '6185' TO MOD-IDTRANS                                           
034400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
034500                                                                          
034510     IF MSGI-IDLAND-SPR = 'SE'                                            
034520        MOVE '0' TO MFS-KDHUVOMR                                          
034530     END-IF                                                               
034540                                                                          
034600     IF EGEN-MID OR HELP-MID                                              
034700       CONTINUE                                                           
034800     ELSE                                                                 
034900       MOVE SPACE TO MFS-KDTRTYP                                          
035000       MOVE '7' TO MFS-IDPFK                                              
035100     END-IF                                                               
035200                                                                          
035300     MOVE NEJ TO HOPP-SW                                                  
035400                 SW-RAKNA-TOT                                             
035500     MOVE LOW-VALUE  TO W-WDK6C1KY-MIN                                    
035600                        W-WDD1A1KY-MIN                                    
035700     MOVE HIGH-VALUE TO W-WDK6C1KY-MAX                                    
035800                        W-WDD1A1KY-MAX                                    
035900     .                                                                    
036000     EJECT                                                                
036100 B-KOLLA-NYCKLAR SECTION.                                                 
036200                                                                          
036300     MOVE ALL '+'            TO MSGI-WMSGINIT                             
036400     MOVE '001'              TO MSGI-KDCALL                               
036500     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
036600     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
036700     MOVE '6185'             TO MSGI-IDTRANS                              
036800     IF EGEN-MID                                                          
036900         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
037000         MOVE MID-IDLEVNR-IN TO MSGI-IDLEVNR                              
037500     END-IF                                                               
037600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
037700     MOVE MSGI-SPAR-AREA     TO SPAR-AREA                                 
037800                                                                          
037900     MOVE JA TO NYCKLAR-SW                                                
038000                                                                          
038100*    -- KONTROLL AV IDARTNR                                               
038200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
038300     IF MID-IDARTNR-IN NOT = ALL '+'                                      
038400       MOVE '7'         TO MFS-IDPFK                                      
038500       MOVE SPACE       TO MFS-KDTRTYP                                    
038600     END-IF                                                               
038700     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
038800     IF MSGI-IDARTNR NUMERIC                                              
038900       MOVE MSGI-IDARTNR TO W-SEQC-IDARTNR-EMB-MIN                        
039000                            W-SEQC-IDARTNR-EMB-MAX                        
039100     ELSE                                                                 
039200       MOVE NEJ TO NYCKLAR-SW                                             
039300     END-IF                                                               
039400                                                                          
039500*    -- KONTROLL AV IDLEVNR                                               
039600     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
039700     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
039800       MOVE '7'         TO MFS-IDPFK                                      
039900       MOVE SPACE       TO MFS-KDTRTYP                                    
040000     END-IF                                                               
040600     MOVE MSGI-IDLEVNR TO W-IDLEVNR                                       
041100                                                                          
041200*    -- KONTROLL AV TYP                                                   
041300     MOVE MFS-RENSA-FAELT TO MOD-TYP-IN                                   
041400     IF MID-TYP-IN NOT = ALL '+'                                          
041500       MOVE '7'         TO MFS-IDPFK                                      
041600       MOVE SPACE       TO MFS-KDTRTYP                                    
041700       MOVE MID-TYP-IN  TO WS-TYP                                         
041800     ELSE                                                                 
041900       MOVE MID-TYP-UT  TO WS-TYP                                         
042000     END-IF                                                               
042100     IF WS-TYP = 'J' OR 'Y' OR 'N' OR 'T'                                 
042200       CONTINUE                                                           
042300     ELSE                                                                 
042400       IF WS-TYP = SPACE OR '+'                                           
042500          MOVE 'J' TO WS-TYP                                              
042600       ELSE                                                               
042700          MOVE NEJ TO NYCKLAR-SW                                          
042800       END-IF                                                             
042900     END-IF                                                               
043000                                                                          
043100*    -- KONTROLL AV SUM                                                   
043200     MOVE MFS-RENSA-FAELT TO MOD-SUM-IN                                   
043300     IF MID-SUM-IN NOT = ALL '+'                                          
043400       MOVE '7'         TO MFS-IDPFK                                      
043500       MOVE SPACE       TO MFS-KDTRTYP                                    
043600       MOVE MID-SUM-IN  TO WS-SUM                                         
043700     ELSE                                                                 
043800       MOVE MID-SUM-UT  TO WS-SUM                                         
043900     END-IF                                                               
044000     IF WS-SUM = 'J' OR 'Y' OR 'N'                                        
044010        IF WS-SUM = 'J' OR 'Y'                                            
044020           MOVE 'J' TO WS-SUM                                             
044100        END-IF                                                            
044200     ELSE                                                                 
044300       IF WS-SUM = SPACE OR '+'                                           
044400          MOVE 'N' TO WS-SUM                                              
044500       ELSE                                                               
044600          MOVE NEJ TO NYCKLAR-SW                                          
044700       END-IF                                                             
044800     END-IF                                                               
044900                                                                          
045000     MOVE NEJ TO ARTNR-SOEK                                               
045100                 ARTNR-LEVNR-SOEK                                         
045200                                                                          
045300     IF NYCKLAR-OK                                                        
045400        IF MSGI-IDARTNR = ZERO                                            
045500           MOVE NEJ TO NYCKLAR-SW                                         
045600        ELSE                                                              
045700           IF MSGI-IDLEVNR = SPACE                                        
045800              MOVE JA TO ARTNR-SOEK                                       
045900           ELSE                                                           
046000              MOVE JA TO ARTNR-LEVNR-SOEK                                 
046100           END-IF                                                         
046200        END-IF                                                            
046300     END-IF                                                               
046400                                                                          
046500     IF EGEN-MID OR NYCKLAR-OK                                            
046600       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
046700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
046800       MOVE MSGI-IDLEVNR    TO MOD-IDLEVNR-UT                             
046992********************                                                      
047000       MOVE WS-TYP          TO MOD-TYP-UT                                 
047100       MOVE WS-SUM          TO MOD-SUM-UT                                 
047200     ELSE                                                                 
047300       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
047400                               MOD-IDLEVNR-UT                             
047500                               MOD-TYP-UT                                 
047600                               MOD-SUM-UT                                 
047700     END-IF                                                               
047800                                                                          
047900     IF NYCKLAR-FEL                                                       
048000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
048100       CALL WMEDKONV USING MED-WMEDAREA                                   
048200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
048300       PERFORM MFS-RENSA-FAELT-IN                                         
048400       PERFORM MFS-RENSA-FAELT-UT                                         
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 C-FOERSTA-SIDA SECTION.                                                  
048900                                                                          
049000     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
049100     CALL WMEDKONV USING MED-WMEDAREA                                     
049200     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
049300     PERFORM MFS-RENSA-FAELT-IN                                           
049400     MOVE JA TO SW-RAKNA-TOT                                              
049500     .                                                                    
049600     EJECT                                                                
049700 D-NAESTA-SIDA SECTION.                                                   
049800                                                                          
049900     IF SPAR-IDTRANS = '6185'                                             
050000       MOVE SPAR-IDARTNR-NEXT  TO W-SEQC-IDARTNR-MIN                      
050100       MOVE SPAR-KDEMBKEY-NEXT TO W-SEQC-KDEMBKEY-MIN                     
050200       MOVE SPAR-IDFPINST-NEXT TO W-SEQA-IDFPINST-MIN                     
050300     END-IF                                                               
050400     PERFORM MFS-RENSA-FAELT-IN                                           
050500     .                                                                    
050600     EJECT                                                                
050700 E-SAMMA-SIDA SECTION.                                                    
050800                                                                          
050900     IF SPAR-IDTRANS = '6185'                                             
051000        MOVE SPAR-IDARTNR-ENTER  TO W-SEQC-IDARTNR-MIN                    
051100        MOVE SPAR-KDEMBKEY-ENTER TO W-SEQC-KDEMBKEY-MIN                   
051200        MOVE SPAR-IDFPINST-ENTER TO W-SEQA-IDFPINST-MIN                   
051300        IF MID-IDLTERM NOT = ALL '+'                                      
051400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLTERM-ATTR                  
051500           MOVE MFS-ROER-EJ-FAELT TO MOD-IDLTERM                          
051600        END-IF                                                            
051700        IF MID-CMD(1) = ALL '+'                                           
051800        AND MID-CMD(2) = ALL '+'                                          
051900        AND MID-CMD(3) = ALL '+'                                          
052000        AND MID-CMD(4) = ALL '+'                                          
052100        AND MID-CMD(5) = ALL '+'                                          
052200        AND MID-CMD(6) = ALL '+'                                          
052300        AND MID-CMD(7) = ALL '+'                                          
052400        AND MID-CMD(8) = ALL '+'                                          
052500        AND MID-CMD(9) = ALL '+'                                          
052600        AND MID-CMD(10)= ALL '+'                                          
052700        AND MID-CMD(11)= ALL '+'                                          
052800        AND MID-CMD(12)= ALL '+'                                          
052900        AND MID-CMD(13)= ALL '+'                                          
053000            CONTINUE                                                      
053100        ELSE                                                              
053200           PERFORM EA-KOLLA-CMD                                           
053300           IF INDATA-OK                                                   
053400              MOVE JA TO HOPP-SW                                          
053500           ELSE                                                           
053600              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
053700              CALL WMEDKONV USING MED-WMEDAREA                            
053800              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
053900              PERFORM MFS-ROER-EJ-FAELT-IN                                
054000              PERFORM MFS-ROER-EJ-FAELT-UT                                
054100           END-IF                                                         
054200        END-IF                                                            
054300     ELSE                                                                 
054400        PERFORM MFS-RENSA-FAELT-IN                                        
054500     END-IF                                                               
054600     .                                                                    
054700     EJECT                                                                
054800 EA-KOLLA-CMD SECTION.                                                    
054900                                                                          
055000                                                                          
055100     MOVE SPACE TO WS-CMD                                                 
055200     MOVE +1    TO RAD-IX                                                 
055300     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
055400        IF MID-CMD(RAD-IX) NOT = ALL '+'                                  
055500           MOVE MFS-ROER-EJ-FAELT TO MOD-CMD(RAD-IX)                      
055600           IF MID-CMD(RAD-IX) = 'X' OR 'S'                                
055700              MOVE MFS-ALFA-FAELT-RAETT TO MOD-CMD-ATTR(RAD-IX)           
055800              IF WS-CMD = SPACE                                           
055900                 MOVE MID-CMD(RAD-IX) TO WS-CMD                           
056000              ELSE                                                        
056100                 MOVE MFS-ALFA-FAELT-FEL TO MOD-CMD-ATTR(RAD-IX)          
056200                 MOVE NEJ TO INDATA-SW                                    
056300              END-IF                                                      
056400           ELSE                                                           
056500              MOVE MFS-ALFA-FAELT-FEL   TO MOD-CMD-ATTR(RAD-IX)           
056600              MOVE NEJ TO INDATA-SW                                       
056700           END-IF                                                         
056800        END-IF                                                            
056900        ADD +1 TO RAD-IX                                                  
057000     END-PERFORM                                                          
057100     .                                                                    
057200     EJECT                                                                
057300 F-LAES-VISA-INFO SECTION.                                                
057400                                                                          
057500     IF WS-TYP = JA OR 'Y'                                                
057600        PERFORM FA-SOEK-FPINST                                            
057700     ELSE                                                                 
057800        IF WS-TYP = NEJ                                                   
057900           PERFORM FB-SOEK-EJ-FPINST                                      
058000        ELSE                                                              
058100           PERFORM FC-SOEK-TOT                                            
058200        END-IF                                                            
058300     END-IF                                                               
058400                                                                          
058500     IF TRAEFF-SW = NEJ                                                   
058600        MOVE URVAL-SAKNAS TO MED-IDMFSINF                                 
058700        CALL WMEDKONV USING MED-WMEDAREA                                  
058800        MOVE MED-MFSINF TO MOD-TEMFSFEL                                   
058900        PERFORM MFS-RENSA-FAELT-UT                                        
059000     ELSE                                                                 
059100                                                                          
059200        MOVE +1 TO RAD-IX                                                 
059300        PERFORM UNTIL RAD-IX > RAD-IX-MAX                                 
059400                                                                          
059500           IF TRAEFF-SW = JA                                              
059600              IF VISA-RAD = JA                                            
059700                 PERFORM S02-VISA-RAD                                     
059800              ELSE                                                        
059900                 IF VISA-RENSAD-RAD = JA                                  
060000                    PERFORM S01-RENSA-RAD                                 
060100                 END-IF                                                   
060200              END-IF                                                      
060300              MOVE SEQC-IDARTNR TO W-IDARTNR                              
060400                                   MOD-IDARTNR(RAD-IX)                    
060500              IF VISA-RAD = JA                                            
060600                 PERFORM IMS-GET-WDK611                                   
060700                 IF SEGMENT-FINNS                                         
060800                    MOVE CLAG-BEFT TO MOD-BEFT(RAD-IX)                    
060900                 END-IF                                                   
061000              ELSE                                                        
061100                 IF VISA-RENSAD-RAD = JA                                  
061200                    PERFORM IMS-GET-WDK601                                
061300                    IF SEGMENT-FINNS                                      
061400                       MOVE ART-IDLEVNR TO MOD-IDLEVNR(RAD-IX)            
061500                       PERFORM IMS-GNP-WDK611                             
061600                       IF SEGMENT-FINNS                                   
061700                          MOVE CLAG-BEFT TO MOD-BEFT(RAD-IX)              
061800                       END-IF                                             
061900                    END-IF                                                
062000                 END-IF                                                   
062100              END-IF                                                      
062200                                                                          
062300              IF WS-TYP = JA OR 'Y'                                       
062400                 PERFORM FE-SOEK-FPINST-NEXT                              
062500              ELSE                                                        
062600                 IF WS-TYP = NEJ                                          
062700                    PERFORM FD-SOEK-EJ-FPINST-NEXT                        
062800                 ELSE                                                     
062900                    PERFORM FF-SOEK-TOT-NEXT                              
063000                 END-IF                                                   
063100              END-IF                                                      
063200           ELSE                                                           
063300              MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR (RAD-IX)                
063400                                      MOD-REGTYP  (RAD-IX)                
063500                                      MOD-IDFPINST(RAD-IX)                
063600                                      MOD-TIUPPDAT(RAD-IX)                
063700                                      MOD-IDPERSON(RAD-IX)                
063800                                      MOD-IDARTNR (RAD-IX)                
063900                                      MOD-BEINIT  (RAD-IX)                
064000                                      MOD-BEFT    (RAD-IX)                
064100           END-IF                                                         
064200           ADD +1 TO RAD-IX                                               
064300       END-PERFORM                                                        
064400                                                                          
064500       IF TRAEFF-SW = JA                                                  
064600          MOVE SEQC-IDARTNR  TO SPAR-IDARTNR-NEXT                         
064700          MOVE SEQC-KDEMBKEY TO SPAR-KDEMBKEY-NEXT                        
064800          IF WS-TYP = NEJ                                                 
064900             MOVE ZERO       TO SPAR-IDFPINST-NEXT                        
065000          ELSE                                                            
065100             IF WS-TYP = 'T'                                              
065200                IF SEQA-IDFPINST NUMERIC                                  
065300                   MOVE SEQA-IDFPINST TO SPAR-IDFPINST-NEXT               
065400                ELSE                                                      
065500                   MOVE ZERO          TO SPAR-IDFPINST-NEXT               
065600                END-IF                                                    
065700             ELSE                                                         
065800                MOVE SEQA-IDFPINST    TO SPAR-IDFPINST-NEXT               
065900             END-IF                                                       
066000          END-IF                                                          
066100                                                                          
066200          MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                       
066300          CALL WMEDKONV USING MED-WMEDAREA                                
066400          MOVE MED-TEMFSINF TO MOD-TEMFSINF                               
066500       ELSE                                                               
066600         MOVE ZERO  TO SPAR-IDARTNR-NEXT                                  
066700                       SPAR-IDFPINST-NEXT                                 
066800         MOVE SPACE TO SPAR-KDEMBKEY-NEXT                                 
066900       END-IF                                                             
067000                                                                          
067100       IF WS-SUM = JA                                                     
067200         IF SW-RAKNA-TOT = JA                                             
067300            PERFORM FG-RAKNA-TOT                                          
067400         END-IF                                                           
067500         MOVE SPAR-TOT-IDFPINST TO MOD-KVANTAL-FPINST                     
067600         MOVE SPAR-TOT-IDART    TO MOD-KVANTAL-ART                        
067700       END-IF                                                             
067800                                                                          
067900       MOVE '002'      TO MSGI-KDCALL                                     
068000       MOVE '6185'     TO SPAR-IDTRANS                                    
068100       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
068200       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
068300                                                                          
068400     END-IF                                                               
068500     .                                                                    
068600     EJECT                                                                
068700 FA-SOEK-FPINST SECTION.                                                  
068800                                                                          
068900     MOVE NEJ TO VISA-RAD                                                 
069000                 VISA-RENSAD-RAD                                          
069100                 TRAEFF-SW                                                
069200                                                                          
069300     IF ARTNR-LEVNR-SOEK = JA                                             
069400        PERFORM IMS-GET-WDK6C-FIRST                                       
069500        PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                    
069600           MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                        
069700                                W-SEQA-IDARTNR-MAX                        
069800           PERFORM IMS-GET-WDD1A-FIRST                                    
069900           PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                 
070000              MOVE SEQA-IDFPINST TO W-IDFPINST                            
070100              PERFORM IMS-GET-WDD101                                      
070200              IF FPI-IDLEVNR = W-IDLEVNR                                  
070300                 MOVE JA TO TRAEFF-SW                                     
070400                 MOVE JA TO VISA-RAD                                      
070500                 MOVE SEQC-IDARTNR  TO SPAR-IDARTNR-ENTER                 
070600                 MOVE SEQC-KDEMBKEY TO SPAR-KDEMBKEY-ENTER                
070700                 MOVE SEQA-IDFPINST TO SPAR-IDFPINST-ENTER                
070800              ELSE                                                        
070900                 PERFORM IMS-GET-WDD1A-NEXT                               
071000              END-IF                                                      
071100           END-PERFORM                                                    
071200           IF TRAEFF-SW = NEJ                                             
071300              PERFORM IMS-GET-WDK6C-NEXT                                  
071400           END-IF                                                         
071500        END-PERFORM                                                       
071600     END-IF                                                               
071700                                                                          
071800     IF ARTNR-SOEK = JA                                                   
071900        PERFORM IMS-GET-WDK6C-FIRST                                       
072000        PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                    
072100           MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                        
072200                                W-SEQA-IDARTNR-MAX                        
072300           PERFORM IMS-GET-WDD1A-FIRST                                    
072400           PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                 
072500              MOVE SEQA-IDFPINST TO W-IDFPINST                            
072600              PERFORM IMS-GET-WDD101                                      
072700              IF SEGMENT-FINNS                                            
072800                 MOVE JA TO VISA-RAD                                      
072900                            TRAEFF-SW                                     
073000                 MOVE SEQC-IDARTNR  TO SPAR-IDARTNR-ENTER                 
073100                 MOVE SEQC-KDEMBKEY TO SPAR-KDEMBKEY-ENTER                
073200                 MOVE SEQA-IDFPINST TO SPAR-IDFPINST-ENTER                
073300              ELSE                                                        
073400                 PERFORM IMS-GET-WDD1A-NEXT                               
073500              END-IF                                                      
073600           END-PERFORM                                                    
073700           IF TRAEFF-SW = NEJ                                             
073800              PERFORM IMS-GET-WDK6C-NEXT                                  
073900           END-IF                                                         
074000        END-PERFORM                                                       
074100     END-IF                                                               
074200     .                                                                    
074300     EJECT                                                                
074400 FB-SOEK-EJ-FPINST SECTION.                                               
074500                                                                          
074600     MOVE NEJ TO VISA-RAD                                                 
074700                 VISA-RENSAD-RAD                                          
074800                 TRAEFF-SW                                                
074900                                                                          
075000     IF ARTNR-SOEK = JA                                                   
075100        PERFORM IMS-GET-WDK6C-FIRST                                       
075200        PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                    
075300           MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                        
075400                                W-SEQA-IDARTNR-MAX                        
075500           PERFORM IMS-GET-WDD1A-FIRST                                    
075600           IF SEGMENT-FINNS                                               
075700              MOVE NEJ TO TRAEFF-SW                                       
075800           ELSE                                                           
075900              MOVE SEQC-IDARTNR  TO SPAR-IDARTNR-ENTER                    
076000              MOVE SEQC-KDEMBKEY TO SPAR-KDEMBKEY-ENTER                   
076100              MOVE ZERO          TO SPAR-IDFPINST-ENTER                   
076200              MOVE JA TO VISA-RENSAD-RAD                                  
076300                         TRAEFF-SW                                        
076400           END-IF                                                         
076500           IF TRAEFF-SW = NEJ                                             
076600              PERFORM IMS-GET-WDK6C-NEXT                                  
076700           END-IF                                                         
076800        END-PERFORM                                                       
076900     ELSE                                                                 
077000        IF ARTNR-LEVNR-SOEK = JA                                          
077100           PERFORM IMS-GET-WDK6C-FIRST                                    
077200           PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                 
077300              MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                     
077400                                   W-SEQA-IDARTNR-MAX                     
077500              PERFORM IMS-GET-WDD1A-FIRST                                 
077600              IF SEGMENT-FINNS                                            
077700                 PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA           
077800                    MOVE SEQA-IDFPINST TO W-IDFPINST                      
077900                    PERFORM IMS-GET-WDD101                                
078000                    IF FPI-IDLEVNR = W-IDLEVNR                            
078100                       MOVE NEJ TO TRAEFF-SW                              
078200***                 ELSE                                                  
078300***                    MOVE SEQC-IDARTNR TO W-IDARTNR                     
078400***                    PERFORM IMS-GET-WDK601                             
078500***                    IF ART-IDLEVNR = W-IDLEVNR                         
078600***                       MOVE SEQC-IDARTNR                               
078700***                                 TO SPAR-IDARTNR-ENTER                 
078800***                       MOVE SEQC-KDEMBKEY                              
078900***                                 TO SPAR-KDEMBKEY-ENTER                
079000***                       MOVE ZERO TO SPAR-IDFPINST-ENTER                
079100***                       MOVE JA TO VISA-RENSAD-RAD                      
079200***                                  TRAEFF-SW                            
079300***                    END-IF                                             
079400                    END-IF                                                
079500                    IF TRAEFF-SW = NEJ                                    
079600                       PERFORM IMS-GET-WDD1A-NEXT                         
079700                    END-IF                                                
079800                 END-PERFORM                                              
079900              ELSE                                                        
080000                 MOVE SEQC-IDARTNR TO W-IDARTNR                           
080100                 PERFORM IMS-GET-WDK601                                   
080200                 IF SEGMENT-FINNS                                         
080300                    IF ART-IDLEVNR = W-IDLEVNR                            
080400                       MOVE SEQC-IDARTNR                                  
080500                                 TO SPAR-IDARTNR-ENTER                    
080600                       MOVE SEQC-KDEMBKEY                                 
080700                                 TO SPAR-KDEMBKEY-ENTER                   
080800                       MOVE ZERO TO SPAR-IDFPINST-ENTER                   
080900                       MOVE JA TO VISA-RENSAD-RAD                         
081000                                  TRAEFF-SW                               
081100                       MOVE JA TO VISA-RENSAD-RAD                         
081200                    END-IF                                                
081300                 END-IF                                                   
081400              END-IF                                                      
081500              IF TRAEFF-SW = NEJ                                          
081600                 PERFORM IMS-GET-WDK6C-NEXT                               
081700              END-IF                                                      
081800           END-PERFORM                                                    
081900        END-IF                                                            
082000     END-IF                                                               
082100     .                                                                    
082200     EJECT                                                                
082300 FC-SOEK-TOT SECTION.                                                     
082400                                                                          
082500     MOVE NEJ TO VISA-RAD                                                 
082600                 VISA-RENSAD-RAD                                          
082700                 TRAEFF-SW                                                
082800                                                                          
082900     PERFORM IMS-GET-WDK6C-FIRST                                          
083000     IF ARTNR-SOEK = JA                                                   
083100        IF SEGMENT-FINNS                                                  
083200           MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                        
083300                                W-SEQA-IDARTNR-MAX                        
083400           PERFORM IMS-GET-WDD1A-FIRST                                    
083500           IF SEGMENT-FINNS                                               
083600              MOVE SEQA-IDFPINST TO W-IDFPINST                            
083700              PERFORM IMS-GET-WDD101                                      
083800              MOVE SEQC-IDARTNR  TO SPAR-IDARTNR-ENTER                    
083900              MOVE SEQC-KDEMBKEY TO SPAR-KDEMBKEY-ENTER                   
084000              MOVE SEQA-IDFPINST TO SPAR-IDFPINST-ENTER                   
084100              MOVE JA TO VISA-RAD                                         
084200                         TRAEFF-SW                                        
084300           ELSE                                                           
084400              MOVE JA TO VISA-RENSAD-RAD                                  
084500                         TRAEFF-SW                                        
084600              MOVE SEQC-IDARTNR  TO SPAR-IDARTNR-ENTER                    
084700              MOVE SEQC-KDEMBKEY TO SPAR-KDEMBKEY-ENTER                   
084800              MOVE ZERO          TO SPAR-IDFPINST-ENTER                   
084900           END-IF                                                         
085000        END-IF                                                            
085100     ELSE                                                                 
085200        IF ARTNR-LEVNR-SOEK = JA                                          
085300           PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                 
085400              MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                     
085500                                   W-SEQA-IDARTNR-MAX                     
085600              PERFORM IMS-GET-WDD1A-FIRST                                 
085700              IF SEGMENT-FINNS                                            
085800                 PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA           
085900                    MOVE SEQA-IDFPINST TO W-IDFPINST                      
086000                    PERFORM IMS-GET-WDD101                                
086100                    IF FPI-IDLEVNR = W-IDLEVNR                            
086200                       MOVE JA TO TRAEFF-SW                               
086300                       MOVE JA TO VISA-RAD                                
086400                       MOVE SEQC-IDARTNR  TO SPAR-IDARTNR-ENTER           
086500                       MOVE SEQC-KDEMBKEY TO SPAR-KDEMBKEY-ENTER          
086600                       MOVE SEQA-IDFPINST TO SPAR-IDFPINST-ENTER          
086700*****               ELSE                                                  
086800*****                  MOVE SEQC-IDARTNR TO W-IDARTNR                     
086900*****                  PERFORM IMS-GET-WDK601                             
087000*****                  IF ART-IDLEVNR = W-IDLEVNR                         
087100*****                     MOVE SEQC-IDARTNR                               
087200*****                           TO SPAR-IDARTNR-ENTER                     
087300*****                     MOVE SEQC-KDEMBKEY                              
087400*****                           TO SPAR-KDEMBKEY-ENTER                    
087500*****                     MOVE ZERO TO SPAR-IDFPINST-ENTER                
087600*****                     MOVE JA TO VISA-RENSAD-RAD                      
087700*****                                TRAEFF-SW                            
087800*****                  END-IF                                             
087900                    END-IF                                                
088000                    IF TRAEFF-SW = NEJ                                    
088100                       PERFORM IMS-GET-WDD1A-NEXT                         
088200                    END-IF                                                
088300                 END-PERFORM                                              
088400              ELSE                                                        
088500                 MOVE SEQC-IDARTNR TO W-IDARTNR                           
088600                 PERFORM IMS-GET-WDK601                                   
088700                 IF SEGMENT-FINNS                                         
088800                    IF ART-IDLEVNR = W-IDLEVNR                            
088900                       MOVE SEQC-IDARTNR  TO SPAR-IDARTNR-ENTER           
089000                       MOVE SEQC-KDEMBKEY TO SPAR-KDEMBKEY-ENTER          
089100                       MOVE ZERO          TO SPAR-IDFPINST-ENTER          
089200                       MOVE JA TO VISA-RENSAD-RAD                         
089300                                  TRAEFF-SW                               
089400                    END-IF                                                
089500                 END-IF                                                   
089600              END-IF                                                      
089700              IF TRAEFF-SW = NEJ                                          
089800                 PERFORM IMS-GET-WDK6C-NEXT                               
089900              END-IF                                                      
090000           END-PERFORM                                                    
090100        END-IF                                                            
090200     END-IF                                                               
090300     .                                                                    
090400     EJECT                                                                
090500 FD-SOEK-EJ-FPINST-NEXT SECTION.                                          
090600                                                                          
090700     MOVE NEJ TO VISA-RAD                                                 
090800                 VISA-RENSAD-RAD                                          
090900                 TRAEFF-SW                                                
091000                                                                          
091100     IF ARTNR-SOEK = JA                                                   
091200        PERFORM IMS-GET-WDK6C-NEXT                                        
091300        PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                    
091400           MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                        
091500                                W-SEQA-IDARTNR-MAX                        
091600           PERFORM IMS-GET-WDD1A-FIRST                                    
091700           IF SEGMENT-FINNS                                               
091800              MOVE NEJ TO TRAEFF-SW                                       
091900           ELSE                                                           
092000              MOVE JA TO VISA-RENSAD-RAD                                  
092100                         TRAEFF-SW                                        
092200           END-IF                                                         
092300           IF TRAEFF-SW = NEJ                                             
092400              MOVE ZERO TO W-SEQA-IDFPINST-MIN                            
092500              PERFORM IMS-GET-WDK6C-NEXT                                  
092600           END-IF                                                         
092700        END-PERFORM                                                       
092800     ELSE                                                                 
092900        IF ARTNR-LEVNR-SOEK = JA                                          
093000           PERFORM IMS-GET-WDK6C-NEXT                                     
093100           PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                 
093200              MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                     
093300                                   W-SEQA-IDARTNR-MAX                     
093400              PERFORM IMS-GET-WDD1A-FIRST                                 
093500              IF SEGMENT-FINNS                                            
093600                 MOVE SEQA-IDFPINST TO W-IDFPINST                         
093700                 PERFORM IMS-GET-WDD101                                   
093800                 IF FPI-IDLEVNR = W-IDLEVNR                               
093900                    MOVE NEJ TO TRAEFF-SW                                 
094000                 ELSE                                                     
094100                    MOVE SEQC-IDARTNR TO W-IDARTNR                        
094200                    PERFORM IMS-GET-WDK601                                
094300                    IF ART-IDLEVNR = W-IDLEVNR                            
094400                       MOVE JA TO VISA-RENSAD-RAD                         
094500                                  TRAEFF-SW                               
094600                    ELSE                                                  
094700                       MOVE NEJ TO TRAEFF-SW                              
094800                    END-IF                                                
094900                 END-IF                                                   
095000              ELSE                                                        
095100                 MOVE SEQC-IDARTNR TO W-IDARTNR                           
095200                 PERFORM IMS-GET-WDK601                                   
095300                 IF ART-IDLEVNR = W-IDLEVNR                               
095400                    MOVE JA TO VISA-RENSAD-RAD                            
095500                               TRAEFF-SW                                  
095600                 ELSE                                                     
095700                    MOVE NEJ TO TRAEFF-SW                                 
095800                 END-IF                                                   
095900              END-IF                                                      
096000              IF TRAEFF-SW = NEJ                                          
096100                 MOVE ZERO TO W-SEQA-IDFPINST-MIN                         
096200                 PERFORM IMS-GET-WDK6C-NEXT                               
096300              END-IF                                                      
096400           END-PERFORM                                                    
096500        END-IF                                                            
096600     END-IF                                                               
096700     .                                                                    
096800     EJECT                                                                
096900 FE-SOEK-FPINST-NEXT SECTION.                                             
097000                                                                          
097100     MOVE NEJ TO VISA-RAD                                                 
097200                 VISA-RENSAD-RAD                                          
097300                 TRAEFF-SW                                                
097400                                                                          
097500     IF ARTNR-LEVNR-SOEK = JA                                             
097600                                                                          
097700        PERFORM IMS-GET-WDD1A-NEXT                                        
097800        PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                    
097900           MOVE SEQA-IDFPINST TO W-IDFPINST                               
098000           PERFORM IMS-GET-WDD101                                         
098100           IF FPI-IDLEVNR = W-IDLEVNR                                     
098200              MOVE JA TO TRAEFF-SW                                        
098300              MOVE JA TO VISA-RAD                                         
098400           ELSE                                                           
098500              PERFORM IMS-GET-WDD1A-NEXT                                  
098600           END-IF                                                         
098700        END-PERFORM                                                       
098800                                                                          
098900        IF SEGMENT-SAKNAS OR TRAEFF-SW = NEJ                              
099000           MOVE NEJ TO TRAEFF-SW                                          
099100           MOVE ZERO TO W-SEQA-IDFPINST-MIN                               
099200           PERFORM IMS-GET-WDK6C-NEXT                                     
099300           PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                 
099400              MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                     
099500                                   W-SEQA-IDARTNR-MAX                     
099600              PERFORM IMS-GET-WDD1A-FIRST                                 
099700              PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA              
099800                 MOVE SEQA-IDFPINST TO W-IDFPINST                         
099900                 PERFORM IMS-GET-WDD101                                   
100000                 IF FPI-IDLEVNR = W-IDLEVNR                               
100100                    MOVE JA TO TRAEFF-SW                                  
100200                    MOVE JA TO VISA-RAD                                   
100300                 ELSE                                                     
100400                    PERFORM IMS-GET-WDD1A-NEXT                            
100500                 END-IF                                                   
100600              END-PERFORM                                                 
100700              IF TRAEFF-SW = NEJ                                          
100800                 MOVE ZERO TO W-SEQA-IDFPINST-MIN                         
100900                 PERFORM IMS-GET-WDK6C-NEXT                               
101000              END-IF                                                      
101100          END-PERFORM                                                     
101200       END-IF                                                             
101300     END-IF                                                               
101400                                                                          
101500     IF ARTNR-SOEK = JA                                                   
101600                                                                          
101700        PERFORM IMS-GET-WDD1A-NEXT                                        
101800        PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                    
101900           MOVE SEQA-IDFPINST TO W-IDFPINST                               
102000           PERFORM IMS-GET-WDD101                                         
102100           IF SEGMENT-FINNS                                               
102200              MOVE JA TO TRAEFF-SW                                        
102300                         VISA-RAD                                         
102400           ELSE                                                           
102500              PERFORM IMS-GET-WDD1A-NEXT                                  
102600           END-IF                                                         
102700        END-PERFORM                                                       
102800                                                                          
102900        IF SEGMENT-SAKNAS OR TRAEFF-SW = NEJ                              
103000           MOVE NEJ TO TRAEFF-SW                                          
103100           MOVE ZERO TO W-SEQA-IDFPINST-MIN                               
103200           PERFORM IMS-GET-WDK6C-NEXT                                     
103300           PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                 
103400              MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                     
103500                                   W-SEQA-IDARTNR-MAX                     
103600              PERFORM IMS-GET-WDD1A-FIRST                                 
103700              PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA              
103800                 MOVE SEQA-IDFPINST TO W-IDFPINST                         
103900                 PERFORM IMS-GET-WDD101                                   
104000                 IF SEGMENT-FINNS                                         
104100                    MOVE JA TO VISA-RAD                                   
104200                               TRAEFF-SW                                  
104300                 ELSE                                                     
104400                    PERFORM IMS-GET-WDD1A-NEXT                            
104500                 END-IF                                                   
104600              END-PERFORM                                                 
104700              IF TRAEFF-SW = NEJ                                          
104800                 MOVE ZERO TO W-SEQA-IDFPINST-MIN                         
104900                 PERFORM IMS-GET-WDK6C-NEXT                               
105000              END-IF                                                      
105100           END-PERFORM                                                    
105200        END-IF                                                            
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600 FF-SOEK-TOT-NEXT SECTION.                                                
105700                                                                          
105800     MOVE NEJ TO VISA-RAD                                                 
105900                 VISA-RENSAD-RAD                                          
106000                 TRAEFF-SW                                                
106100                                                                          
106200     IF ARTNR-SOEK = JA                                                   
106300        PERFORM IMS-GET-WDD1A-NEXT                                        
106400        IF SEGMENT-FINNS                                                  
106500           MOVE SEQA-IDFPINST TO W-IDFPINST                               
106600           PERFORM IMS-GET-WDD101                                         
106700           IF SEGMENT-FINNS                                               
106800              MOVE JA TO VISA-RAD                                         
106900                         TRAEFF-SW                                        
107000           END-IF                                                         
107100        END-IF                                                            
107200                                                                          
107300                                                                          
107400       IF TRAEFF-SW = NEJ                                                 
107500          MOVE ZERO TO W-SEQA-IDFPINST-MIN                                
107600          PERFORM IMS-GET-WDK6C-NEXT                                      
107700          IF SEGMENT-FINNS                                                
107800             MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                      
107900                                  W-SEQA-IDARTNR-MAX                      
108000             PERFORM IMS-GET-WDD1A-FIRST                                  
108100             IF SEGMENT-FINNS                                             
108200                MOVE SEQA-IDFPINST TO W-IDFPINST                          
108300                PERFORM IMS-GET-WDD101                                    
108400                MOVE JA TO VISA-RAD                                       
108500                           TRAEFF-SW                                      
108600             ELSE                                                         
108700                MOVE JA TO VISA-RENSAD-RAD                                
108800                           TRAEFF-SW                                      
108900             END-IF                                                       
109000          END-IF                                                          
109100       END-IF                                                             
109200                                                                          
109300     ELSE                                                                 
109400                                                                          
109500        IF ARTNR-LEVNR-SOEK = JA                                          
109600                                                                          
109700           PERFORM IMS-GET-WDD1A-NEXT                                     
109800           PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA                 
109900              MOVE SEQA-IDFPINST TO W-IDFPINST                            
110000              PERFORM IMS-GET-WDD101                                      
110100              IF SEGMENT-FINNS                                            
110200                 IF FPI-IDLEVNR = W-IDLEVNR                               
110300                   MOVE JA TO VISA-RAD                                    
110400                              TRAEFF-SW                                   
110500                 ELSE                                                     
110600                    MOVE SEQA-IDARTNR TO W-IDARTNR                        
110700                    PERFORM IMS-GET-WDK601                                
110800                    IF ART-IDLEVNR = W-IDLEVNR                            
110900                       MOVE JA TO VISA-RENSAD-RAD                         
111000                                  TRAEFF-SW                               
111100                    END-IF                                                
111200                 END-IF                                                   
111300              END-IF                                                      
111400              IF TRAEFF-SW = NEJ                                          
111500                 PERFORM IMS-GET-WDD1A-NEXT                               
111600              END-IF                                                      
111700           END-PERFORM                                                    
111800                                                                          
111900           IF TRAEFF-SW = NEJ                                             
112000              MOVE ZERO TO W-SEQA-IDFPINST-MIN                            
112100              PERFORM IMS-GET-WDK6C-NEXT                                  
112200              PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF-SW = JA              
112300                 MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                  
112400                                      W-SEQA-IDARTNR-MAX                  
112500                 PERFORM IMS-GET-WDD1A-FIRST                              
112600                 IF SEGMENT-FINNS                                         
112700                    PERFORM UNTIL SEGMENT-SAKNAS OR                       
112800                       TRAEFF-SW = JA                                     
112900                       MOVE SEQA-IDFPINST TO W-IDFPINST                   
113000                       PERFORM IMS-GET-WDD101                             
113100                       IF FPI-IDLEVNR = W-IDLEVNR                         
113200                          MOVE JA TO VISA-RAD                             
113300                                     TRAEFF-SW                            
113400                       ELSE                                               
113500                          MOVE SEQC-IDARTNR TO W-IDARTNR                  
113600                          PERFORM IMS-GET-WDK601                          
113700                          IF ART-IDLEVNR = W-IDLEVNR                      
113800                             MOVE JA TO VISA-RENSAD-RAD                   
113900                                        TRAEFF-SW                         
114000                          END-IF                                          
114100                       END-IF                                             
114200                       IF TRAEFF-SW = NEJ                                 
114300                          PERFORM IMS-GET-WDD1A-NEXT                      
114400                       END-IF                                             
114500                    END-PERFORM                                           
114600                 ELSE                                                     
114700                    MOVE SEQC-IDARTNR TO W-IDARTNR                        
114800                    PERFORM IMS-GET-WDK601                                
114900                    IF ART-IDLEVNR = W-IDLEVNR                            
115000                       MOVE JA TO VISA-RENSAD-RAD                         
115100                                  TRAEFF-SW                               
115200                    END-IF                                                
115300                END-IF                                                    
115400                IF TRAEFF-SW = NEJ                                        
115500                   PERFORM IMS-GET-WDK6C-NEXT                             
115600                END-IF                                                    
115700              END-PERFORM                                                 
115800           END-IF                                                         
115900        END-IF                                                            
116000     END-IF                                                               
116100     .                                                                    
116200     EJECT                                                                
116300 FG-RAKNA-TOT SECTION.                                                    
116400                                                                          
116500     MOVE LOW-VALUE    TO W-WDK6C1KY-MIN                                  
116600                          W-WDD1A1KY-MIN                                  
116700     MOVE HIGH-VALUE   TO W-WDK6C1KY-MAX                                  
116800                          W-WDD1A1KY-MAX                                  
116900     MOVE MSGI-IDARTNR TO W-SEQC-IDARTNR-EMB-MIN                          
117000                          W-SEQC-IDARTNR-EMB-MAX                          
117100     MOVE MSGI-IDLEVNR TO W-IDLEVNR                                       
117200     MOVE ZERO         TO WS-TOT-ART-MED                                  
117300                          WS-TOT-ART-UTAN                                 
117400                          WS-FINNS-IDFPINST                               
117500                          WS-SAKNAR-IDFPINST                              
117600                                                                          
117700     MOVE +1 TO TAB-IX                                                    
117800     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
117900        MOVE ZERO TO TAB-IDFPINST(TAB-IX)                                 
118000        ADD +1 TO TAB-IX                                                  
118100     END-PERFORM                                                          
118200                                                                          
118300     MOVE +1 TO TAB-IX                                                    
118400     MOVE ZERO TO WS-IDARTNR-SPAR                                         
118500     IF ARTNR-SOEK = JA                                                   
118600        PERFORM IMS-GET-WDK6C-FIRST                                       
118700        PERFORM UNTIL SEGMENT-SAKNAS                                      
118800           MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                        
118900                                W-SEQA-IDARTNR-MAX                        
119000           PERFORM IMS-GET-WDD1A-FIRST                                    
119100           IF SEGMENT-FINNS                                               
119200              IF W-SEQA-IDARTNR-MIN = WS-IDARTNR-SPAR                     
119300                 CONTINUE                                                 
119400              ELSE                                                        
119500                 ADD +1 TO WS-TOT-ART-MED                                 
119600                 MOVE W-SEQA-IDARTNR-MIN TO WS-IDARTNR-SPAR               
119700              END-IF                                                      
119800           ELSE                                                           
119900              IF W-SEQA-IDARTNR-MIN = WS-IDARTNR-SPAR                     
120000                 CONTINUE                                                 
120100              ELSE                                                        
120200                 ADD +1 TO WS-TOT-ART-UTAN                                
120300                 MOVE W-SEQA-IDARTNR-MIN TO WS-IDARTNR-SPAR               
120400              END-IF                                                      
120500           END-IF                                                         
120600           PERFORM UNTIL SEGMENT-SAKNAS                                   
120700              MOVE SEQA-IDFPINST TO W-IDFPINST                            
120800              PERFORM IMS-GET-WDD101                                      
120900              IF SEGMENT-FINNS                                            
121000                 MOVE SEQA-IDFPINST TO TAB-IDFPINST(TAB-IX)               
121100                 ADD +1 TO TAB-IX                                         
121200              END-IF                                                      
121300              PERFORM IMS-GET-WDD1A-NEXT                                  
121400           END-PERFORM                                                    
121500           IF SEGMENT-SAKNAS                                              
121600              MOVE ZERO  TO W-SEQA-IDFPINST-MIN                           
121700              MOVE SPACE TO W-SEQC-KDEMBKEY-MIN                           
121800              PERFORM IMS-GET-WDK6C-NEXT                                  
121900           END-IF                                                         
122000        END-PERFORM                                                       
122100        IF WS-TYP = NEJ                                                   
122200           MOVE WS-TOT-ART-UTAN       TO SPAR-TOT-IDART                   
122300           MOVE ZERO                  TO SPAR-TOT-IDFPINST                
122400        END-IF                                                            
122500        IF WS-TYP = JA                                                    
122600           PERFORM FGA-RAKNA-IDFPINST                                     
122700           MOVE WS-TOT-ART-MED        TO SPAR-TOT-IDART                   
122800           MOVE WS-FINNS-IDFPINST     TO SPAR-TOT-IDFPINST                
122900        END-IF                                                            
123000        IF WS-TYP = 'T'                                                   
123100           PERFORM FGA-RAKNA-IDFPINST                                     
123200           COMPUTE SPAR-TOT-IDART = WS-TOT-ART-UTAN +                     
123300                               WS-TOT-ART-MED                             
123400           MOVE WS-FINNS-IDFPINST TO SPAR-TOT-IDFPINST                    
123500        END-IF                                                            
123600     END-IF                                                               
123700                                                                          
123800     MOVE ZERO TO WS-IDARTNR-SPAR                                         
123900     IF ARTNR-LEVNR-SOEK = JA                                             
124000        PERFORM IMS-GET-WDK6C-FIRST                                       
124100        PERFORM UNTIL SEGMENT-SAKNAS                                      
124200           MOVE SEQC-IDARTNR TO W-SEQA-IDARTNR-MIN                        
124300                                W-SEQA-IDARTNR-MAX                        
124400           PERFORM IMS-GET-WDD1A-FIRST                                    
124500           IF SEGMENT-FINNS                                               
124600              PERFORM UNTIL SEGMENT-SAKNAS                                
124700                 MOVE SEQA-IDFPINST TO W-IDFPINST                         
124800                 PERFORM IMS-GET-WDD101                                   
124900                 IF FPI-IDLEVNR = W-IDLEVNR                               
125000                    MOVE SEQA-IDFPINST TO TAB-IDFPINST(TAB-IX)            
125100                    ADD +1 TO TAB-IX                                      
125200                    IF W-SEQA-IDARTNR-MIN = WS-IDARTNR-SPAR               
125300                       CONTINUE                                           
125400                    ELSE                                                  
125500                       ADD +1 TO WS-TOT-ART-MED                           
125600                       MOVE W-SEQA-IDARTNR-MIN TO WS-IDARTNR-SPAR         
125700                    END-IF                                                
125800                 ELSE                                                     
125900                    IF W-SEQA-IDARTNR-MIN = WS-IDARTNR-SPAR               
126000                       CONTINUE                                           
126100                    ELSE                                                  
126200                       MOVE SEQC-IDARTNR TO W-IDARTNR                     
126300                       PERFORM IMS-GET-WDK601                             
126400                       IF ART-IDLEVNR = W-IDLEVNR                         
126500                          IF W-SEQA-IDARTNR-MIN = WS-IDARTNR-SPAR         
126600                             CONTINUE                                     
126700                          ELSE                                            
126800                             ADD +1 TO WS-TOT-ART-UTAN                    
126900                             MOVE W-SEQA-IDARTNR-MIN                      
127000                                           TO WS-IDARTNR-SPAR             
127100                          END-IF                                          
127200                       END-IF                                             
127300                    END-IF                                                
127400                 END-IF                                                   
127500                 PERFORM IMS-GET-WDD1A-NEXT                               
127600              END-PERFORM                                                 
127700           ELSE                                                           
127800              MOVE SEQC-IDARTNR TO W-IDARTNR                              
127900              PERFORM IMS-GET-WDK601                                      
128000              IF ART-IDLEVNR = W-IDLEVNR                                  
128100                 IF W-IDARTNR = WS-IDARTNR-SPAR                           
128200                    CONTINUE                                              
128300                 ELSE                                                     
128400                    ADD +1 TO WS-TOT-ART-UTAN                             
128500                    MOVE SEQC-IDARTNR TO WS-IDARTNR-SPAR                  
128600                 END-IF                                                   
128700              END-IF                                                      
128800              MOVE 'GE' TO STATUS-WS                                      
128900           END-IF                                                         
129000           IF SEGMENT-SAKNAS                                              
129100              MOVE ZERO  TO W-SEQA-IDFPINST-MIN                           
129200              MOVE SPACE TO W-SEQC-KDEMBKEY-MIN                           
129300              PERFORM IMS-GET-WDK6C-NEXT                                  
129400           END-IF                                                         
129500        END-PERFORM                                                       
129600        IF WS-TYP = NEJ                                                   
129700           MOVE WS-TOT-ART-UTAN       TO SPAR-TOT-IDART                   
129800           MOVE ZERO                  TO SPAR-TOT-IDFPINST                
129900        END-IF                                                            
130000        IF WS-TYP = JA                                                    
130100           PERFORM FGA-RAKNA-IDFPINST                                     
130200           MOVE WS-TOT-ART-MED        TO SPAR-TOT-IDART                   
130300           MOVE WS-FINNS-IDFPINST     TO SPAR-TOT-IDFPINST                
130400        END-IF                                                            
130500        IF WS-TYP = 'T'                                                   
130600           PERFORM FGA-RAKNA-IDFPINST                                     
130700           COMPUTE SPAR-TOT-IDART = WS-TOT-ART-UTAN +                     
130800                               WS-TOT-ART-MED                             
130900           MOVE WS-FINNS-IDFPINST TO SPAR-TOT-IDFPINST                    
131000        END-IF                                                            
131100     END-IF                                                               
131200     .                                                                    
131300     EJECT                                                                
131400 FGA-RAKNA-IDFPINST SECTION.                                              
131500                                                                          
131600     MOVE +4     TO STEGLAANGD                                            
131700     MOVE TAB-IX TO ANTAL                                                 
131800     MOVE +4     TO NYCKELLAANGD                                          
131900                                                                          
132000     CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                      
132100     TAB-SORT-BEGREPP(1) NYCKELLAANGD                                     
132200                                                                          
132300     MOVE ZERO TO SPAR-TAB-IDFPINST                                       
132400     MOVE +1 TO TAB-IX                                                    
132500     PERFORM UNTIL TAB-IX > TAB-IX-MAX                                    
132600        IF TAB-IDFPINST(TAB-IX) = SPAR-TAB-IDFPINST                       
132700           CONTINUE                                                       
132800        ELSE                                                              
132900           IF TAB-IDFPINST(TAB-IX) NOT = ZERO                             
133000              ADD +1 TO WS-FINNS-IDFPINST                                 
133100              MOVE TAB-IDFPINST(TAB-IX) TO SPAR-TAB-IDFPINST              
133200           END-IF                                                         
133300        END-IF                                                            
133400        ADD +1 TO TAB-IX                                                  
133500     END-PERFORM                                                          
133600     .                                                                    
133700     EJECT                                                                
133800 I-EDIT-MID-W6I18301 SECTION.                                             
133900                                                                          
134000     MOVE LOW-VALUE TO 6183-MID-W6I18301                                  
134100     MOVE SPACE     TO 6183-MID-IDLEVNR-IN                                
134200     MOVE ZERO      TO 6183-MID-IDARTNR-IN                                
134300                                                                          
134400     MOVE +1 TO RAD-IX                                                    
134500     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
134600        IF MID-CMD(RAD-IX) = 'S' OR 'X'                                   
134700           INSPECT MID-IDFPINST(RAD-IX)                                   
134800                       REPLACING LEADING SPACE BY ZERO                    
134900           MOVE MID-IDFPINST(RAD-IX) TO 6183-MID-IDFPINST-IN              
135000        END-IF                                                            
135100        ADD +1 TO RAD-IX                                                  
135200     END-PERFORM                                                          
135300     .                                                                    
135400     EJECT                                                                
135500 L-PRINT SECTION.                                                         
135700*    -- CHECK OF IDLTERM (DVS. SKRIVAREN !)                               
135800     MOVE MID-IDLTERM       TO PRT-IDLTERM                                
135900     MOVE 002               TO PRT-KDCALL                                 
136000     CALL W006PRT USING PRT-W006PRT                                       
136100     IF PRT-KDSVAR = 'R'                                                  
136200        PERFORM L1-SKICKA-BMP-ORDER                                       
136710     ELSE                                                                 
136800*       FELTEXT SKALL LÄGGAS UT !                                         
136900        MOVE ERR-WRONG-PRINTER    TO MED-IDMFSFEL                         
137000        CALL WMEDKONV USING MED-WMEDAREA                                  
137100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
137200     END-IF                                                               
137400     .                                                                    
137500     EJECT                                                                
137600 L1-SKICKA-BMP-ORDER SECTION.                                             
137800     MOVE PRT-IDLTERM        TO STYR-IDPRINTER                            
137900     MOVE MSGI-IDLEVNR       TO STYR-6185-IDLEVNR                         
138000     MOVE MSGI-IDARTNR       TO STYR-6185-IDARTNR-EMB                     
138100     MOVE WS-TYP             TO STYR-6185-TYP                             
138200     MOVE '6185'   TO MSGSOP-IDTRANS                                      
138300     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
138400     MOVE 'W613S1' TO MSGSOP-IDPROCESS                                    
138500     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
138700     STRING 'STYRDATA(' WS-STYRDATA ')PRT('                               
138800            WS-PRINTER ')'                                                
138900            DELIMITED BY SIZE   INTO MSGSOP-TESYMBV                       
139100     PERFORM IMS-INSERT-PRT-MSG                                           
139101*    IF ????????                                                          
139110*       MSGTEXT LÄGGS UT !                                                
139200        MOVE INF-PRINTING-STARTED TO MED-IDMFSFEL                         
139300        CALL WMEDKONV USING MED-WMEDAREA                                  
139400        MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                           
139401*    ELSE                                                                 
139402*       FELTEXT SKALL LÄGGAS UT !                                         
139410*    END-IF                                                               
139500     .                                                                    
139600     EJECT                                                                
139700 S01-RENSA-RAD SECTION.                                                   
139800                                                                          
139900     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR (RAD-IX)                         
140000                             MOD-REGTYP  (RAD-IX)                         
140100                             MOD-IDFPINST(RAD-IX)                         
140200                             MOD-TIUPPDAT(RAD-IX)                         
140300                             MOD-IDPERSON(RAD-IX)                         
140400                             MOD-BEINIT  (RAD-IX)                         
140500     .                                                                    
140600     EJECT                                                                
140700 S02-VISA-RAD SECTION.                                                    
140800                                                                          
140900     MOVE FPI-IDLEVNR      TO MOD-IDLEVNR  (RAD-IX)                       
141000     IF FPI-IDLEVNR = SPACE                                               
141100        MOVE 'UNIK'        TO MOD-REGTYP   (RAD-IX)                       
141200     ELSE                                                                 
141300        MOVE 'ALLM'        TO MOD-REGTYP   (RAD-IX)                       
141400     END-IF                                                               
141500     MOVE FPI-IDFPINST     TO MOD-IDFPINST (RAD-IX)                       
141600     MOVE FPI-TIUPPDAT     TO MOD-TIUPPDAT (RAD-IX)                       
141700     MOVE FPI-IDUSER       TO WS-IDPERSON                                 
141800     MOVE WS-IDPERSON      TO MOD-IDPERSON (RAD-IX)                       
141900                                                                          
142000     MOVE WS-IDPERSON TO W-IDPERSON                                       
142100     PERFORM IMS-GET-WDP311                                               
142200     IF SEGMENT-FINNS                                                     
142300        MOVE PERS-BEINIT   TO MOD-BEINIT(RAD-IX)                          
142400     END-IF                                                               
142500     .                                                                    
142600     EJECT                                                                
142700 MFS-RENSA-FAELT-UT SECTION.                                              
142800                                                                          
142900     MOVE MFS-RENSA-FAELT TO MOD-KVANTAL-ART                              
143000                             MOD-KVANTAL-FPINST                           
143100     PERFORM MFS-RENSA-RAD-FAELT-UT                                       
143200     .                                                                    
143300     SKIP3                                                                
143400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
143500                                                                          
143600     MOVE +1 TO RAD-IX                                                    
143700     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
143800        MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR       (RAD-IX)                
143900                                MOD-IDARTNR       (RAD-IX)                
144000                                MOD-BEFT          (RAD-IX)                
144100                                MOD-REGTYP        (RAD-IX)                
144200                                MOD-IDFPINST      (RAD-IX)                
144300                                MOD-TIUPPDAT      (RAD-IX)                
144400                                MOD-IDPERSON      (RAD-IX)                
144500                                MOD-BEINIT        (RAD-IX)                
144600        ADD +1 TO RAD-IX                                                  
144700     END-PERFORM                                                          
144800     .                                                                    
144900     SKIP3                                                                
145000 MFS-RENSA-FAELT-IN SECTION.                                              
145100                                                                          
145200     MOVE MFS-RENSA-FAELT TO MOD-IDLTERM                                  
145300     MOVE +1 TO RAD-IX                                                    
145400     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
145500        MOVE MFS-RENSA-FAELT TO MOD-CMD(RAD-IX)                           
145600        ADD +1 TO RAD-IX                                                  
145700     END-PERFORM                                                          
145800     .                                                                    
145900     EJECT                                                                
146000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
146100                                                                          
146200     MOVE MFS-ROER-EJ-FAELT TO MOD-KVANTAL-ART                            
146300                               MOD-KVANTAL-FPINST                         
146400     PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                     
146500     .                                                                    
146600     SKIP2                                                                
146700 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
146800                                                                          
146900     MOVE +1 TO RAD-IX                                                    
147000     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
147100        MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR       (RAD-IX)              
147200                                  MOD-IDARTNR       (RAD-IX)              
147300                                  MOD-BEFT          (RAD-IX)              
147400                                  MOD-REGTYP        (RAD-IX)              
147500                                  MOD-IDFPINST      (RAD-IX)              
147600                                  MOD-TIUPPDAT      (RAD-IX)              
147700                                  MOD-IDPERSON      (RAD-IX)              
147800                                  MOD-BEINIT        (RAD-IX)              
147900        ADD +1 TO RAD-IX                                                  
148000     END-PERFORM                                                          
148100     .                                                                    
148200     EJECT                                                                
148300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
148400                                                                          
148500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDLTERM                                
148600     MOVE +1 TO RAD-IX                                                    
148700     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
148800        MOVE MFS-ROER-EJ-FAELT TO MOD-CMD(RAD-IX)                         
148900        ADD +1 TO RAD-IX                                                  
149000     END-PERFORM                                                          
149100     .                                                                    
149200     EJECT                                                                
149300* --- IMS SEKTIONER ---                                                   
149400     SKIP3                                                                
149500 IMS-GET-MSG SECTION.                                                     
149600                                                                          
149700     MOVE '  QC' TO GODK-STATUSKODER                                      
149800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
149900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
150000     PERFORM IMS-STATUSKONTROLL                                           
150100     .                                                                    
150200     SKIP3                                                                
150300 IMS-INSERT-MSG SECTION.                                                  
150400                                                                          
150500     IF MSGI-IDLAND-SPR = 'SE'                                            
150600       MOVE '0' TO MFS-KDHUVOMR                                           
150700     END-IF                                                               
150800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
150900     MOVE SPACE TO GODK-STATUSKODER                                       
151000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
151100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
151200     PERFORM IMS-STATUSKONTROLL                                           
151300     .                                                                    
151400     SKIP3                                                                
151500 IMS-ISRT-ALT-PCB SECTION.                                                
151600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
151700     MOVE SPACE TO GODK-STATUSKODER                                       
151800     CALL CBLTDLI USING ISRT ALT-PCB P-TO-P-AREA                          
151900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
152000     PERFORM IMS-STATUSKONTROLL                                           
152100     .                                                                    
152200     EJECT                                                                
152300 IMS-INSERT-PRT-MSG SECTION.                                              
152400                                                                          
152500     MOVE SPACE TO GODK-STATUSKODER                                       
152600     CALL CBLTDLI USING PURG PRT-PCB PROG-TO-PROG-SW                      
152700     MOVE PRT-STATUS-CODE TO STATUS-WS                                    
152800     PERFORM IMS-STATUSKONTROLL                                           
152900     .                                                                    
153000     EJECT                                                                
153100 IMS-GET-WDK611 SECTION.                                                  
153200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
153300          DELIMITED BY SIZE INTO SSA1                                     
153400     MOVE 'WDK611  ' TO SSA2                                              
153500     MOVE '  GE' TO GODK-STATUSKODER                                      
153600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
153700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
153800     PERFORM IMS-STATUSKONTROLL                                           
153900     SKIP3                                                                
154000     .                                                                    
154100 IMS-GNP-WDK611 SECTION.                                                  
154200     MOVE 'WDK611  ' TO SSA1                                              
154300     MOVE '  GE' TO GODK-STATUSKODER                                      
154400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
154500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
154600     PERFORM IMS-STATUSKONTROLL                                           
154700     SKIP3                                                                
154800     .                                                                    
154900 IMS-GET-WDK601 SECTION.                                                  
155000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
155100          DELIMITED BY SIZE INTO SSA1                                     
155200     MOVE '  ' TO GODK-STATUSKODER                                        
155300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
155400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
155500     PERFORM IMS-STATUSKONTROLL                                           
155600     EJECT                                                                
155700     .                                                                    
155800 IMS-GET-WDD101 SECTION.                                                  
155900     STRING 'WDD101  (IDFPINST =' W-IDFPINST-X ')'                        
156000          DELIMITED BY SIZE INTO SSA1                                     
156100     MOVE '  GE' TO GODK-STATUSKODER                                      
156200     CALL CBLTDLI USING GU WDD1-PCB DLI-IO-WDD101 SSA1                    
156300     MOVE WDD1-STATUS-CODE TO STATUS-WS                                   
156400     PERFORM IMS-STATUSKONTROLL                                           
156500     .                                                                    
156600     SKIP3                                                                
156700 IMS-GET-WDK6C-FIRST SECTION.                                             
156800     STRING 'WDK6C1  (WDK6C1KY=>' W-WDK6C1KY-MIN                          
156900                    '&WDK6C1KY=<' W-WDK6C1KY-MAX ')'                      
157000          DELIMITED BY SIZE INTO SSA1                                     
157100     MOVE '  GE' TO GODK-STATUSKODER                                      
157200     CALL CBLTDLI USING GU WDK6C-PCB DLI-IO-WDK6C SSA1                    
157300     MOVE WDK6C-STATUS-CODE TO STATUS-WS                                  
157400     PERFORM IMS-STATUSKONTROLL                                           
157500     .                                                                    
157600     SKIP3                                                                
157700 IMS-GET-WDK6C-NEXT SECTION.                                              
157800     STRING 'WDK6C1  (WDK6C1KY=>' W-WDK6C1KY-MIN                          
157900                    '&WDK6C1KY=<' W-WDK6C1KY-MAX ')'                      
158000          DELIMITED BY SIZE INTO SSA1                                     
158100     MOVE '  GE' TO GODK-STATUSKODER                                      
158200     CALL CBLTDLI USING GN WDK6C-PCB DLI-IO-WDK6C SSA1                    
158300     MOVE WDK6C-STATUS-CODE TO STATUS-WS                                  
158400     PERFORM IMS-STATUSKONTROLL                                           
158500     .                                                                    
158600     EJECT                                                                
158700 IMS-GET-WDP311 SECTION.                                                  
158800     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
158900            DELIMITED BY SIZE INTO SSA1                                   
159000     STRING 'WDP311  (IDPERSON ='  W-IDPERSON-X ')'                       
159100            DELIMITED BY SIZE INTO SSA2                                   
159200     MOVE '  GE' TO GODK-STATUSKODER                                      
159300     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
159400     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
159500     PERFORM IMS-STATUSKONTROLL                                           
159600     .                                                                    
159700     SKIP3                                                                
159800 IMS-GET-WDD1A-FIRST SECTION.                                             
159900     STRING 'WDD1A1  (WDD1A1KY=>' W-WDD1A1KY-MIN                          
160000                    '&WDD1A1KY=<' W-WDD1A1KY-MAX ')'                      
160100          DELIMITED BY SIZE INTO SSA1                                     
160200     MOVE '  GE' TO GODK-STATUSKODER                                      
160300     CALL CBLTDLI USING GU WDD1A-PCB DLI-IO-WDD1A SSA1                    
160400     MOVE WDD1A-STATUS-CODE TO STATUS-WS                                  
160500     PERFORM IMS-STATUSKONTROLL                                           
160600     .                                                                    
160700     SKIP3                                                                
160800 IMS-GET-WDD1A-NEXT SECTION.                                              
160900     STRING 'WDD1A1  (WDD1A1KY=>' W-WDD1A1KY-MIN                          
161000                    '&WDD1A1KY=<' W-WDD1A1KY-MAX ')'                      
161100          DELIMITED BY SIZE INTO SSA1                                     
161200     MOVE '  GE' TO GODK-STATUSKODER                                      
161300     CALL CBLTDLI USING GN WDD1A-PCB DLI-IO-WDD1A SSA1                    
161400     MOVE WDD1A-STATUS-CODE TO STATUS-WS                                  
161500     PERFORM IMS-STATUSKONTROLL                                           
161600     .                                                                    
161700     EJECT                                                                
161800 IMS-STATUSKONTROLL SECTION.                                              
161900                                                                          
162000     SET STATUS-IX TO 1                                                   
162100     SEARCH GODK-STATUS                                                   
162200       AT END                                                             
162300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
162400         DELIMITED BY SIZE INTO FELTEXT                                   
162500         CALL FELLOG                                                      
162600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
162700         CONTINUE                                                         
162800     END-SEARCH                                                           
162900     .                                                                    

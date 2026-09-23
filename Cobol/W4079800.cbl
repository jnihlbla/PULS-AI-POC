001400 ID DIVISION.                                                             
001500 PROGRAM-ID.     W4079800.                                                
001600 AUTHOR.         SUSANNE OLSSON.                                          
001700 DATE-WRITTEN.   02/02/14.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        PROGRAMMET ÄR EN BAKGRUNDS MPP SOM STARTAS AV PRISMODULEN        
002200*        W3039100. FÖR DDI-MARKNADER SKALL PRIS, KDVAT ETC. HÄMTAS        
002300*        FRÅN VIPS FÖR RADER SOM KRÄVER DESSA UPPGIFTER OCH PRIS=0        
002310*        TRANSAKTIONER TAS EMOT MED HJÄLP AV WZ01 MODULEN.                
002400*                                                                         
002501*        PROGRAMMET UPPDATERAR WDA2                                       
002510*        PROGRAMMET LÄSER + RENSAR  WDC7                                  
002600*                                                                         
002700*    INDATA.                                                              
002810*        TRANSAKTION: W40798X                                             
002910*        MID:         W40798I1 (VIA WZ01)                                 
003000*                                                                         
003010*    E-TRACKER 1572353 DATUM 20050420                                     
003100*                                                                         
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600                                                                          
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W4079800'.            
004100                                                                          
004200*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700                                                                          
005400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005500     88  NYCKLAR-OK                          VALUE 'J'.                   
005600     88  NYCKLAR-FEL                         VALUE 'N'.                   
005700                                                                          
006600     EJECT                                                                
006700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006800 01  GENERELLA-SUBPROGRAM.                                                
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
007310     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
007400     EJECT                                                                
008240**************DISTRIKTCOPYTEXT SOM ANGER DDI DISTRIKT                     
008251*                                                                         
008252 01  TEST-IDDISTR           PIC 9(5)   COMP-3.                            
008255     EJECT                                                                
008256*    ---  LÄNKAREA TILL W418OKOD                                          
008257 01  FILLER                      PIC X(16)   VALUE 'OKOD-AREA'.           
008258*01  -COPY W418OKOD           -PRE OKOD-.                                 
008260     EJECT                                                                
008270*    --- AREOR FÖR KOMMUNIKATION                                          
008280 01  FILLER                      PIC X(16)   VALUE 'RECEIVE-AREA'.        
008290*01  -COPY WZ01RECV                                                       
008291     SKIP3                                                                
008292 01  RECV-DATA.                                                           
008293*03  -COPY  WZ01RESP                                                      
008294*03  -COPY  W40798I1                                                      
008295                                                                          
008296 01   KDRC-DISPLAY               PIC Z(5).                                
008300     EJECT                                                                
011300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011400*                                                                         
011500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011600     SKIP3                                                                
011700 01  NYCKLAR-TILL-DLI.                                                    
011800                                                                          
011803     03  W-IDLEVANM-X.                                                    
011804         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
011805         05  W-IDKUNDNR          PIC S9(7)    VALUE ZERO COMP-3.          
011806         05  W-IDRAPPNR          PIC  9(7)    VALUE ZERO.                 
011807                                                                          
011808     03  W-WDA211KY-X.                                                    
011809         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
011810         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
011811                                                                          
011813   03  W-WDC701KY-X.                                                      
011814     05  W-PRQ-IDDISTR           PIC 9(4)  VALUE ZERO.                    
011815     05  W-PRQ-IDKUNDNR          PIC 9(7)  VALUE ZERO.                    
011816     05  W-PRQ-IDBUNDLE          PIC X(15) VALUE SPACE.                   
011817     05  W-PRQ-IDORDNR7-FILLER REDEFINES W-PRQ-IDBUNDLE.                  
011818       07  W-PRQ-IDORDNR7        PIC 9(7).                                
011819       07  FILLER                PIC X(8).                                
011820     05  W-PRQ-IDRAPPNR-FILLER REDEFINES W-PRQ-IDBUNDLE.                  
011821       07  W-PRQ-IDRAPPNR        PIC 9(7).                                
011822       07  FILLER                PIC X(8).                                
011823                                                                          
011824   03  W-IDPRQUES-X.                                                      
011825     05  W-LPRQ-IDPRQUES         PIC 9(7)  VALUE ZERO.                    
011828                                                                          
011829   03  W-WDGXKEY-4103-X.                                                  
011830       05  W-IDHTYP-4103       PIC X(4)  VALUE '4103'.                    
011840       05  W-IDDISTR-4103      PIC S9(5) VALUE ZERO COMP-3.               
011850       05  W-IDKUNDNR-4103     PIC S9(7) VALUE ZERO COMP-3.               
011860       05  W-IDRAPPNR-4103     PIC 9(7)  VALUE ZERO.                      
011870       05  FILLER              PIC X(12) VALUE SPACE.                     
011880                                                                          
011890   03  W-KEY4104-X.                                                       
011891       05  W-IDDC-4104         PIC X(2)  VALUE SPACE.                     
011892       05  W-KDKRENOT-4104     PIC X(2)  VALUE SPACE.                     
011898                                                                          
011900     SKIP2                                                                
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012500     SKIP2                                                                
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(64).                               
013000 01  SSA2                        PIC X(64).                               
013100     EJECT                                                                
013200*    --- IMS FUNKTIONSKODER                                               
013300*01  -COPY W0003                                                          
013500     EJECT                                                                
013600*    ---  DLI INPUT-OUTPUT AREA                                           
013700                                                                          
013801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA201'.                      
013802 01  DLI-IO-WDA201.                                                       
013803*    03  -COPY WDA201                                                     
013804     EJECT                                                                
013805 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA211'.                      
013806 01  DLI-IO-WDA211.                                                       
013807*    03  -COPY WDA211                                                     
013808 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC701'.                      
013809 01  DLI-IO-WDC701.                                                       
013810*    03  -COPY WDC701                                                     
013811     EJECT                                                                
013812 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC711'.                      
013813 01  DLI-IO-WDC711.                                                       
013820*    03  -COPY WDC711                                                     
013830 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4103'.                    
013840 01  DLI-IO-WDGX4103.                                                     
013850*    03  -COPY WDGX4103                                                   
013860     EJECT                                                                
013870 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4104'.                    
013880 01  DLI-IO-WDGX4104.                                                     
013890*    03  -COPY WDGX4104                                                   
014100     EJECT                                                                
014200 LINKAGE SECTION.                                                         
014300*    -- ANVÄNDS EJ I DETTA PROGRAMMET , BARA I PSB-LADDMODULEN            
014400 01  MSG-PCB                     PIC X.                                   
014602*01  -COPY W0008  -PRE WDA2-                                              
014603     05  FILLER                  PIC X.                                   
014604                                                                          
014605*01  -COPY W0008  -PRE WDC7-                                              
014610     05  FILLER                  PIC X.                                   
014700     EJECT                                                                
014800*01  -COPY W0008  -PRE 4103-                                              
014801     05  FILLER                  PIC X.                                   
014802     EJECT                                                                
014803 PROCEDURE DIVISION  USING  MSG-PCB WDA2-PCB WDC7-PCB 4103-PCB.           
014804 MAIN SECTION.                                                            
014810     ENTRY 'DLITCBL' USING  MSG-PCB WDA2-PCB WDC7-PCB 4103-PCB.           
014900                                                                          
015000     PERFORM S01-RECV-OPEN                                                
015010     PERFORM S02-RECV-MESSAGE                                             
015030     PERFORM UNTIL RECV-KDRC > ZERO                                       
015031       PERFORM A-INIT                                                     
015040       PERFORM B-UPPDATERA                                                
015050       PERFORM S02-RECV-MESSAGE                                           
015060     END-PERFORM                                                          
015061     PERFORM S03-RECV-CLOSE                                               
016800                                                                          
016900     MOVE ZERO TO RETURN-CODE                                             
017000     GOBACK                                                               
017100     .                                                                    
017200     EJECT                                                                
017300 A-INIT SECTION.                                                          
017400                                                                          
019000***** HÄMTA  NYCKELINFO FRÅN MIDEN                                        
019100     MOVE MID-IDDISTR  TO  W-IDDISTR                                      
019110                           W-PRQ-IDDISTR                                  
019120                           W-IDDISTR-4103                                 
019200     MOVE MID-IDKUNDNR TO  W-IDKUNDNR                                     
019210                           W-PRQ-IDKUNDNR                                 
019220                           W-IDKUNDNR-4103                                
019300     MOVE MID-IDRAPPNR TO  W-IDRAPPNR                                     
019301                           W-PRQ-IDRAPPNR                                 
019302                           W-IDRAPPNR-4103                                
019400     MOVE MID-IDPRQUES TO  W-LPRQ-IDPRQUES                                
019500                                                                          
020200     .                                                                    
020300     EJECT                                                                
025865 B-UPPDATERA SECTION.                                                     
025866                                                                          
025867     PERFORM IMS-GHU-WDC711                                               
025868     PERFORM IMS-GHU-WDA201                                               
025869     IF SEGMENT-FINNS                                                     
025870       IF ANM-KDVALISO = SPACE                                            
025871         MOVE LPRQ-KDVALISO  TO ANM-KDVALISO                              
025872         PERFORM IMS-REPL-WDA201                                          
025873       END-IF                                                             
025874                                                                          
025875       PERFORM IMS-GHNP-WDA211                                            
025876       IF SEGMENT-FINNS AND                                               
025877                   (LEV-IDPRQUES  = MID-IDPRQUES)                         
025878         PERFORM BA-UPPDATERA-WDA211                                      
025880       ELSE                                                               
025881         PERFORM UNTIL (LEV-IDPRQUES = MID-IDPRQUES) OR                   
025882                                    SEGMENT-SAKNAS                        
025883           PERFORM IMS-GHNP-WDA211                                        
025884           IF SEGMENT-FINNS                                               
025885             IF LEV-IDPRQUES = MID-IDPRQUES                               
025886               PERFORM BA-UPPDATERA-WDA211                                
025896             END-IF                                                       
025897           END-IF                                                         
025898         END-PERFORM                                                      
025899       END-IF                                                             
025900     END-IF                                                               
025901     .                                                                    
025910     EJECT                                                                
026000 BA-UPPDATERA-WDA211 SECTION.                                             
026100                                                                          
026141     IF LEV-KDVAT = SPACE                                                 
026142       MOVE LPRQ-KDVAT          TO LEV-KDVAT                              
026143     END-IF                                                               
026150     IF LEV-BEART-VIPS = SPACE                                            
026160       MOVE LPRQ-BEART-VIPS     TO LEV-BEART-VIPS                         
026170     END-IF                                                               
026171                                                                          
026172*--- ANROPA KONTROLL AV ORSAKSKODER                                       
026173                                                                          
026174     MOVE LEV-KDANMORS          TO OKOD-KDANMORS                          
026175     CALL W418OKOD USING OKOD-W418OKOD                                    
026176                                                                          
026177     IF OKOD-FL-PRISTILLAEMPA = JA                                        
026178                                                                          
026180       IF LEV-PRARTBTO-LOC = ZERO                                         
026190         MOVE LPRQ-PRARTNTO-LOC   TO LEV-PRARTBTO-LOC                     
026191         IF ANM-IDFTG = 57                                                
026192           PERFORM BAA-UPD-ATTEST-KRENOT                                  
026193         END-IF                                                           
026194       END-IF                                                             
026195     END-IF                                                               
026196                                                                          
026197     PERFORM IMS-REPL-WDA211                                              
026198     PERFORM IMS-DLET-WDC711                                              
026199                                                                          
026210     .                                                                    
026300     EJECT                                                                
026310 BAA-UPD-ATTEST-KRENOT SECTION.                                           
026320                                                                          
026391     IF (OKOD-FL-KRENOT-DIREKT = JA)     OR                               
026392        (OKOD-FL-KRENOT-DIREKT-SKR = JA) OR                               
026393        (OKOD-FL-KRENOT-EFTER-RT = JA)   OR                               
026394        LEV-KDANMORS = '97'                                               
026397       IF (OKOD-FL-KRENOT-EFTER-RT = JA) OR                               
026398          LEV-KDANMORS = '97'                                             
026399          MOVE 'RP' TO W-KDKRENOT-4104                                    
026400       ELSE                                                               
026401          MOVE 'CN' TO W-KDKRENOT-4104                                    
026402       END-IF                                                             
026403                                                                          
026404       MOVE LEV-IDDC TO W-IDDC-4104                                       
026405                                                                          
026406       PERFORM IMS-GU-WDR501-4103                                         
026407       IF SEGMENT-FINNS                                                   
026408         PERFORM IMS-GHNP-WDGX4104                                        
026409         ADD LPRQ-PRARTNTO-LOC TO 4104-SUKRENOT                           
026410         PERFORM IMS-REPL-WDGX4104                                        
026411       END-IF                                                             
026412     END-IF                                                               
026420     .                                                                    
026500     EJECT                                                                
030510 S01-RECV-OPEN SECTION.                                                   
030520     MOVE 'OPEN' TO RECV-KDFUNC                                           
030530     MOVE 'CARPARTS.PULS.CREPRICE' TO RECV-ADDISPABS                      
030540                                                                          
030550     CALL WZ01RECV USING           RECV-CONTROL-AREA                      
030560                                   RECV-OPEN-AREA                         
030570********              ...FELHANTERING...                                  
030580     IF RECV-KDRC > 0                                                     
030590      MOVE RECV-KDRC TO KDRC-DISPLAY                                      
030591      STRING 'WZ01RECV OPEN ERROR RC= ' KDRC-DISPLAY                      
030592      DELIMITED BY SIZE INTO FELTEXT                                      
030593      DISPLAY FELTEXT                                                     
030594      CALL FELLOG                                                         
030595     END-IF                                                               
030596     .                                                                    
030597     EJECT                                                                
030598 S02-RECV-MESSAGE SECTION.                                                
030599                                                                          
030600     MOVE 'GET' TO RECV-KDFUNC                                            
030601     MOVE LENGTH OF RECV-DATA TO RECV-KVDLEN                              
030602     CALL WZ01RECV USING RECV-CONTROL-AREA                                
030603                         RECV-KVDLEN                                      
030604                         RECV-DATA                                        
030605**FELHANTERING...                                                         
030606     IF RECV-KDRC > 1                                                     
030607       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
030608       STRING 'WZ01RECV GET  ERROR RC= ' KDRC-DISPLAY                     
030609       DELIMITED BY SIZE INTO FELTEXT                                     
030610       DISPLAY FELTEXT                                                    
030611       CALL FELLOG                                                        
030612     END-IF                                                               
030613     .                                                                    
030614     EJECT                                                                
030615 S03-RECV-CLOSE SECTION.                                                  
030616                                                                          
030617     MOVE 'CLOSE' TO RECV-KDFUNC                                          
030618     CALL WZ01RECV USING RECV-CONTROL-AREA                                
030619**FELHANTERING...                                                         
030620     IF RECV-KDRC > 0                                                     
030621      MOVE RECV-KDRC TO KDRC-DISPLAY                                      
030622      STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                     
030623       DELIMITED BY SIZE INTO FELTEXT                                     
030624       DISPLAY FELTEXT                                                    
030625       CALL FELLOG                                                        
030626     END-IF                                                               
030627     .                                                                    
030628     EJECT                                                                
030630* --- IMS SEKTIONER ---                                                   
030700     SKIP3                                                                
032702 IMS-GHU-WDA201 SECTION.                                                  
032703                                                                          
032704     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
032705          DELIMITED BY SIZE INTO SSA1                                     
032706     MOVE '  GE' TO GODK-STATUSKODER                                      
032707     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA201 SSA1                   
032708     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
032709     PERFORM IMS-STATUSKONTROLL                                           
032710     .                                                                    
032711     SKIP3                                                                
032712 IMS-REPL-WDA201 SECTION.                                                 
032713                                                                          
032714     MOVE '  ' TO GODK-STATUSKODER                                        
032715     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA201                       
032716     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
032717     PERFORM IMS-STATUSKONTROLL                                           
032718     .                                                                    
032719     EJECT                                                                
032720 IMS-GHNP-WDA211 SECTION.                                                 
032721                                                                          
032722     MOVE 'WDA211   ' TO SSA1                                             
032725     MOVE '  GE' TO GODK-STATUSKODER                                      
032726     CALL CBLTDLI USING GHNP WDA2-PCB DLI-IO-WDA211 SSA1                  
032727     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
032728     PERFORM IMS-STATUSKONTROLL                                           
032729     .                                                                    
032730     SKIP3                                                                
032731 IMS-REPL-WDA211 SECTION.                                                 
032732                                                                          
032733     MOVE '  ' TO GODK-STATUSKODER                                        
032734     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA211                       
032735     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
032736     PERFORM IMS-STATUSKONTROLL                                           
032737     .                                                                    
032738     EJECT                                                                
032810 IMS-GHU-WDC711 SECTION.                                                  
032820                                                                          
032830     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
032840          DELIMITED BY SIZE INTO SSA1                                     
032850     STRING 'WDC711  (IDPRQUES =' W-IDPRQUES-X ')'                        
032860          DELIMITED BY SIZE INTO SSA2                                     
032870     MOVE '    ' TO GODK-STATUSKODER                                      
032880     CALL CBLTDLI USING GHU WDC7-PCB DLI-IO-WDC711 SSA1 SSA2              
032890     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
032891     PERFORM IMS-STATUSKONTROLL                                           
032892     .                                                                    
032893     SKIP3                                                                
032894 IMS-DLET-WDC711 SECTION.                                                 
032895                                                                          
032896     MOVE '  ' TO GODK-STATUSKODER                                        
032897     CALL CBLTDLI USING DLET WDC7-PCB DLI-IO-WDC711                       
032898     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
032899     PERFORM IMS-STATUSKONTROLL                                           
032900     .                                                                    
032901     EJECT                                                                
032902 IMS-GU-WDR501-4103 SECTION.                                              
032903                                                                          
032904     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-4103-X ')'                    
032905          DELIMITED BY SIZE INTO SSA1                                     
032906     MOVE '  GE' TO GODK-STATUSKODER                                      
032907     CALL CBLTDLI USING GU 4103-PCB DLI-IO-WDGX4103 SSA1                  
032908     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
032909     PERFORM IMS-STATUSKONTROLL                                           
032910     .                                                                    
032920     SKIP3                                                                
032930 IMS-GHNP-WDGX4104 SECTION.                                               
032931                                                                          
032932     STRING 'WDGX4104*F(KEY4104  =' W-KEY4104-X ')'                       
032933          DELIMITED BY SIZE INTO SSA2                                     
032934     MOVE '  GE' TO GODK-STATUSKODER                                      
032935     CALL CBLTDLI USING GHNP 4103-PCB DLI-IO-WDGX4104 SSA1                
032936     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
032937     PERFORM IMS-STATUSKONTROLL                                           
032938     .                                                                    
032939     SKIP3                                                                
032949 IMS-REPL-WDGX4104 SECTION.                                               
032950                                                                          
032951     MOVE 'WDGX4104' TO SSA1                                              
032952     MOVE '    ' TO GODK-STATUSKODER                                      
032953     CALL CBLTDLI USING REPL 4103-PCB DLI-IO-WDGX4104 SSA1                
032954     MOVE 4103-STATUS-CODE TO STATUS-WS                                   
032955     PERFORM IMS-STATUSKONTROLL                                           
032956     .                                                                    
032957     EJECT                                                                
032960 IMS-STATUSKONTROLL SECTION.                                              
033000                                                                          
033100     SET STATUS-IX TO 1                                                   
033200     SEARCH GODK-STATUS                                                   
033300       AT END                                                             
033400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033500         DELIMITED BY SIZE INTO FELTEXT                                   
033600         CALL FELLOG                                                      
033700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033800         CONTINUE                                                         
033900     END-SEARCH                                                           
034000     .                                                                    

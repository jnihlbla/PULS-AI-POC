000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6016200.                                                
000400 AUTHOR.         MÅNS SAMUELSSON/TOMMIE JIVARP                            
000500 DATE-WRITTEN.   95/12/12.      /TILLÄGG 98/03/18.                        
000600 DATE-COMPILED.                                                           
000700                                                                          
000800**   FUNKTION:                                                            
000900*        UPPDATERAR BUFFERTSALDO                                          
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001200*        PROGRAMMET UPPDATERAR WLARTD (WDD8)                              
001300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001400*        PROGRAMMET UPPDATERAR WLLOCB (WDJ9)                              
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W6T162                                              
001800*        MID:         W6I16201                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W6O16201                                            
002200*                                                                         
002300*    CHANGE LOG:                                                          
002400*      YY/MM/DD - INITIALS        - DESCRIPTION.                          
002500*                                                                         
002600*                                 - e-tracker: 7450319                    
002700*                                   2008-höst  vohf                       
002800*                                                                         
002900*      14/02/19 - REDDY RAHUL     - ETRACKER 10207035                     
003000*                                   ADD AREA 20, 24 AND 28 TO BE          
003100*                                   INCLUDED AS DATED BUFFER AREA         
003200*      2015     - GÖRAN KJELLSON  - ETRACKER 10254592                     
003300*                                   DECOMISSION VOHF                      
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000*    -- CHECKED BY WY2000                                                 
004100     SKIP3                                                                
004200 77  IDPGM                       PIC X(08)   VALUE 'W6016200'.            
004300                                                                          
004400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004600                                                                          
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000 77  RAD-IX                      PIC S9(9)   VALUE +0 COMP SYNC.          
005100 77  MAX-IX                      PIC S9(9)   VALUE +7 COMP SYNC.          
005200 77  SPRAK-IX                    PIC S9(3)   VALUE +1 COMP SYNC.          
005300                                                                          
005400*    --- ARBETSFÄLT FÖR UPPDATERING AV PLATSREGISTRET (WDJ9)              
005500 77      LOGG-DATUM         PIC S9(8)             VALUE ZERO.             
005600 77      LOGG-TID           PIC S9(7)             VALUE ZERO.             
005700 77      BUFFER-LOCATION    PIC X                 VALUE 'B'.              
005800                                                                          
005900 01  WS-TIORDTIME.                                                        
006000     05 ORDDATE  PIC 9(6)             VALUE ZERO.                         
006100     05 ORDTIME  PIC 9(6)             VALUE ZERO.                         
006200                                                                          
006300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006400                                                                          
006500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006600     88  INDATA-OK                           VALUE 'J'.                   
006700     88  INDATA-FEL                          VALUE 'N'.                   
006800 77  UPDATE-W6A162               PIC X       VALUE 'N'.                   
006900     88  W6A162-OK                           VALUE 'J'.                   
007000     88  W6A162-NOK                          VALUE 'N'.                   
007100 77  SYNQ-CALL                   PIC X       VALUE 'N'.                   
007200     88  SYNQ-OK                           VALUE 'J'.                     
007300     88  SYNQ-FEL                          VALUE 'N'.                     
007400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007500     88  NYCKLAR-OK                          VALUE 'J'.                   
007600     88  NYCKLAR-FEL                         VALUE 'N'.                   
007700                                                                          
007800 77  BATTERY-SW                  PIC 9(2).                                
007900     88  BATTERY-LOC                         VALUE 46.                    
008000                                                                          
008100 77  BUFFOMR-SW                  PIC X       VALUE 'J'.                   
008200     88  BUFFOMR-OK                          VALUE 'J'.                   
008300     88  BUFFOMR-FEL                         VALUE 'N'.                   
008400                                                                          
008500 77  UPD-FRAN-NYCKELRAD-SW       PIC X       VALUE 'N'.                   
008600     88  UPD-FRAN-NYCKELRAD                  VALUE 'J'.                   
008700                                                                          
008800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008900     88  EGEN-MID                            VALUE '6162'.                
009000     88  HELP-MID                            VALUE '0551'.                
009100                                                                          
009200 77  WX-ADBUFFOMR                PIC X(2)    VALUE SPACE.                 
009300 77  WY-ADBUFFOMR                PIC X(2)    VALUE SPACE.                 
009400     EJECT                                                                
009500 01  WS-KVBUFF                   PIC X(7).                                
009600 01  WS-ADBUFFOMR                PIC X(2).                                
009700 01  WS-ADBUFFGANG               PIC X(2).                                
009800 01  WS-ADBUFFPL                 PIC X(5).                                
009900 01  WS-ANTAL                    PIC 9(7).                                
010000                                                                          
010100 01  WS-SUBUFF-F                 PIC S9(7)   VALUE +0.                    
010200 01  WS-SUBUFF-OF                PIC S9(7)   VALUE +0.                    
010300 01  WS-SUKOLLI-F                PIC S9(5)   VALUE +0.                    
010400 01  WS-SUKOLLI-OF               PIC S9(5)   VALUE +0.                    
010500                                                                          
010600 01  W-WDD811KY-AREA.                                                     
010700   02  W-MINKEY-WDD811KY.                                                 
010800     03  W-MINKEY-IDTRANS        PIC X(4)    VALUE '6162'.                
010900     03  W-MINKEY-ADBUFFOMR      PIC S9(3)   VALUE +0 COMP-3.             
011000     03  W-MINKEY-DABUFPAF       PIC  9(8)   VALUE ZERO.                  
011100     03  W-MINKEY-ADBUFFGANG     PIC S9(3)   VALUE +0 COMP-3.             
011200     03  W-MINKEY-ADBUFFPL       PIC S9(5)   VALUE +0 COMP-3.             
011300                                                                          
011400   02  W-WDD811KY-ENTER.                                                  
011500     03  W-IDTRANS-ENTER        PIC X(4)    VALUE '6162'.                 
011600     03  W-ADBUFFOMR-ENTER      PIC S9(3)   VALUE +0 COMP-3.              
011700     03  W-DABUFPAF-ENTER       PIC  9(8)   VALUE ZERO.                   
011800     03  W-ADBUFFGANG-ENTER     PIC S9(3)   VALUE +0 COMP-3.              
011900     03  W-ADBUFFPL-ENTER       PIC S9(5)   VALUE +0 COMP-3.              
012000                                                                          
012100*      --- VALID IDDC CODES                                               
012200*                                                                         
012300*01    -COPY WWDCKONS                                                     
012400       EJECT                                                              
012500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012600 01  GENERELLA-SUBPROGRAM.                                                
012700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013000     03  W488ORCR                PIC X(8)    VALUE 'W488ORCR'.            
013100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013200     EJECT                                                                
013300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013400*01 -COPY WMEDAREA                                                        
013500     SKIP3                                                                
013600*    --- PARAMETERS FOR SUB PROGRAM W488ORCR                              
013700 01  FILLER                      PIC X(16)   VALUE 'W488ORCR'.            
013800*01 -COPY W488ORCR                                                        
013900     EJECT                                                                
014000 01  MESSAGE-CODES.                                                       
014100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014200     03  ERR-CONFLICT-FLDS       PIC X(3)    VALUE '002'.                 
014300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014400     03  ERR-MISSING-IN-REG      PIC X(3)    VALUE '010'.                 
014500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014600     03  ERR-MISSING-IN-ARTREG   PIC X(3)    VALUE '017'.                 
014700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014900     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
015000     03  ERR-ALREADY-EXIST       PIC X(3)    VALUE '245'.                 
015100     03  ERR-MISSING-IN-BUFF-REG PIC X(3)    VALUE '316'.                 
015200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
015300     EJECT                                                                
015400 01  MEDDELANDE.                                                          
015500   03   FEL1.                                                             
015600     05 FILLER                   PIC X(40)                                
015700          VALUE 'ENDAST BO 42 ELLER 43 TILLÅTEN'.                         
015800     05 FILLER                   PIC X(40)                                
015900          VALUE 'ONLY  BO 42 OR 43 ALLOWED  '.                            
016000   03  FILLER REDEFINES FEL1.                                             
016100     05  FEL-1                   PIC X(40)   OCCURS 2.                    
016200   03   FEL2.                                                             
016300     05 FILLER                   PIC X(40)                                
016400          VALUE 'ENDAST BO 42/43 TILLÅTEN/ NYCKEL FEL'.                   
016500     05 FILLER                   PIC X(40)                                
016600          VALUE 'ONLY  BO 42/43 ALLOWED  /KEYS WRONG'.                    
016700   03  FILLER REDEFINES FEL2.                                             
016800     05  FEL-2                   PIC X(40)   OCCURS 2.                    
016900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
017000*                                                                         
017100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
017200     SKIP3                                                                
017300*01 -COPY WMSGINIT                                                        
017400     SKIP3                                                                
017500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017600*                                                                         
017700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017800     SKIP3                                                                
017900*01  MID -COPY W6I16201                                                   
018000*01  MID -COPY W6I16202                                                   
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018300     SKIP3                                                                
018400*01  -COPY WMSGAREA                                                       
018500     EJECT                                                                
018600     03  MOD REDEFINES MSG-AREA.                                          
018700*      05  -COPY W6O16201                                                 
018800     EJECT                                                                
018900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019000     SKIP3                                                                
019100*01  -COPY WMFSAREA                                                       
019200     EJECT                                                                
019300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019400*                                                                         
019500     EJECT                                                                
019600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019700     SKIP3                                                                
019800 01  NYCKLAR-TILL-DLI.                                                    
019900     03  W-IDARTNR-X.                                                     
020000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
020100     03  W-IDDC-X.                                                        
020200         05  W-IDDC              PIC X(2)    VALUE '11'.                  
020300     03  W-KDSEGKEY-X.                                                    
020400         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
020500     03  W-WDD811KY-X.                                                    
020600         05  W-IDDC              PIC X(2)    VALUE '11'.                  
020700         05  W-ADBUFFOMR         PIC S9(3)   VALUE ZERO COMP-3.           
020800         05  W-DABUFPAF          PIC  9(8)   VALUE ZERO.                  
020900         05  W-ADBUFFGANG        PIC S9(3)   VALUE ZERO COMP-3.           
021000         05  W-ADBUFFPL          PIC S9(5)   VALUE ZERO COMP-3.           
021100     03  W-WDD811KY-MIN-X.                                                
021200         05  W-IDDC-MIN          PIC X(2)    VALUE '11'.                  
021300         05  W-ADBUFFOMR-MIN     PIC S9(3)   VALUE ZERO COMP-3.           
021400         05  W-DABUFPAF-MIN      PIC  9(8)   VALUE ZERO.                  
021500         05  W-ADBUFFGANG-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
021600         05  W-ADBUFFPL-MIN      PIC S9(5)   VALUE ZERO COMP-3.           
021700     03  W-WDD811KY-MAX-X.                                                
021800         05  W-IDDC-MAX          PIC X(2)    VALUE '11'.                  
021900         05  W-ADBUFFOMR-MAX     PIC S9(3)   VALUE +999 COMP-3.           
022000         05  W-ADBUFPAF-MAX      PIC  9(8)   VALUE  99999999.             
022100         05  W-ADBUFFGANG-MAX    PIC S9(3)   VALUE +999 COMP-3.           
022200         05  W-ADBUFFPL-MAX      PIC S9(5)   VALUE +99999 COMP-3.         
022300     03  W-WDJ911KY-X.                                                    
022400         05  W-IDDC-WDJ9         PIC 9(2)    VALUE ZERO.                  
022500         05  W-DASTADAT          PIC S9(9)   VALUE ZERO.                  
022600         05  W-TISTATID          PIC S9(7)   VALUE ZERO.                  
022700         05  W-ADLAGOMR          PIC 9(2)    VALUE ZERO COMP-3.           
022800         05  W-ADGANG            PIC 9(2)    VALUE ZERO COMP-3.           
022900         05  W-ADPLATS           PIC 9(5)    VALUE ZERO COMP-3.           
023000                                                                          
023100     03  W-WDD8A1KY-MIN-X.                                                
023200         05  W-IDDC-A1KY-MIN      PIC X(2)  VALUE '11'.                   
023300         05  W-ADBUFFOM-A1KY-MIN  PIC S9(3) VALUE +000 COMP-3.            
023400         05  W-ADBUFGAN-A1KY-MIN  PIC S9(3) VALUE +000 COMP-3.            
023500         05  W-ADBUFPL-A1KY-MIN   PIC S9(5) VALUE +00000 COMP-3.          
023600         05  FILLER               PIC X(13) VALUE LOW-VALUE.              
023700     03  W-WDD8A1KY-MAX-X.                                                
023800         05  W-IDDC-A1KY-MAX      PIC X(2)  VALUE '11'.                   
023900         05  W-ADBUFFOM-A1KY-MAX  PIC S9(3) VALUE +000 COMP-3.            
024000         05  W-ADBUFGAN-A1KY-MAX  PIC S9(3) VALUE +000 COMP-3.            
024100         05  W-ADBUFPL-A1KY-MAX   PIC S9(5) VALUE +00000 COMP-3.          
024200         05  FILLER               PIC X(13) VALUE HIGH-VALUE.             
024300                                                                          
024400     03  W-IDSKYLT-X.                                                     
024500         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
024600     SKIP2                                                                
024700*    --- STATUS-KOD FRÅN IMS                                              
024800 01  STATUS-WS                   PIC XX.                                  
024900     88  SEGMENT-FINNS                       VALUE '  '.                  
025000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025200     SKIP2                                                                
025300 01  GODK-STATUSKODER.                                                    
025400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025500     SKIP3                                                                
025600 01  SSA1                        PIC X(128).                              
025700 01  SSA2                        PIC X(96).                               
025800     EJECT                                                                
025900*    --- IMS FUNKTIONSKODER                                               
026000*01  -COPY W0003                                                          
026100     EJECT                                                                
026200*    --- AREOR FÖR BAKGRUNDS MPP:ER / BMP:ER                              
026300*                                                                         
026400 01  P-TO-P-T43.                                                          
026500*----TILL W60143                                                          
026600   03  PTOP1-LL              PIC S9(4)   COMP SYNC.                       
026700   03  PTOP1-Z1              PIC X(1)    VALUE LOW-VALUE.                 
026800   03  PTOP1-Z2              PIC X(1)    VALUE LOW-VALUE.                 
026900   03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W6T143X'.                 
027000   03  FILLER                PIC X(1)    VALUE SPACE.                     
027100   03  PTOP1-IDTRANS         PIC X(4)    VALUE '6162'.                    
027200   03  PTOP1-KDMFSFOR        PIC X(1)    VALUE SPACE.                     
027300*  03  MID -COPY W6I14301       -PRE T43-                                 
027400   EJECT                                                                  
027500                                                                          
027600 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-AREA'.         
027700                                                                          
027800*01  -COPY WMSGSNUF   -PRE  P-TO-P-                                       
027900   EJECT                                                                  
028000*    ---  DLI INPUT-OUTPUT AREA                                           
028100 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
028200     SKIP3                                                                
028300 01  DLI-IO-AREA.                                                         
028400     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
028500     SKIP3                                                                
028600     03  WLARTC01 REDEFINES IO-AREA.                                      
028700*        05  -COPY WDK601 -PRE ARTC-                                      
028800     SKIP3                                                                
028900     03  WLARTC11 REDEFINES IO-AREA.                                      
029000*        05  -COPY WDK611 -PRE ARTC-                                      
029100     SKIP3                                                                
029200     03  WLARTD01 REDEFINES IO-AREA.                                      
029300*        05  -COPY WDD801 -PRE ARTD-                                      
029400     SKIP3                                                                
029500     03  WLARTD11 REDEFINES IO-AREA.                                      
029600*        05  -COPY WDD811 -PRE ARTD-                                      
029700     SKIP3                                                                
029800     03  WLBENA11 REDEFINES IO-AREA.                                      
029900*        05  -COPY WDD311 -PRE BENA-                                      
030000     SKIP3                                                                
030100     03  WLLOCB01 REDEFINES IO-AREA.                                      
030200*        05  -COPY WDJ901 -PRE LOCB-                                      
030300     SKIP3                                                                
030400     03  WLLOCB11 REDEFINES IO-AREA.                                      
030500*        05  -COPY WDJ911 -PRE LOCB-                                      
030600     EJECT                                                                
030700*    ---  DLI INPUT-OUTPUT AREA3                                          
030800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
030900     SKIP3                                                                
031000 01  DLI-IO-AREA3.                                                        
031100     03  IO-AREA3                PIC X(100)  VALUE SPACE.                 
031200     SKIP3                                                                
031300     03  WDD8A1-X REDEFINES IO-AREA3.                                     
031400*        05  -COPY WDD8A1                                                 
031500     SKIP3                                                                
031600 LINKAGE SECTION.                                                         
031700                                                                          
031800*01  -COPY W0009   -PRE MSG-                                              
031900*01  -COPY W0009  -PRE SYNQ-                                              
032000*01  -COPY W0009   -PRE ALT1-                                             
032100     EJECT                                                                
032200*01  -COPY W0008   -PRE USEA-                                             
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
032500*01  -COPY W0008  -PRE ARTC-                                              
032600     05  FILLER                  PIC X.                                   
032700     EJECT                                                                
032800*01  -COPY W0008  -PRE ARTD-                                              
032900     05  FILLER                  PIC X.                                   
033000     EJECT                                                                
033100*01  -COPY W0008  -PRE BENA-                                              
033200     05  FILLER                  PIC X.                                   
033300     EJECT                                                                
033400*01  -COPY W0008  -PRE LOCB-                                              
033500     05  FILLER                  PIC X.                                   
033600     EJECT                                                                
033700*01  -COPY W0008  -PRE WDD8A-                                             
033800     05  FILLER                  PIC X.                                   
033900     EJECT                                                                
034000 01  SYNQ-ATAB-PCB             PIC X.                                     
034000 01  WDQ3-PCB                  PIC X.                                     
034100 PROCEDURE DIVISION  USING MSG-PCB SYNQ-PCB ALT1-PCB                      
034200                           USEA-PCB ARTC-PCB ARTD-PCB                     
034300         BENA-PCB LOCB-PCB WDD8A-PCB SYNQ-ATAB-PCB WDQ3-PCB.              
034400 MAIN SECTION.                                                            
034500     ENTRY 'DLITCBL' USING MSG-PCB SYNQ-PCB ALT1-PCB                      
034600                           USEA-PCB ARTC-PCB ARTD-PCB                     
034700         BENA-PCB LOCB-PCB WDD8A-PCB SYNQ-ATAB-PCB WDQ3-PCB.              
034800                                                                          
034900     PERFORM IMS-GET-MSG                                                  
035000     IF SEGMENT-FINNS                                                     
035100       PERFORM A-INIT                                                     
035200       PERFORM B-KOLLA-NYCKLAR                                            
035300       IF NYCKLAR-OK                                                      
035400         IF MFS-UPDATE OR W6A162-OK                                       
035500           PERFORM G-KOLLA-INPUT                                          
035600           IF INDATA-OK                                                   
035700             PERFORM H-UPPDATERA                                          
035800           END-IF                                                         
035900         ELSE                                                             
036000           IF MFS-FIRST                                                   
036100             PERFORM C-FOERSTA-SIDA                                       
036200           ELSE                                                           
036300             IF MFS-NEXT                                                  
036400               PERFORM D-NAESTA-SIDA                                      
036500             ELSE                                                         
036600               PERFORM E-SAMMA-SIDA                                       
036700             END-IF                                                       
036800           END-IF                                                         
036900         END-IF                                                           
037000         IF INDATA-OK AND W6A162-NOK                                      
037100           PERFORM F-LAES-VISA-INFO                                       
037200         END-IF                                                           
037300       END-IF                                                             
037400       PERFORM IMS-INSERT-MSG                                             
037500     END-IF                                                               
037600                                                                          
037700     MOVE ZERO TO RETURN-CODE                                             
037800     GOBACK                                                               
037900     .                                                                    
038000     EJECT                                                                
038100 A-INIT SECTION.                                                          
038200                                                                          
038300     IF MSG-KDTRANS-1(1:6) = 'W6A162'                                     
038400         MOVE ALL '+' TO MID-W6I16201                                     
038500         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO REQU-W6I16202               
038600         MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                               
038700         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
038800         PERFORM AA-FILL-BUFFSALDO                                        
038900     ELSE                                                                 
039000     IF MSG-DUBBLA-TRANSKODER                                             
039100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I16201                 
039200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
039300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
039400     ELSE                                                                 
039500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I16201                  
039600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
039700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
039800     END-IF                                                               
039900     END-IF                                                               
040000                                                                          
040100     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
040200     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
040300     MOVE MFS-IDTRANS TO W-IDTRANS                                        
040400                                                                          
040500     MOVE LOW-VALUE TO MSG-AREA                                           
040600     MOVE 'W6O162N1' TO MFS-IDMOD                                         
040700     MOVE '6162' TO MOD-IDTRANS                                           
040800     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
040900                                                                          
041000     COMPUTE MSG-KVLL = LENGTH OF MOD-W6O16201 + 4                        
041100                                                                          
041200     IF EGEN-MID OR HELP-MID                                              
041300       CONTINUE                                                           
041400     ELSE                                                                 
041500       MOVE SPACE TO MFS-KDTRTYP                                          
041600       MOVE '7' TO MFS-IDPFK                                              
041700     END-IF                                                               
041800     MOVE SPACE    TO MED-IDMFSINF                                        
041900     .                                                                    
042000     EJECT                                                                
042100 AA-FILL-BUFFSALDO SECTION.                                               
042200         MOVE JA  TO UPDATE-W6A162                                        
042300         MOVE 1 TO W-ADBUFFOMR                                            
042400         MOVE 0 TO W-ADBUFFGANG                                           
042500         MOVE 0 TO W-ADBUFFPL                                             
042600         MOVE REQU-IDARTNR-SYNQ TO W-IDARTNR                              
042700         PERFORM IMS-GU-ARTD11                                            
042800         IF SEGMENT-FINNS                                                 
042900          MOVE '01' TO MID-ADBUFFOMR(1)                                   
043000          MOVE '00' TO MID-ADBUFFGANG(1)                                  
043100          MOVE '00000' TO MID-ADBUFFPL(1)                                 
043200          MOVE '00000000' TO MID-DABUFPAF(1)                              
043300          MOVE REQU-KVBUFF-F-SYNQ TO MID-KVBUFF-F-IN(1)                   
043400          MOVE REQU-KVBUFF-OF-SYNQ TO MID-KVBUFF-OF-IN(1)                 
043500          MOVE REQU-KVKOLLI-F-SYNQ TO MID-KVKOLLI-F-IN(1)                 
043600          MOVE REQU-KVKOLLI-OF-SYNQ TO MID-KVKOLLI-OF-IN(1)               
043700         IF REQU-KDTECKEN-BUFF-F-SYNQ = '-' OR                            
043800            REQU-KDTECKEN-BUFF-OF-SYNQ = '-' OR                           
043900            REQU-KDTECKEN-KLI-F-SYNQ = '-' OR                             
044000            REQU-KDTECKEN-KLI-OF-SYNQ = '-'                               
044400            MOVE 'U' TO MID-KDCMDVAL (1)                                  
044300         ELSE                                                             
044100            MOVE 'IN' TO MID-KDCMDVAL (1)                                 
044200           PERFORM S02-TRANS-W60143                                       
044500         END-IF                                                           
044600         END-IF                                                           
044700         IF SEGMENT-SAKNAS                                                
044800          MOVE '01' TO MID-ADBUFFOMR-UPD                                  
044900          MOVE '00' TO MID-ADBUFFGANG-UPD                                 
045000          MOVE '00000' TO MID-ADBUFFPL-UPD                                
045100          MOVE ALL '+' TO MID-KVBUFF-F-IN(1)                              
045200                         MID-KVBUFF-OF-IN(1)                              
045300                         MID-KVKOLLI-F-IN(1)                              
045400                         MID-KVKOLLI-OF-IN(1)                             
045500                                                                          
045600          MOVE REQU-KVBUFF-F-SYNQ  TO MID-KVBUFF-F-UPD                    
045700          MOVE REQU-KVBUFF-OF-SYNQ TO MID-KVBUFF-OF-UPD                   
045800          MOVE REQU-KVKOLLI-F-SYNQ TO MID-KVKOLLI-F-UPD                   
045900          MOVE REQU-KVKOLLI-OF-SYNQ TO MID-KVKOLLI-OF-UPD                 
046000         END-IF                                                           
046100     .                                                                    
046200     EJECT                                                                
046300 B-KOLLA-NYCKLAR SECTION.                                                 
046400                                                                          
046500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
046600     MOVE '001'             TO MSGI-KDCALL                                
046700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
046800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
046900     MOVE '6162'            TO MSGI-IDTRANS                               
047000     IF EGEN-MID                                                          
047100         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
047200     ELSE                                                                 
047300         MOVE ALL '+'            TO MID-KVBUFF-IN                         
047400                                    MID-ADBUFFOMR-IN                      
047500                                    MID-ADBUFFGANG-IN                     
047600                                    MID-ADBUFFPL-IN                       
047700         MOVE SPACE              TO MID-KVBUFF-UT                         
047800                                    MID-ADBUFFOMR-UT                      
047900                                    MID-ADBUFFGANG-UT                     
048000                                    MID-ADBUFFPL-UT                       
048100     END-IF                                                               
048200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
048300                                                                          
048400     IF MSGI-IDLAND-SPR = 'GB' OR W6A162-OK                               
048500       MOVE 'GB '           TO W-IDSKYLT                                  
048600                               MED-IDSKYLT                                
048700       MOVE +2              TO SPRAK-IX                                   
048800     ELSE                                                                 
048900       MOVE 'S  '           TO W-IDSKYLT                                  
049000                               MED-IDSKYLT                                
049100       MOVE +1              TO SPRAK-IX                                   
049200     END-IF                                                               
049300     MOVE JA TO NYCKLAR-SW                                                
049400                                                                          
049500                                                                          
049600*    -- KONTROLL AV IDARTNR                                               
049700     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
049800                                                                          
049900     IF MID-IDARTNR-IN NOT = ALL '+'                                      
050000       MOVE '7'         TO MFS-IDPFK                                      
050100       MOVE SPACE       TO MFS-KDTRTYP                                    
050200     END-IF                                                               
050300     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
050400     IF MSGI-IDARTNR NUMERIC AND W6A162-NOK                               
050500       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
050600     ELSE                                                                 
050700       IF W6A162-NOK                                                      
050800       MOVE NEJ TO NYCKLAR-SW                                             
050900       END-IF                                                             
051000     END-IF                                                               
051100                                                                          
051200*    -- KONTROLL AV KVBUFF                                                
051300     MOVE MFS-RENSA-FAELT TO MOD-KVBUFF-IN                                
051400                                                                          
051500     IF MID-KVBUFF-IN NOT = ALL '+'                                       
051600       MOVE '7'         TO MFS-IDPFK                                      
051700       MOVE SPACE       TO MFS-KDTRTYP                                    
051800       MOVE MID-KVBUFF-IN   TO WS-KVBUFF                                  
051900     ELSE                                                                 
052000       IF MID-IDARTNR-IN NOT = ALL '+'                                    
052100         MOVE ZERO            TO WS-KVBUFF                                
052200       ELSE                                                               
052300         MOVE MID-KVBUFF-UT   TO WS-KVBUFF                                
052400       END-IF                                                             
052500     END-IF                                                               
052600     INSPECT WS-KVBUFF REPLACING LEADING SPACE BY ZERO                    
052700     IF WS-KVBUFF NUMERIC AND W6A162-NOK                                  
052800       CONTINUE                                                           
052900     ELSE                                                                 
053000       IF W6A162-NOK                                                      
053100       MOVE NEJ TO NYCKLAR-SW                                             
053200       END-IF                                                             
053300     END-IF                                                               
053400                                                                          
053500*    -- KONTROLL AV ADBUFFOMR                                             
053600     MOVE MFS-RENSA-FAELT TO MOD-ADBUFFOMR-IN                             
053700                                                                          
053800     IF MID-ADBUFFOMR-IN NOT = ALL '+'                                    
053900       MOVE '7'         TO MFS-IDPFK                                      
054000       MOVE SPACE       TO MFS-KDTRTYP                                    
054100       MOVE MID-ADBUFFOMR-IN   TO WS-ADBUFFOMR                            
054200     ELSE                                                                 
054300       IF MID-IDARTNR-IN NOT = ALL '+'                                    
054400         MOVE ZERO               TO WS-ADBUFFOMR                          
054500       ELSE                                                               
054600         MOVE MID-ADBUFFOMR-UT   TO WS-ADBUFFOMR                          
054700       END-IF                                                             
054800     END-IF                                                               
054900     INSPECT WS-ADBUFFOMR REPLACING LEADING SPACE BY ZERO                 
055000     IF WS-ADBUFFOMR NUMERIC                                              
055100       MOVE WS-ADBUFFOMR TO W-ADBUFFOMR                                   
055200                            W-ADBUFFOMR-MIN                               
055300                            WX-ADBUFFOMR                                  
055400       IF MID-ADBUFFOMR-IN = ALL '+'                                      
055500         MOVE 42              TO WY-ADBUFFOMR                             
055600                                 WX-ADBUFFOMR                             
055700         MOVE MID-ADBUFFOMR-UT TO WY-ADBUFFOMR                            
055800         INSPECT WY-ADBUFFOMR REPLACING LEADING SPACE BY ZERO             
055900         PERFORM S01-ADBUFFOMR-KOLL                                       
056000         IF BUFFOMR-FEL                                                   
056100           MOVE NEJ TO INDATA-SW                                          
056200                       NYCKLAR-SW                                         
056300         END-IF                                                           
056400       ELSE                                                               
056500         MOVE 42              TO WY-ADBUFFOMR                             
056600         PERFORM S01-ADBUFFOMR-KOLL                                       
056700         IF BUFFOMR-FEL                                                   
056800           MOVE NEJ TO INDATA-SW                                          
056900                       NYCKLAR-SW                                         
057000         END-IF                                                           
057100       END-IF                                                             
057200     ELSE                                                                 
057300       IF W6A162-NOK                                                      
057400       MOVE NEJ TO NYCKLAR-SW                                             
057500       END-IF                                                             
057600     END-IF                                                               
057700                                                                          
057800*    -- KONTROLL AV ADBUFFGANG                                            
057900     MOVE MFS-RENSA-FAELT TO MOD-ADBUFFGANG-IN                            
058000                                                                          
058100     IF MID-ADBUFFGANG-IN NOT = ALL '+'                                   
058200       MOVE '7'         TO MFS-IDPFK                                      
058300       MOVE SPACE       TO MFS-KDTRTYP                                    
058400       MOVE MID-ADBUFFGANG-IN TO WS-ADBUFFGANG                            
058500     ELSE                                                                 
058600       IF MID-IDARTNR-IN NOT = ALL '+'                                    
058700         MOVE ZERO              TO WS-ADBUFFGANG                          
058800       ELSE                                                               
058900         MOVE MID-ADBUFFGANG-UT TO WS-ADBUFFGANG                          
059000       END-IF                                                             
059100     END-IF                                                               
059200     INSPECT WS-ADBUFFGANG REPLACING LEADING SPACE BY ZERO                
059300     IF WS-ADBUFFGANG NUMERIC                                             
059400       MOVE WS-ADBUFFGANG TO W-ADBUFFGANG                                 
059500                             W-ADBUFFGANG-MIN                             
059600     ELSE                                                                 
059700       IF W6A162-NOK                                                      
059800       MOVE NEJ TO NYCKLAR-SW                                             
059900       END-IF                                                             
060000     END-IF                                                               
060100                                                                          
060200*    -- KONTROLL AV ADBUFFPL                                              
060300     MOVE MFS-RENSA-FAELT TO MOD-ADBUFFPL-IN                              
060400                                                                          
060500     IF MID-ADBUFFPL-IN NOT = ALL '+'                                     
060600       MOVE '7'         TO MFS-IDPFK                                      
060700       MOVE SPACE       TO MFS-KDTRTYP                                    
060800       MOVE MID-ADBUFFPL-IN TO WS-ADBUFFPL                                
060900     ELSE                                                                 
061000       IF MID-IDARTNR-IN NOT = ALL '+'                                    
061100         MOVE ZERO            TO WS-ADBUFFPL                              
061200       ELSE                                                               
061300         MOVE MID-ADBUFFPL-UT TO WS-ADBUFFPL                              
061400       END-IF                                                             
061500     END-IF                                                               
061600     INSPECT WS-ADBUFFPL REPLACING LEADING SPACE BY ZERO                  
061700     IF WS-ADBUFFPL NUMERIC                                               
061800       MOVE WS-ADBUFFPL TO W-ADBUFFPL                                     
061900                           W-ADBUFFPL-MIN                                 
062000     ELSE                                                                 
062100       IF W6A162-NOK                                                      
062200       MOVE NEJ TO NYCKLAR-SW                                             
062300       END-IF                                                             
062400     END-IF                                                               
062500                                                                          
062600     IF EGEN-MID OR NYCKLAR-OK                                            
062700       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
062800       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
062900       MOVE WS-KVBUFF           TO MOD-KVBUFF-UT                          
063000       INSPECT MOD-KVBUFF-UT REPLACING LEADING ZERO BY SPACE              
063100       MOVE WS-ADBUFFOMR        TO MOD-ADBUFFOMR-UT                       
063200       INSPECT MOD-ADBUFFOMR-UT REPLACING LEADING ZERO BY SPACE           
063300       MOVE WS-ADBUFFGANG       TO MOD-ADBUFFGANG-UT                      
063400       INSPECT MOD-ADBUFFGANG-UT REPLACING LEADING ZERO BY SPACE          
063500       MOVE WS-ADBUFFPL         TO MOD-ADBUFFPL-UT                        
063600       INSPECT MOD-ADBUFFPL-UT REPLACING LEADING ZERO BY SPACE            
063700     ELSE                                                                 
063800       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
063900                               MOD-KVBUFF-UT                              
064000                               MOD-ADBUFFOMR-UT                           
064100                               MOD-ADBUFFGANG-UT                          
064200                               MOD-ADBUFFPL-UT                            
064300     END-IF                                                               
064400                                                                          
064500     IF NYCKLAR-FEL                                                       
064600       MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                             
064700       CALL WMEDKONV USING MED-WMEDAREA                                   
064800       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
064900       IF BUFFOMR-FEL                                                     
065000         MOVE FEL-2(SPRAK-IX) TO MOD-TEMFSFEL                             
065100       END-IF                                                             
065200       PERFORM MFS-RENSA-FAELT-IN                                         
065300       PERFORM MFS-RENSA-FAELT-UT                                         
065400     END-IF                                                               
065500     .                                                                    
065600     EJECT                                                                
065700 C-FOERSTA-SIDA SECTION.                                                  
065800                                                                          
065900     PERFORM MFS-RENSA-FAELT-IN                                           
066000     IF MID-ADBUFFOMR-IN  = ALL '+' AND                                   
066100        MID-ADBUFFGANG-IN = ALL '+' AND                                   
066200        MID-ADBUFFPL-IN   = ALL '+'                                       
066300        MOVE ZERO     TO W-ADBUFFOMR-MIN                                  
066400                         W-DABUFPAF-MIN                                   
066500                         W-ADBUFFGANG-MIN                                 
066600                         W-ADBUFFPL-MIN                                   
066700     END-IF                                                               
066800     .                                                                    
066900     EJECT                                                                
067000 D-NAESTA-SIDA SECTION.                                                   
067100                                                                          
067200     PERFORM MFS-RENSA-FAELT-IN                                           
067300     IF EGEN-MID OR HELP-MID                                              
067400       MOVE MSGI-SPAR-AREA         TO W-MINKEY-WDD811KY                   
067500       IF W-MINKEY-IDTRANS = '6162'                                       
067600         MOVE W-MINKEY-ADBUFFOMR     TO W-ADBUFFOMR-MIN                   
067700         MOVE W-MINKEY-DABUFPAF      TO W-DABUFPAF-MIN                    
067800         MOVE W-MINKEY-ADBUFFGANG    TO W-ADBUFFGANG-MIN                  
067900         MOVE W-MINKEY-ADBUFFPL      TO W-ADBUFFPL-MIN                    
068000       ELSE                                                               
068100         MOVE ZERO                   TO W-ADBUFFOMR-MIN                   
068200                                        W-DABUFPAF-MIN                    
068300                                        W-ADBUFFGANG-MIN                  
068400                                        W-ADBUFFPL-MIN                    
068500       END-IF                                                             
068600     ELSE                                                                 
068700       MOVE ZERO                   TO W-ADBUFFOMR-MIN                     
068800                                      W-DABUFPAF-MIN                      
068900                                      W-ADBUFFGANG-MIN                    
069000                                      W-ADBUFFPL-MIN                      
069100     END-IF                                                               
069200     .                                                                    
069300     EJECT                                                                
069400 E-SAMMA-SIDA SECTION.                                                    
069500     IF EGEN-MID OR HELP-MID                                              
069600       MOVE MSGI-SPAR-AREA         TO W-WDD811KY-AREA                     
069700       MOVE W-WDD811KY-ENTER       TO W-MINKEY-WDD811KY                   
069800       IF W-MINKEY-IDTRANS = '6162'                                       
069900         MOVE W-MINKEY-ADBUFFOMR     TO W-ADBUFFOMR-MIN                   
070000         MOVE W-MINKEY-DABUFPAF      TO W-DABUFPAF-MIN                    
070100         MOVE W-MINKEY-ADBUFFGANG    TO W-ADBUFFGANG-MIN                  
070200         MOVE W-MINKEY-ADBUFFPL      TO W-ADBUFFPL-MIN                    
070300       ELSE                                                               
070400         MOVE ZERO                   TO W-ADBUFFOMR-MIN                   
070500                                        W-DABUFPAF-MIN                    
070600                                        W-ADBUFFGANG-MIN                  
070700                                        W-ADBUFFPL-MIN                    
070800       END-IF                                                             
070900       IF MID-INPUT-RAD = ALL '+' AND                                     
071000          MID-KDCMDVAL (1) = ALL '+' AND                                  
071100          MID-KDCMDVAL (2) = ALL '+' AND                                  
071200          MID-KDCMDVAL (3) = ALL '+' AND                                  
071300          MID-KDCMDVAL (4) = ALL '+' AND                                  
071400          MID-KDCMDVAL (5) = ALL '+' AND                                  
071500          MID-KDCMDVAL (6) = ALL '+' AND                                  
071600          MID-KDCMDVAL (7) = ALL '+' AND                                  
071700          MID-INPUT-SALDO (1) = ALL '+' AND                               
071800          MID-INPUT-SALDO (2) = ALL '+' AND                               
071900          MID-INPUT-SALDO (3) = ALL '+' AND                               
072000          MID-INPUT-SALDO (4) = ALL '+' AND                               
072100          MID-INPUT-SALDO (5) = ALL '+' AND                               
072200          MID-INPUT-SALDO (6) = ALL '+' AND                               
072300          MID-INPUT-SALDO (7) = ALL '+' AND                               
072400          MID-KDBRIST-IN      = ALL '+' AND                               
072500          MID-KDPAF-IN        = ALL '+'                                   
072600         PERFORM MFS-RENSA-FAELT-IN                                       
072700       ELSE                                                               
072800         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
072900         CALL WMEDKONV USING MED-WMEDAREA                                 
073000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
073100         PERFORM EA-MID-INDATA-TILL-MOD                                   
073200       END-IF                                                             
073300     ELSE                                                                 
073400       PERFORM MFS-RENSA-FAELT-IN                                         
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800 EA-MID-INDATA-TILL-MOD SECTION.                                          
073900                                                                          
074000     MOVE +1 TO RAD-IX                                                    
074100     PERFORM UNTIL RAD-IX > MAX-IX                                        
074200        IF MID-KDCMDVAL (RAD-IX)       = ALL '+'                          
074300           MOVE MFS-RENSA-FAELT         TO MOD-KDCMDVAL (RAD-IX)          
074400        ELSE                                                              
074500           MOVE MID-KDCMDVAL (RAD-IX)   TO MOD-KDCMDVAL (RAD-IX)          
074600           MOVE MFS-ADD-LAES-IN-FAELT   TO                                
074700                                   MOD-KDCMDVAL-ATTR (RAD-IX)             
074800        END-IF                                                            
074900        IF MID-ADBUFFOMR (RAD-IX)   = ALL '+'                             
075000           MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFOMR (RAD-IX)         
075100        ELSE                                                              
075200           MOVE MID-ADBUFFOMR (RAD-IX) TO MOD-ADBUFFOMR (RAD-IX)          
075300        END-IF                                                            
075400        IF MID-ADBUFFGANG (RAD-IX) = ALL '+'                              
075500           MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFGANG (RAD-IX)        
075600        ELSE                                                              
075700           MOVE MID-ADBUFFGANG (RAD-IX) TO MOD-ADBUFFGANG (RAD-IX)        
075800        END-IF                                                            
075900        IF MID-ADBUFFPL  (RAD-IX) = ALL '+'                               
076000           MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFPL  (RAD-IX)         
076100        ELSE                                                              
076200           MOVE MID-ADBUFFPL  (RAD-IX)  TO MOD-ADBUFFPL  (RAD-IX)         
076300        END-IF                                                            
076400        IF MID-DABUFPAF  (RAD-IX) = ALL '+'                               
076500           MOVE MFS-RENSA-FAELT         TO MOD-DABUFPAF  (RAD-IX)         
076600        ELSE                                                              
076700           MOVE MID-DABUFPAF  (RAD-IX)  TO MOD-DABUFPAF  (RAD-IX)         
076800        END-IF                                                            
076900        IF MID-KVBUFF-F-IN  (RAD-IX) = ALL '+'                            
077000           MOVE MFS-RENSA-FAELT         TO MOD-KVBUFF-F-IN(RAD-IX)        
077100        ELSE                                                              
077200           MOVE MID-KVBUFF-F-IN(RAD-IX) TO MOD-KVBUFF-F-IN(RAD-IX)        
077300           MOVE MFS-ADD-LAES-IN-FAELT   TO                                
077400                                   MOD-KVBUFF-F-IN-ATTR (RAD-IX)          
077500        END-IF                                                            
077600        IF MID-KVBUFF-OF-IN(RAD-IX) = ALL '+'                             
077700           MOVE MFS-RENSA-FAELT       TO MOD-KVBUFF-OF-IN(RAD-IX)         
077800        ELSE                                                              
077900           MOVE MID-KVBUFF-OF-IN (RAD-IX) TO                              
078000                                   MOD-KVBUFF-OF-IN(RAD-IX)               
078100           MOVE MFS-ADD-LAES-IN-FAELT  TO                                 
078200                                   MOD-KVBUFF-OF-IN-ATTR (RAD-IX)         
078300        END-IF                                                            
078400        IF MID-KVKOLLI-OF-IN (RAD-IX) = ALL '+'                           
078500           MOVE MFS-RENSA-FAELT    TO MOD-KVKOLLI-OF-IN(RAD-IX)           
078600        ELSE                                                              
078700           MOVE MID-KVKOLLI-OF-IN (RAD-IX) TO                             
078800                                   MOD-KVKOLLI-OF-IN  (RAD-IX)            
078900           MOVE MFS-ADD-LAES-IN-FAELT   TO                                
079000                                   MOD-KVKOLLI-OF-IN-ATTR(RAD-IX)         
079100        END-IF                                                            
079200        IF MID-KVKOLLI-F-IN (RAD-IX) = ALL '+'                            
079300           MOVE MFS-RENSA-FAELT    TO MOD-KVKOLLI-F-IN  (RAD-IX)          
079400        ELSE                                                              
079500           MOVE MID-KVKOLLI-F-IN (RAD-IX)  TO                             
079600                                    MOD-KVKOLLI-F-IN  (RAD-IX)            
079700           MOVE MFS-ADD-LAES-IN-FAELT   TO                                
079800                                    MOD-KVKOLLI-F-IN-ATTR(RAD-IX)         
079900        END-IF                                                            
080000        ADD +1     TO RAD-IX                                              
080100     END-PERFORM                                                          
080200     IF MID-ADBUFFOMR-UPD     = ALL '+'                                   
080300        MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFOMR-UPD                 
080400     ELSE                                                                 
080500        MOVE MID-ADBUFFOMR-UPD       TO MOD-ADBUFFOMR-UPD                 
080600        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-ADBUFFOMR-UPD-ATTR            
080700     END-IF                                                               
080800     IF MID-ADBUFFGANG-UPD   = ALL '+'                                    
080900        MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFGANG-UPD                
081000     ELSE                                                                 
081100        MOVE MID-ADBUFFGANG-UPD      TO MOD-ADBUFFGANG-UPD                
081200        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-ADBUFFGANG-UPD-ATTR           
081300     END-IF                                                               
081400     IF MID-ADBUFFPL-UPD    = ALL '+'                                     
081500        MOVE MFS-RENSA-FAELT         TO MOD-ADBUFFPL-UPD                  
081600     ELSE                                                                 
081700        MOVE MID-ADBUFFPL-UPD        TO MOD-ADBUFFPL-UPD                  
081800        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-ADBUFFPL-UPD-ATTR             
081900     END-IF                                                               
082000     .                                                                    
082100     EJECT                                                                
082200 F-LAES-VISA-INFO SECTION.                                                
082300                                                                          
082400     PERFORM FA-LAES-GRUNDDATA                                            
082500                                                                          
082600     IF SEGMENT-SAKNAS                                                    
082700        MOVE ERR-MISSING-IN-ARTREG  TO MED-IDMFSFEL                       
082800        CALL WMEDKONV USING MED-WMEDAREA                                  
082900        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
083000        PERFORM MFS-RENSA-FAELT-UT                                        
083100     ELSE                                                                 
083200        PERFORM IMS-GU-ARTD01                                             
083300        IF SEGMENT-SAKNAS                                                 
083400          MOVE ERR-MISSING-IN-BUFF-REG TO MED-IDMFSFEL                    
083500          CALL WMEDKONV USING MED-WMEDAREA                                
083600          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
083700          PERFORM MFS-RENSA-FAELT-UT-BUFFERT                              
083800        ELSE                                                              
083900           IF MFS-FIRST                                                   
084000             MOVE MFS-RENSA-FAELT     TO MOD-KDPAF-UT                     
084100                                         MOD-KDBRIST-UT                   
084200           ELSE                                                           
084300             MOVE MFS-ROER-EJ-FAELT   TO MOD-KDPAF-UT                     
084400                                         MOD-KDBRIST-UT                   
084500           END-IF                                                         
084600           MOVE +1      TO RAD-IX                                         
084700           PERFORM IMS-GNP-ARTD11                                         
084800           IF SEGMENT-FINNS                                               
084900              MOVE ARTD-SALDO-ADBUFFOMR   TO W-MINKEY-ADBUFFOMR           
085000              MOVE ARTD-SALDO-ADBUFFGANG  TO W-MINKEY-ADBUFFGANG          
085100              MOVE ARTD-SALDO-ADBUFFPL    TO W-MINKEY-ADBUFFPL            
085200              MOVE ARTD-SALDO-DABUFPAF    TO W-MINKEY-DABUFPAF            
085300              MOVE '6162'                 TO W-MINKEY-IDTRANS             
085400              MOVE W-MINKEY-WDD811KY      TO W-WDD811KY-ENTER             
085500*CURSOR TILL ARTNR ?                                                      
085600*             MOVE MFS-ADD-SAETT-CURSOR TO MOD-KDCMDVAL-ATTR (1)          
085700           ELSE                                                           
085800              MOVE SPACE                  TO W-WDD811KY-ENTER             
085900           END-IF                                                         
086000           PERFORM UNTIL SEGMENT-SAKNAS OR RAD-IX > MAX-IX                
086100              IF ARTD-SALDO-ADBUFFOMR = 01                                
086200                MOVE ARTD-SALDO-KDPAF     TO MOD-KDPAF-UT                 
086300                MOVE ARTD-SALDO-KDBRIST   TO MOD-KDBRIST-UT               
086400              END-IF                                                      
086500              PERFORM FB-RADINFO                                          
086600              PERFORM FC-SUMMA-RAD                                        
086700              ADD +1      TO RAD-IX                                       
086800              PERFORM IMS-GNP-ARTD11                                      
086900           END-PERFORM                                                    
087000           IF SEGMENT-FINNS                                               
087100              MOVE ARTD-SALDO-ADBUFFOMR   TO W-MINKEY-ADBUFFOMR           
087200              MOVE ARTD-SALDO-ADBUFFGANG  TO W-MINKEY-ADBUFFGANG          
087300              MOVE ARTD-SALDO-ADBUFFPL    TO W-MINKEY-ADBUFFPL            
087400              MOVE ARTD-SALDO-DABUFPAF    TO W-MINKEY-DABUFPAF            
087500              MOVE '6162'                 TO W-MINKEY-IDTRANS             
087600              MOVE W-WDD811KY-AREA        TO MSGI-SPAR-AREA               
087700              IF MED-IDMFSINF = SPACE                                     
087800                MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF               
087900                CALL WMEDKONV USING MED-WMEDAREA                          
088000                MOVE MED-TEMFSINF           TO MOD-TEMFSINF               
088100              END-IF                                                      
088200              PERFORM UNTIL SEGMENT-SAKNAS                                
088300                PERFORM FC-SUMMA-RAD                                      
088400                PERFORM IMS-GNP-ARTD11                                    
088500              END-PERFORM                                                 
088600           ELSE                                                           
088700              MOVE W-WDD811KY-ENTER       TO W-MINKEY-WDD811KY            
088800              MOVE W-WDD811KY-AREA        TO MSGI-SPAR-AREA               
088900              IF MED-IDMFSINF = SPACE                                     
089000                MOVE INF-LAST-PAGE          TO MED-IDMFSINF               
089100                CALL WMEDKONV USING MED-WMEDAREA                          
089200                MOVE MED-TEMFSINF           TO MOD-TEMFSINF               
089300              END-IF                                                      
089400           END-IF                                                         
089500           MOVE '002'                TO MSGI-KDCALL                       
089600           MOVE '6162'               TO MSGI-IDTRANS                      
089700           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
089800           IF MFS-UPDATE                                                  
089900             PERFORM IMS-GU-ARTD01                                        
090000             MOVE ZERO              TO W-ADBUFFOMR-MIN                    
090100                                       W-ADBUFFGANG-MIN                   
090200                                       W-ADBUFFPL-MIN                     
090300             PERFORM IMS-GNP-ARTD11                                       
090400             MOVE ZERO              TO WS-SUBUFF-F                        
090500                                       WS-SUBUFF-OF                       
090600                                       WS-SUKOLLI-F                       
090700                                       WS-SUKOLLI-OF                      
090800             PERFORM UNTIL SEGMENT-SAKNAS                                 
090900               PERFORM FC-SUMMA-RAD                                       
091000               PERFORM IMS-GNP-ARTD11                                     
091100             END-PERFORM                                                  
091200             MOVE WS-SUBUFF-F       TO MOD-SUBUFF-F                       
091300             MOVE WS-SUBUFF-OF      TO MOD-SUBUFF-OF                      
091400             MOVE WS-SUKOLLI-F      TO MOD-SUKOLLI-F                      
091500             MOVE WS-SUKOLLI-OF     TO MOD-SUKOLLI-OF                     
091600           ELSE                                                           
091700             IF MFS-NEXT OR MFS-ENTER                                     
091800               MOVE MFS-ROER-EJ-FAELT TO MOD-SUBUFF-F                     
091900                                         MOD-SUBUFF-OF                    
092000                                         MOD-SUKOLLI-F                    
092100                                         MOD-SUKOLLI-OF                   
092200             ELSE                                                         
092300               MOVE WS-SUBUFF-F       TO MOD-SUBUFF-F                     
092400               MOVE WS-SUBUFF-OF      TO MOD-SUBUFF-OF                    
092500               MOVE WS-SUKOLLI-F      TO MOD-SUKOLLI-F                    
092600               MOVE WS-SUKOLLI-OF     TO MOD-SUKOLLI-OF                   
092700             END-IF                                                       
092800           END-IF                                                         
092900        END-IF                                                            
093000     END-IF                                                               
093100     .                                                                    
093200     EJECT                                                                
093300 FA-LAES-GRUNDDATA SECTION.                                               
093400                                                                          
093500     PERFORM IMS-GU-ARTC11                                                
093600     IF SEGMENT-FINNS                                                     
093700        MOVE ARTC-CLAG-ADLAGOMR     TO MOD-ADLAGOMR                       
093800        MOVE ARTC-CLAG-ADGANG       TO MOD-ADGANG                         
093900        MOVE ARTC-CLAG-ADPLATS      TO MOD-ADPLATS                        
094000        IF ARTC-CLAG-ADLAGOMR-SVS > ZERO       OR                         
094100           ARTC-CLAG-ADLAGOMR-CD(1) > ZERO                                
094200          MOVE JA                   TO MOD-FLERPL                         
094300        ELSE                                                              
094400          MOVE NEJ                  TO MOD-FLERPL                         
094500        END-IF                                                            
094600        MOVE ARTC-CLAG-KVLS         TO MOD-KVLS                           
              MOVE ARTC-CLAG-KVQPACK-3    TO MOD-KVQPACK-3                      
094700        PERFORM IMS-GU-BENA11                                             
094800        IF SEGMENT-FINNS                                                  
094900          MOVE BENA-TEXT-BEART      TO MOD-BEART                          
095000        ELSE                                                              
095100          MOVE MFS-RENSA-FAELT      TO MOD-BEART                          
095200        END-IF                                                            
095300                                                                          
095400     END-IF                                                               
095500     .                                                                    
095600     EJECT                                                                
095700 FB-RADINFO        SECTION.                                               
095800                                                                          
095900     MOVE ARTD-SALDO-ADBUFFOMR   TO MOD-ADBUFFOMR (RAD-IX)                
096000                                    BATTERY-SW                            
096100     MOVE ARTD-SALDO-ADBUFFGANG  TO MOD-ADBUFFGANG (RAD-IX)               
096200     MOVE ARTD-SALDO-ADBUFFPL    TO MOD-ADBUFFPL  (RAD-IX)                
096300     IF BATTERY-LOC                                                       
096400        MOVE ARTD-SALDO-DABUFPAF    TO MOD-DABUFPAF  (RAD-IX)             
096500     ELSE                                                                 
096600        MOVE ARTD-SALDO-DABUFPAF   TO MOD-DABUFPAF  (RAD-IX)              
096700        MOVE MFS-CLOSE-NONDISP-FIELD                                      
096800                          TO MOD-DABUFPAF-ATTR  (RAD-IX)                  
096900     END-IF                                                               
097000                                                                          
097100     MOVE ARTD-SALDO-KVBUFF-F    TO MOD-KVBUFF-F  (RAD-IX)                
097200     MOVE ARTD-SALDO-KVBUFF-OF   TO MOD-KVBUFF-OF (RAD-IX)                
097300     MOVE ARTD-SALDO-KVKOLLI-F   TO MOD-KVKOLLI-F (RAD-IX)                
097400     MOVE ARTD-SALDO-KVKOLLI-OF  TO MOD-KVKOLLI-OF(RAD-IX)                
097500                                                                          
097600     .                                                                    
097700     EJECT                                                                
097800 FC-SUMMA-RAD SECTION.                                                    
097900                                                                          
098000     ADD  ARTD-SALDO-KVBUFF-F    TO WS-SUBUFF-F                           
098100     ADD  ARTD-SALDO-KVBUFF-OF   TO WS-SUBUFF-OF                          
098200     ADD  ARTD-SALDO-KVKOLLI-F   TO WS-SUKOLLI-F                          
098300     ADD  ARTD-SALDO-KVKOLLI-OF  TO WS-SUKOLLI-OF                         
098400     .                                                                    
098500     EJECT                                                                
098600 G-KOLLA-INPUT SECTION.                                                   
098700                                                                          
098800     MOVE JA  TO INDATA-SW                                                
098900     IF MID-KDCMDVAL (1) = ALL '+' AND                                    
099000        MID-KDCMDVAL (2) = ALL '+' AND                                    
099100        MID-KDCMDVAL (3) = ALL '+' AND                                    
099200        MID-KDCMDVAL (4) = ALL '+' AND                                    
099300        MID-KDCMDVAL (5) = ALL '+' AND                                    
099400        MID-KDCMDVAL (6) = ALL '+' AND                                    
099500        MID-KDCMDVAL (7) = ALL '+' AND                                    
099600        MID-INPUT-SALDO (1) = ALL '+' AND                                 
099700        MID-INPUT-SALDO (2) = ALL '+' AND                                 
099800        MID-INPUT-SALDO (3) = ALL '+' AND                                 
099900        MID-INPUT-SALDO (4) = ALL '+' AND                                 
100000        MID-INPUT-SALDO (5) = ALL '+' AND                                 
100100        MID-INPUT-SALDO (6) = ALL '+' AND                                 
100200        MID-INPUT-SALDO (7) = ALL '+' AND                                 
100300        MID-KDBRIST-IN      = ALL '+' AND                                 
100400        MID-KDPAF-IN        = ALL '+' AND                                 
100500        MID-INPUT-RAD   = ALL '+'     AND                                 
100600        MID-KVANTAL-UPD = ALL '+'  AND                                    
100700       (WS-KVBUFF           = ZERO    OR                                  
100800        WS-ADBUFFOMR        = ZERO    OR                                  
100900        WS-ADBUFFPL         = ZERO)                                       
101000       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
101100       CALL WMEDKONV USING MED-WMEDAREA                                   
101200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
101300       PERFORM MFS-RENSA-FAELT-IN                                         
101400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
101500       MOVE NEJ TO INDATA-SW                                              
101600     ELSE                                                                 
101700       IF MID-KDBRIST-IN NOT = ALL '+'                                    
101800          IF MID-KDBRIST-IN = 0 OR 1                                      
101900            MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBRIST-IN-ATTR               
102000          ELSE                                                            
102100            MOVE MFS-NUM-FAELT-FEL TO MOD-KDBRIST-IN-ATTR                 
102200            MOVE NEJ TO INDATA-SW                                         
102300          END-IF                                                          
102400       ELSE                                                               
102500          MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBRIST-IN-ATTR                 
102600       END-IF                                                             
102700                                                                          
102800       IF MID-KDPAF-IN NOT = ALL '+'                                      
102900          IF MID-KDPAF-IN = 0 OR 1                                        
103000            MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPAF-IN-ATTR                 
103100          ELSE                                                            
103200            MOVE MFS-NUM-FAELT-FEL TO MOD-KDPAF-IN-ATTR                   
103300            MOVE NEJ TO INDATA-SW                                         
103400          END-IF                                                          
103500       ELSE                                                               
103600          MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPAF-IN-ATTR                   
103700       END-IF                                                             
103800                                                                          
103900       MOVE SPACE TO MED-IDMFSFEL                                         
104000                                                                          
104100       IF MID-KVANTAL-UPD = ALL '+'                                       
104200       PERFORM GA-KOLLA-RADER                                             
104300       IF MID-INPUT-RAD NOT = ALL '+'                                     
104400         PERFORM GB-KOLLA-INPUT-RAD                                       
104500         IF INDATA-OK                                                     
104600           PERFORM GC-KOLLA-KONFLIKT                                      
104700         END-IF                                                           
104800       END-IF                                                             
104900       else                                                               
105000        IF MID-KVANTAL-UPD = ALL '+'                                      
105100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVANTAL-UPD-ATTR                 
105200        ELSE                                                              
105300        IF MID-KVANTAL-UPD NUMERIC                                        
105400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVANTAL-UPD-ATTR                
               MOVE JA TO SYNQ-CALL                                             
105600        ELSE                                                              
105700         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KVANTAL-UPD-ATTR                
105800         MOVE NEJ TO INDATA-SW                                            
105900        END-IF                                                            
106000        END-IF                                                            
106100                                                                          
106200        IF MID-TREATED-UPD = ALL '+'                                      
106300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TREATED-UPD-ATTR                
106400        ELSE                                                              
106500         IF MID-TREATED-UPD = 'J' OR 'O' OR 'N' OR 'F' OR 'Y'             
106600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TREATED-UPD-ATTR                
               MOVE JA TO SYNQ-CALL                                             
106800         ELSE                                                             
106900         MOVE MFS-ALFA-FAELT-FEL   TO MOD-TREATED-UPD-ATTR                
107000         MOVE NEJ TO INDATA-SW                                            
107100         END-IF                                                           
107200         END-IF                                                           
107300                                                                          
107400         IF MID-LOCATION-UPD = ALL '+'                                    
                IF SYNQ-OK                                                      
                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-LOCATION-UPD-ATTR             
                 MOVE NEJ TO INDATA-SW                                          
                ELSE                                                            
107500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-LOCATION-UPD-ATTR             
                END-IF                                                          
107600         ELSE                                                             
107700         IF MID-LOCATION-UPD =  'UT5 ' OR 'UT6 ' OR                       
107800                                'UT8 ' OR 'UT9 ' OR 'EMIL' OR             
107900                                'GROV' OR 'UT7 ' OR 'QC' OR               
108000                                'FB' OR 'VOL' OR '72' OR                  
108100                                'VOR' OR 'FLYG' OR 'FBG' OR               
108200                                'SKROT' OR 'USA' OR 'REF' OR              
108300                                'OMP' OR 'RD' OR 'VOK' OR                 
108400                                'HL' OR 'PLOCKORDER' OR                   
108500                                'REJECT_UT4' OR 'VOM' OR                  
                                      'VOSHF' OR 'NYX'                          
108600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-LOCATION-UPD-ATTR               
108700         MOVE JA TO SYNQ-CALL                                             
108800         ELSE                                                             
108900         MOVE MFS-ALFA-FAELT-FEL   TO MOD-LOCATION-UPD-ATTR               
109000         MOVE NEJ TO INDATA-SW                                            
109100         END-IF                                                           
109200         END-IF                                                           
109300       END-IF                                                             
109400       IF INDATA-FEL                                                      
109500         IF MED-IDMFSFEL = SPACE                                          
109600           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
109700         END-IF                                                           
109800         CALL WMEDKONV USING MED-WMEDAREA                                 
109900         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
110000         PERFORM MFS-ROER-EJ-FAELT-UT                                     
110100         PERFORM MFS-ROER-EJ-FAELT-IN                                     
110200       ELSE                                                               
110300         PERFORM GD-KOLL-MOT-DB                                           
110400         IF INDATA-FEL                                                    
110500           IF MED-IDMFSFEL = SPACE                                        
110600             MOVE ERR-MISSING-IN-REG   TO MED-IDMFSFEL                    
110700           END-IF                                                         
110800           CALL WMEDKONV USING MED-WMEDAREA                               
110900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
111000           IF BUFFOMR-FEL                                                 
111100             MOVE FEL-1(SPRAK-IX)  TO MOD-TEMFSFEL                        
111200           END-IF                                                         
111300           PERFORM MFS-ROER-EJ-FAELT-IN                                   
111400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
111500*          PERFORM MFS-LAES-IN-IGEN                                       
111600**ANNARS FUNGERAR EJ UPPLYSTA FÄLT                                        
111700         END-IF                                                           
111800       END-IF                                                             
111900     END-IF                                                               
112000     .                                                                    
112100     EJECT                                                                
112200 GA-KOLLA-RADER      SECTION.                                             
112300                                                                          
112400     MOVE +1      TO RAD-IX                                               
112500     PERFORM UNTIL RAD-IX > MAX-IX                                        
112600        IF MID-KDCMDVAL (RAD-IX) = ALL '+'                                
112700          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR(RAD-IX)          
112800        ELSE                                                              
112900          IF MID-KDCMDVAL(RAD-IX) = 'UT ' OR 'IN ' OR 'B  '               
113000                                 OR 'U  ' OR 'I  '                        
113100            MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMDVAL-ATTR(RAD-IX)        
113200          ELSE                                                            
113300            MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(RAD-IX)        
113400            MOVE NEJ TO INDATA-SW                                         
113500          END-IF                                                          
113600        END-IF                                                            
113700        PERFORM GAA-KOLLA-ANTAL                                           
113800        ADD +1 TO RAD-IX                                                  
113900     END-PERFORM                                                          
114000                                                                          
114100     .                                                                    
114200     EJECT                                                                
114300 GAA-KOLLA-ANTAL     SECTION.                                             
114400                                                                          
114500     IF MID-INPUT-SALDO (RAD-IX) NOT = ALL '+'                            
114600       IF MID-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'                          
114700         IF MID-KVBUFF-F-IN (RAD-IX) NUMERIC                              
114800           MOVE MFS-NUM-FAELT-RAETT TO                                    
114900                         MOD-KVBUFF-F-IN-ATTR(RAD-IX)                     
115000         ELSE                                                             
115100           MOVE MFS-NUM-FAELT-FEL   TO                                    
115200                         MOD-KVBUFF-F-IN-ATTR(RAD-IX)                     
115300           MOVE NEJ TO INDATA-SW                                          
115400         END-IF                                                           
115500       ELSE                                                               
115600         MOVE MFS-NUM-FAELT-RAETT TO                                      
115700                       MOD-KVBUFF-F-IN-ATTR(RAD-IX)                       
115800       END-IF                                                             
115900       IF MID-KVBUFF-OF-IN (RAD-IX) NOT = ALL '+'                         
116000         IF MID-KVBUFF-OF-IN (RAD-IX) NUMERIC                             
116100           MOVE MFS-NUM-FAELT-RAETT TO                                    
116200                         MOD-KVBUFF-OF-IN-ATTR(RAD-IX)                    
116300         ELSE                                                             
116400           MOVE MFS-NUM-FAELT-FEL   TO                                    
116500                         MOD-KVBUFF-OF-IN-ATTR(RAD-IX)                    
116600           MOVE NEJ TO INDATA-SW                                          
116700         END-IF                                                           
116800       ELSE                                                               
116900         MOVE MFS-NUM-FAELT-RAETT TO                                      
117000                       MOD-KVBUFF-OF-IN-ATTR(RAD-IX)                      
117100       END-IF                                                             
117200       IF MID-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'                         
117300         IF MID-KVKOLLI-F-IN (RAD-IX) NUMERIC                             
117400           MOVE MFS-NUM-FAELT-RAETT TO                                    
117500                         MOD-KVKOLLI-F-IN-ATTR(RAD-IX)                    
117600         ELSE                                                             
117700           MOVE MFS-NUM-FAELT-FEL   TO                                    
117800                         MOD-KVKOLLI-F-IN-ATTR(RAD-IX)                    
117900           MOVE NEJ TO INDATA-SW                                          
118000         END-IF                                                           
118100       ELSE                                                               
118200         MOVE MFS-NUM-FAELT-RAETT TO                                      
118300                       MOD-KVKOLLI-F-IN-ATTR(RAD-IX)                      
118400       END-IF                                                             
118500       IF MID-KVKOLLI-OF-IN (RAD-IX) NOT = ALL '+'                        
118600         IF MID-KVKOLLI-OF-IN (RAD-IX) NUMERIC                            
118700           MOVE MFS-NUM-FAELT-RAETT TO                                    
118800                         MOD-KVKOLLI-OF-IN-ATTR(RAD-IX)                   
118900         ELSE                                                             
119000           MOVE MFS-NUM-FAELT-FEL   TO                                    
119100                         MOD-KVKOLLI-OF-IN-ATTR(RAD-IX)                   
119200           MOVE NEJ TO INDATA-SW                                          
119300         END-IF                                                           
119400       ELSE                                                               
119500         MOVE MFS-NUM-FAELT-RAETT TO                                      
119600                       MOD-KVKOLLI-OF-IN-ATTR(RAD-IX)                     
119700       END-IF                                                             
119800       IF MID-KDCMDVAL (RAD-IX) = 'B  ' OR '+++'                          
119900         MOVE MFS-ALFA-FAELT-FEL   TO                                     
120000                       MOD-KDCMDVAL-ATTR(RAD-IX)                          
120100         MOVE MFS-NUM-FAELT-FEL   TO                                      
120200                       MOD-KVBUFF-F-IN-ATTR(RAD-IX)                       
120300                       MOD-KVBUFF-OF-IN-ATTR(RAD-IX)                      
120400                       MOD-KVKOLLI-F-IN-ATTR(RAD-IX)                      
120500                       MOD-KVKOLLI-OF-IN-ATTR(RAD-IX)                     
120600         MOVE NEJ TO INDATA-SW                                            
120700         MOVE ERR-CONFLICT-FLDS TO MED-IDMFSFEL                           
120800       END-IF                                                             
120900     ELSE                                                                 
121000       IF MID-KDCMDVAL (RAD-IX) = 'IN ' OR 'UT '                          
121100                               OR 'I  ' OR 'U  '                          
121200         IF WS-KVBUFF NUMERIC                                             
121300           CONTINUE                                                       
121400         ELSE                                                             
121500           MOVE MFS-ALFA-FAELT-FEL   TO                                   
121600                         MOD-KDCMDVAL-ATTR(RAD-IX)                        
121700           MOVE NEJ TO INDATA-SW                                          
121800         END-IF                                                           
121900       END-IF                                                             
122000     END-IF                                                               
122100     .                                                                    
122200     EJECT                                                                
122300 GB-KOLLA-INPUT-RAD  SECTION.                                             
122400                                                                          
122500     IF MID-ADBUFFOMR-UPD = ALL '+'                                       
122600       IF MID-ADBUFFGANG-UPD = ALL '+' AND                                
122700          MID-ADBUFFPL-UPD = ALL '+'                                      
122800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFOMR-UPD-ATTR              
122900       ELSE                                                               
123000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADBUFFOMR-UPD-ATTR              
123100                                      MOD-ADBUFFGANG-UPD-ATTR             
123200                                      MOD-ADBUFFPL-UPD-ATTR               
123300         MOVE NEJ TO INDATA-SW                                            
123400       END-IF                                                             
123500     ELSE                                                                 
123600       IF MID-ADBUFFOMR-UPD NUMERIC AND                                   
123700          MID-ADBUFFOMR-UPD > ZERO                                        
123800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFOMR-UPD-ATTR              
123900       ELSE                                                               
124000         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADBUFFOMR-UPD-ATTR              
124100         MOVE NEJ TO INDATA-SW                                            
124200       END-IF                                                             
124300     END-IF                                                               
124400                                                                          
124500     IF MID-ADBUFFGANG-UPD = ALL '+'                                      
124600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFGANG-UPD-ATTR             
124700     ELSE                                                                 
124800       IF MID-ADBUFFGANG-UPD NUMERIC                                      
124900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFGANG-UPD-ATTR             
125000       ELSE                                                               
125100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADBUFFGANG-UPD-ATTR             
125200         MOVE NEJ TO INDATA-SW                                            
125300       END-IF                                                             
125400     END-IF                                                               
125500                                                                          
125600     IF MID-ADBUFFPL-UPD = ALL '+'                                        
125700         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFPL-UPD-ATTR               
125800     ELSE                                                                 
125900       IF MID-ADBUFFPL-UPD NUMERIC                                        
126000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADBUFFPL-UPD-ATTR               
126100       ELSE                                                               
126200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADBUFFPL-UPD-ATTR               
126300         MOVE NEJ TO INDATA-SW                                            
126400       END-IF                                                             
126500     END-IF                                                               
126600                                                                          
126700     IF MID-KVBUFF-F-UPD = ALL '+'                                        
126800         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVBUFF-F-UPD-ATTR               
126900     ELSE                                                                 
127000       IF MID-KVBUFF-F-UPD NUMERIC                                        
127100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVBUFF-F-UPD-ATTR               
127200       ELSE                                                               
127300         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KVBUFF-F-UPD-ATTR               
127400         MOVE NEJ TO INDATA-SW                                            
127500       END-IF                                                             
127600     END-IF                                                               
127700                                                                          
127800     IF MID-KVKOLLI-F-UPD = ALL '+'                                       
127900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVKOLLI-F-UPD-ATTR              
128000     ELSE                                                                 
128100       IF MID-KVKOLLI-F-UPD NUMERIC                                       
128200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVKOLLI-F-UPD-ATTR              
128300       ELSE                                                               
128400         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KVKOLLI-F-UPD-ATTR              
128500         MOVE NEJ TO INDATA-SW                                            
128600       END-IF                                                             
128700     END-IF                                                               
128800                                                                          
128900     IF MID-KVBUFF-OF-UPD = ALL '+'                                       
129000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVBUFF-OF-UPD-ATTR              
129100     ELSE                                                                 
129200       IF MID-KVBUFF-OF-UPD NUMERIC                                       
129300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVBUFF-OF-UPD-ATTR              
129400       ELSE                                                               
129500         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KVBUFF-OF-UPD-ATTR              
129600         MOVE NEJ TO INDATA-SW                                            
129700       END-IF                                                             
129800     END-IF                                                               
129900                                                                          
130000     IF MID-KVKOLLI-OF-UPD = ALL '+'                                      
130100         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVKOLLI-OF-UPD-ATTR             
130200     ELSE                                                                 
130300       IF MID-KVKOLLI-OF-UPD NUMERIC                                      
130400         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVKOLLI-OF-UPD-ATTR             
130500       ELSE                                                               
130600         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KVKOLLI-OF-UPD-ATTR             
130700         MOVE NEJ TO INDATA-SW                                            
130800       END-IF                                                             
130900     END-IF                                                               
131000                                                                          
131100                                                                          
131200     IF MID-ADBUFFOMR-UPD = ALL    '+'                                    
131300       IF MID-KVBUFF-F-UPD = ALL   '+' AND                                
131400          MID-KVBUFF-OF-UPD = ALL  '+' AND                                
131500          MID-KVKOLLI-F-UPD = ALL  '+' AND                                
131600          MID-KVKOLLI-OF-UPD = ALL '+'                                    
131700         CONTINUE                                                         
131800       ELSE                                                               
131900         MOVE MFS-ALFA-FAELT-FEL   TO MOD-ADBUFFOMR-UPD-ATTR              
132000                                      MOD-ADBUFFGANG-UPD-ATTR             
132100                                      MOD-ADBUFFPL-UPD-ATTR               
132200         MOVE NEJ TO INDATA-SW                                            
132300       END-IF                                                             
132400     END-IF                                                               
132500     .                                                                    
132600     EJECT                                                                
132700 GC-KOLLA-KONFLIKT SECTION.                                               
132800                                                                          
132900     MOVE +1      TO RAD-IX                                               
133000     PERFORM UNTIL RAD-IX > MAX-IX                                        
133100        IF MID-KDCMDVAL (RAD-IX) = ALL '+'                                
133200          CONTINUE                                                        
133300        ELSE                                                              
133400          MOVE ERR-CONFLICT-FLDS    TO MED-IDMFSFEL                       
133500          MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(RAD-IX)          
133600                                       MOD-ADBUFFOMR-UPD-ATTR             
133700                                       MOD-ADBUFFGANG-UPD-ATTR            
133800                                       MOD-ADBUFFPL-UPD-ATTR              
133900                                       MOD-KVBUFF-F-UPD-ATTR              
134000                                       MOD-KVBUFF-OF-UPD-ATTR             
134100                                       MOD-KVKOLLI-F-UPD-ATTR             
134200                                       MOD-KVKOLLI-OF-UPD-ATTR            
134300          MOVE NEJ TO INDATA-SW                                           
134400        END-IF                                                            
134500        ADD +1 TO RAD-IX                                                  
134600     END-PERFORM                                                          
134700     .                                                                    
134800     EJECT                                                                
134900 GD-KOLL-MOT-DB      SECTION.                                             
135000                                                                          
135100     PERFORM IMS-GU-ARTC11                                                
135200     IF SEGMENT-SAKNAS                                                    
135300        MOVE NEJ TO INDATA-SW                                             
135400        MOVE ERR-MISSING-IN-ARTREG  TO MED-IDMFSFEL                       
135500     END-IF                                                               
135600                                                                          
135700     PERFORM GDA-UPD-FRAN-NYCKELRAD                                       
135800     MOVE +1      TO RAD-IX                                               
135900     PERFORM UNTIL RAD-IX > MAX-IX                                        
136000        IF MID-KDCMDVAL (RAD-IX) = ALL '+'                                
136100          CONTINUE                                                        
136200        ELSE                                                              
136300          INSPECT MID-ADBUFFOMR (RAD-IX)                                  
136400                                   REPLACING LEADING SPACE BY ZERO        
136500          MOVE MID-ADBUFFOMR (RAD-IX) TO W-ADBUFFOMR                      
136600                                         WX-ADBUFFOMR                     
136700                                         WY-ADBUFFOMR                     
136800          PERFORM S01-ADBUFFOMR-KOLL                                      
136900          INSPECT MID-ADBUFFGANG (RAD-IX)                                 
137000                                 REPLACING LEADING SPACE BY ZERO          
137100          MOVE MID-ADBUFFGANG (RAD-IX)   TO W-ADBUFFGANG                  
137200          INSPECT MID-ADBUFFPL (RAD-IX)                                   
137300                                REPLACING LEADING SPACE BY ZERO           
137400          MOVE MID-ADBUFFPL (RAD-IX)    TO W-ADBUFFPL                     
137500          INSPECT MID-DABUFPAF (RAD-IX)                                   
137600                                REPLACING LEADING SPACE BY ZERO           
137700          MOVE MID-DABUFPAF (RAD-IX)    TO W-DABUFPAF                     
137800          PERFORM IMS-GU-ARTD11                                           
137900          IF SEGMENT-SAKNAS                                               
138000            MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMDVAL-ATTR(RAD-IX)        
138100            MOVE NEJ TO INDATA-SW                                         
138200          ELSE                                                            
138300            IF MID-KDCMDVAL (RAD-IX) = 'UT ' OR 'U  '                     
138400              IF MID-INPUT-SALDO (RAD-IX) NOT = ALL '+'                   
138500                IF MID-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'                 
138600                   MOVE MID-KVBUFF-F-IN (RAD-IX) TO WS-ANTAL              
138700                   IF WS-ANTAL        > ARTD-SALDO-KVBUFF-F               
138800                     MOVE MFS-ALFA-FAELT-FEL TO                           
138900                                     MOD-KDCMDVAL-ATTR(RAD-IX)            
139000                                     MOD-KVBUFF-F-IN-ATTR(RAD-IX)         
139100                     MOVE NEJ TO INDATA-SW                                
139200                     MOVE ERR-CONFLICT-FLDS    TO MED-IDMFSFEL            
139300                   END-IF                                                 
139400                END-IF                                                    
139500                IF MID-KVBUFF-OF-IN (RAD-IX) NOT = ALL '+'                
139600                   MOVE MID-KVBUFF-OF-IN (RAD-IX) TO WS-ANTAL             
139700                   IF WS-ANTAL         > ARTD-SALDO-KVBUFF-OF             
139800                     MOVE MFS-ALFA-FAELT-FEL TO                           
139900                                   MOD-KDCMDVAL-ATTR(RAD-IX)              
140000                                   MOD-KVBUFF-OF-IN-ATTR(RAD-IX)          
140100                     MOVE NEJ TO INDATA-SW                                
140200                     MOVE ERR-CONFLICT-FLDS    TO MED-IDMFSFEL            
140300                   END-IF                                                 
140400                END-IF                                                    
140500                IF MID-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'                
140600                   MOVE MID-KVKOLLI-F-IN (RAD-IX) TO WS-ANTAL             
140700                   IF WS-ANTAL         > ARTD-SALDO-KVKOLLI-F             
140800                     MOVE MFS-ALFA-FAELT-FEL TO                           
140900                                   MOD-KDCMDVAL-ATTR(RAD-IX)              
141000                                   MOD-KVKOLLI-F-IN-ATTR(RAD-IX)          
141100                     MOVE NEJ TO INDATA-SW                                
141200                     MOVE ERR-CONFLICT-FLDS    TO MED-IDMFSFEL            
141300                   END-IF                                                 
141400                END-IF                                                    
141500                IF MID-KVKOLLI-OF-IN (RAD-IX) NOT = ALL '+'               
141600                   MOVE MID-KVKOLLI-OF-IN (RAD-IX) TO WS-ANTAL            
141700                   IF WS-ANTAL          > ARTD-SALDO-KVKOLLI-OF           
141800                     MOVE MFS-ALFA-FAELT-FEL TO                           
141900                                   MOD-KDCMDVAL-ATTR(RAD-IX)              
142000                                   MOD-KVKOLLI-OF-IN-ATTR(RAD-IX)         
142100                     MOVE NEJ TO INDATA-SW                                
142200                     MOVE ERR-CONFLICT-FLDS    TO MED-IDMFSFEL            
142300                   END-IF                                                 
142400                END-IF                                                    
142500              ELSE                                                        
142600                MOVE WS-KVBUFF TO WS-ANTAL                                
142700                IF WS-ANTAL  > ARTD-SALDO-KVBUFF-F                        
142800                  MOVE MFS-ALFA-FAELT-FEL TO                              
142900                                MOD-KDCMDVAL-ATTR(RAD-IX)                 
143000                  MOVE NEJ TO INDATA-SW                                   
143100                  MOVE ERR-CONFLICT-FLDS    TO MED-IDMFSFEL               
143200                END-IF                                                    
143300              END-IF                                                      
143400            END-IF                                                        
143500          END-IF                                                          
143600        END-IF                                                            
143700        ADD +1 TO RAD-IX                                                  
143800     END-PERFORM                                                          
143900     IF MID-KDPAF-IN = ALL '+' AND                                        
144000        MID-KDBRIST-IN = ALL '+'                                          
144100       CONTINUE                                                           
144200     ELSE                                                                 
144300       MOVE +1       TO W-ADBUFFOMR                                       
144400       MOVE ZERO     TO W-ADBUFFGANG                                      
144500                        W-ADBUFFPL                                        
144600       PERFORM IMS-GU-ARTD11                                              
144700       IF SEGMENT-SAKNAS                                                  
144800         IF MID-INPUT-RAD = ALL '+'                                       
144900           IF MID-KDBRIST-IN NOT = ALL '+'                                
145000             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDBRIST-IN-ATTR             
145100           ELSE                                                           
145200             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDPAF-IN-ATTR               
145300           END-IF                                                         
145400           MOVE NEJ TO INDATA-SW                                          
145500         ELSE                                                             
145600           IF MID-ADBUFFOMR-UPD = 01 AND                                  
145700             (MID-ADBUFFGANG-UPD = ALL '+' OR                             
145800              MID-ADBUFFGANG-UPD = ZERO)   AND                            
145900             (MID-ADBUFFPL-UPD = ALL '+' OR                               
146000              MID-ADBUFFPL-UPD = ZERO)                                    
146100             CONTINUE                                                     
146200           ELSE                                                           
146300             IF MID-KDBRIST-IN NOT = ALL '+'                              
146400               MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDBRIST-IN-ATTR           
146500             ELSE                                                         
146600               MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDPAF-IN-ATTR             
146700             END-IF                                                       
146800             MOVE NEJ TO INDATA-SW                                        
146900           END-IF                                                         
147000         END-IF                                                           
147100       END-IF                                                             
147200     END-IF                                                               
147300     IF MID-INPUT-RAD NOT = ALL '+'                                       
147400       MOVE MID-ADBUFFOMR-UPD TO W-ADBUFFOMR                              
147500                                 WX-ADBUFFOMR                             
147600                                 WY-ADBUFFOMR                             
147700       PERFORM S01-ADBUFFOMR-KOLL                                         
147800       IF MID-ADBUFFGANG-UPD NOT = ALL '+'                                
147900         MOVE MID-ADBUFFGANG-UPD TO W-ADBUFFGANG                          
148000       ELSE                                                               
148100         MOVE ZERO               TO W-ADBUFFGANG                          
148200       END-IF                                                             
148300       IF MID-ADBUFFPL-UPD NOT = ALL '+'                                  
148400         MOVE MID-ADBUFFPL-UPD TO W-ADBUFFPL                              
148500       ELSE                                                               
148600         MOVE ZERO             TO W-ADBUFFPL                              
148700       END-IF                                                             
148800       PERFORM IMS-GU-ARTD11                                              
148900       IF SEGMENT-FINNS                                                   
149000         MOVE MFS-NUM-FAELT-FEL   TO MOD-ADBUFFOMR-UPD-ATTR               
149100                                     MOD-ADBUFFGANG-UPD-ATTR              
149200                                     MOD-ADBUFFPL-UPD-ATTR                
149300         MOVE NEJ TO INDATA-SW                                            
149400         MOVE ERR-ALREADY-EXIST   TO MED-IDMFSFEL                         
149500       ELSE                                                               
149600         IF W-ADBUFFOMR = 13 OR 21 OR 22 OR 30 OR 31 OR 32 OR 33          
149700                          OR 20 OR 24 OR 28 OR 46 OR 11 OR 50             
                                OR 52                                           
149800            MOVE W-ADBUFFOMR     TO W-ADBUFFOM-A1KY-MIN                   
149900                                    W-ADBUFFOM-A1KY-MAX                   
150000            MOVE W-ADBUFFGANG    TO W-ADBUFGAN-A1KY-MIN                   
150100                                    W-ADBUFGAN-A1KY-MAX                   
150200            MOVE W-ADBUFFPL      TO W-ADBUFPL-A1KY-MIN                    
150300                                    W-ADBUFPL-A1KY-MAX                    
150400            PERFORM IMS-GU-WDD8A1                                         
150500            IF SEGMENT-FINNS                                              
150600               IF MSGI-IDLAND-SPR NOT = 'GB'                              
150700                  MOVE 'Artnr på samma buffertplats '                     
150800                      TO MOD-MOD-LEDTEXT                                  
150900               ELSE                                                       
151000                  MOVE 'Part no in same buffer loc.  '                    
151100                      TO MOD-MOD-LEDTEXT                                  
151200               END-IF                                                     
151300               MOVE SEQA-IDARTNR TO MOD-IDARTNR                           
151400                                                                          
151500               MOVE MFS-NUM-FAELT-FEL  TO MOD-ADBUFFOMR-UPD-ATTR          
151600                                          MOD-ADBUFFGANG-UPD-ATTR         
151700                                          MOD-ADBUFFPL-UPD-ATTR           
151800               MOVE NEJ TO INDATA-SW                                      
151900               MOVE ERR-ALREADY-EXIST  TO MED-IDMFSFEL                    
152000            END-IF                                                        
152100          END-IF                                                          
152200       END-IF                                                             
152300     END-IF                                                               
152400                                                                          
152500     IF UPD-FRAN-NYCKELRAD                                                
152600       MOVE WS-ADBUFFOMR      TO W-ADBUFFOMR                              
152700       MOVE WS-ADBUFFGANG     TO W-ADBUFFGANG                             
152800       MOVE WS-ADBUFFPL       TO W-ADBUFFPL                               
152900       PERFORM IMS-GU-ARTD11                                              
153000       IF SEGMENT-FINNS                                                   
153100         MOVE NEJ TO INDATA-SW                                            
153200         MOVE ERR-ALREADY-EXIST   TO MED-IDMFSFEL                         
153300       ELSE                                                               
153400         IF W-ADBUFFOMR = 13 OR 21 OR 22 OR 30 OR 31 OR 32 OR 33          
153500                           OR 20 OR 24 OR 28 OR 46 OR 11 OR 50            
                                 OR 52                                          
153600            MOVE W-ADBUFFOMR     TO W-ADBUFFOM-A1KY-MIN                   
153700                                    W-ADBUFFOM-A1KY-MAX                   
153800            MOVE W-ADBUFFGANG    TO W-ADBUFGAN-A1KY-MIN                   
153900                                    W-ADBUFGAN-A1KY-MAX                   
154000            MOVE W-ADBUFFPL      TO W-ADBUFPL-A1KY-MIN                    
154100                                    W-ADBUFPL-A1KY-MAX                    
154200            PERFORM IMS-GU-WDD8A1                                         
154300            IF SEGMENT-FINNS                                              
154400               IF MSGI-IDLAND-SPR NOT = 'GB'                              
154500                  MOVE 'Artnr på samma buffertplats '                     
154600                      TO MOD-MOD-LEDTEXT                                  
154700               ELSE                                                       
154800                  MOVE 'Part no in same buffer loc.  '                    
154900                      TO MOD-MOD-LEDTEXT                                  
155000               END-IF                                                     
155100               MOVE SEQA-IDARTNR TO MOD-IDARTNR                           
155200                                                                          
155300               MOVE NEJ TO INDATA-SW                                      
155400               MOVE ERR-ALREADY-EXIST  TO MED-IDMFSFEL                    
155500            END-IF                                                        
155600         END-IF                                                           
155700       END-IF                                                             
155800     END-IF                                                               
155900                                                                          
156000     .                                                                    
156100     EJECT                                                                
156200 GDA-UPD-FRAN-NYCKELRAD SECTION.                                          
156300                                                                          
156400     IF MID-KDCMDVAL (1) = ALL '+' AND                                    
156500        MID-KDCMDVAL (2) = ALL '+' AND                                    
156600        MID-KDCMDVAL (3) = ALL '+' AND                                    
156700        MID-KDCMDVAL (4) = ALL '+' AND                                    
156800        MID-KDCMDVAL (5) = ALL '+' AND                                    
156900        MID-KDCMDVAL (6) = ALL '+' AND                                    
157000        MID-KDCMDVAL (7) = ALL '+' AND                                    
157100        MID-INPUT-SALDO (1) = ALL '+' AND                                 
157200        MID-INPUT-SALDO (2) = ALL '+' AND                                 
157300        MID-INPUT-SALDO (3) = ALL '+' AND                                 
157400        MID-INPUT-SALDO (4) = ALL '+' AND                                 
157500        MID-INPUT-SALDO (5) = ALL '+' AND                                 
157600        MID-INPUT-SALDO (6) = ALL '+' AND                                 
157700        MID-INPUT-SALDO (7) = ALL '+' AND                                 
157800        MID-KDBRIST-IN      = ALL '+' AND                                 
157900        MID-KDPAF-IN        = ALL '+' AND                                 
158000        MID-INPUT-RAD   = ALL '+' and                                     
158100        MID-KVANTAL-UPD = ALL '+'                                         
158200       MOVE JA TO UPD-FRAN-NYCKELRAD-SW                                   
158300     END-IF                                                               
158400     .                                                                    
158500     EJECT                                                                
158600 H-UPPDATERA SECTION.                                                     
158700                                                                          
158800     PERFORM IMS-GU-ARTD01                                                
158900     IF SEGMENT-SAKNAS                                                    
159000       MOVE W-IDARTNR      TO ARTD-ART-IDARTNR                            
159100       PERFORM IMS-ISRT-ARTD01                                            
159200     END-IF                                                               
159300     IF SYNQ-OK                                                           
159400       MOVE 'MOR' TO SYNQ-ORDERTYPE                                       
159500       MOVE W-IDARTNR TO SYNQ-IDARTNR                                     
159600       MOVE MID-KVANTAL-UPD TO SYNQ-KVBEST                                
159700       MOVE MID-TREATED-UPD TO SYNQ-TREATED                               
159800*      MOVE SPACES TO SYNQ-LOCATION                                       
159900       MOVE MID-LOCATION-UPD TO SYNQ-LOCATION                             
160000       MOVE MSGI-IDDC-KEY TO SYNQ-IDDC                                    
160100       MOVE FUNCTION CURRENT-DATE(3:6)  TO ORDDATE                        
160200       MOVE FUNCTION CURRENT-DATE(9:6)  TO ORDTIME                        
160300       MOVE WS-TIORDTIME TO SYNQ-TIORDTIME                                
160400       CALL W488ORCR USING  SYNQ-W488ORCR SYNQ-PCB                        
160500                          SYNQ-ATAB-PCB WDQ3-PCB                          
160600     ELSE                                                                 
160700     IF MID-INPUT-RAD = ALL '+'                                           
160800       MOVE +1   TO RAD-IX                                                
160900       PERFORM UNTIL RAD-IX > MAX-IX                                      
161000         IF MID-KDCMDVAL (RAD-IX) = ALL '+'                               
161100           CONTINUE                                                       
161200         ELSE                                                             
161300           INSPECT MID-ADBUFFOMR (RAD-IX)                                 
161400                                 REPLACING LEADING SPACE BY ZERO          
161500           MOVE MID-ADBUFFOMR (RAD-IX) TO W-ADBUFFOMR                     
161600                                          W-ADBUFFOMR-MIN                 
161700           INSPECT MID-ADBUFFGANG (RAD-IX)                                
161800                                  REPLACING LEADING SPACE BY ZERO         
161900           MOVE MID-ADBUFFGANG (RAD-IX) TO W-ADBUFFGANG                   
162000                                           W-ADBUFFGANG-MIN               
162100           INSPECT MID-ADBUFFPL   (RAD-IX)                                
162200                                  REPLACING LEADING SPACE BY ZERO         
162300           MOVE MID-ADBUFFPL   (RAD-IX) TO W-ADBUFFPL                     
162400                                           W-ADBUFFPL-MIN                 
162500           INSPECT MID-DABUFPAF   (RAD-IX)                                
162600                                  REPLACING LEADING SPACE BY ZERO         
162700           MOVE MID-DABUFPAF   (RAD-IX) TO W-DABUFPAF                     
162800                                           W-DABUFPAF-MIN                 
162900           PERFORM IMS-GHU-ARTD11                                         
163000           IF MID-KDCMDVAL (RAD-IX) = 'B  '                               
163100             PERFORM IMS-DLET-ARTD                                        
163200*      SLÄCKNING AV BUFFERTPLATS (WDJ9)                                   
163300             PERFORM IMS-GU-LOCB01                                        
163400             IF SEGMENT-SAKNAS                                            
163500               CONTINUE                                                   
163600             ELSE                                                         
163700               PERFORM IMS-GHNP-LOCB11                                    
163800               IF SEGMENT-FINNS                                           
163900                 PERFORM UNTIL SEGMENT-SAKNAS OR                          
164000                   (MID-ADBUFFOMR (RAD-IX)  = LOCB-HIST-ADLAGOMR)         
164100                                                             AND          
164200                   (MID-ADBUFFGANG (RAD-IX) = LOCB-HIST-ADGANG)           
164300                                                             AND          
164400                   (MID-ADBUFFPL (RAD-IX)   = LOCB-HIST-ADPLATS)          
164500                                                             AND          
164600                   (LOCB-HIST-KDLOC = 'B')                                
164700                   PERFORM IMS-GHNP-LOCB11                                
164800                 END-PERFORM                                              
164900                 IF SEGMENT-FINNS AND                                     
165000                   (MID-ADBUFFOMR (RAD-IX)  = LOCB-HIST-ADLAGOMR)         
165100                                                             AND          
165200                   (MID-ADBUFFGANG (RAD-IX) = LOCB-HIST-ADGANG)           
165300                                                             AND          
165400                   (MID-ADBUFFPL (RAD-IX)   = LOCB-HIST-ADPLATS)          
165500                                                             AND          
165600                   (LOCB-HIST-KDLOC = 'B')                                
165700                   MOVE FUNCTION CURRENT-DATE(1:8) TO                     
165800                                            LOCB-HIST-DASTODAT            
165900                   MOVE MSGI-IDUSER TO LOCB-HIST-IDUSER-STO               
166000                   PERFORM IMS-REPL-LOCB11                                
166100                 END-IF                                                   
166200               END-IF                                                     
166300             END-IF                                                       
166400           ELSE                                                           
166500             IF MID-KDCMDVAL (RAD-IX) = 'IN ' OR 'I  '                    
166600               IF MID-INPUT-SALDO (RAD-IX) NOT = ALL '+'                  
166700                 IF MID-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'                
166800                   MOVE MID-KVBUFF-F-IN (RAD-IX) TO WS-ANTAL              
166900                   ADD WS-ANTAL            TO ARTD-SALDO-KVBUFF-F         
167000                 END-IF                                                   
167100                 IF MID-KVBUFF-OF-IN (RAD-IX) NOT = ALL '+'               
167200                   MOVE MID-KVBUFF-OF-IN (RAD-IX) TO WS-ANTAL             
167300                   ADD WS-ANTAL          TO ARTD-SALDO-KVBUFF-OF          
167400                 END-IF                                                   
167500                 IF MID-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'               
167600                   MOVE MID-KVKOLLI-F-IN (RAD-IX) TO WS-ANTAL             
167700                   ADD WS-ANTAL          TO ARTD-SALDO-KVKOLLI-F          
167800                 END-IF                                                   
167900                 IF MID-KVKOLLI-OF-IN (RAD-IX) NOT = ALL '+'              
168000                   MOVE MID-KVKOLLI-OF-IN (RAD-IX) TO WS-ANTAL            
168100                   ADD WS-ANTAL          TO ARTD-SALDO-KVKOLLI-OF         
168200                 END-IF                                                   
168300               ELSE                                                       
168400                 MOVE WS-KVBUFF                 TO WS-ANTAL               
168500                 ADD WS-ANTAL          TO ARTD-SALDO-KVBUFF-F             
168600               END-IF                                                     
168700               PERFORM IMS-REPL-ARTD                                      
168800             ELSE                                                         
168900               IF MID-INPUT-SALDO (RAD-IX) NOT = ALL '+'                  
169000                 IF MID-KVBUFF-F-IN (RAD-IX) NOT = ALL '+'                
169100                   MOVE MID-KVBUFF-F-IN (RAD-IX)  TO WS-ANTAL             
169200                   SUBTRACT WS-ANTAL FROM ARTD-SALDO-KVBUFF-F             
169300                 END-IF                                                   
169400                 IF MID-KVBUFF-OF-IN (RAD-IX) NOT = ALL '+'               
169500                   MOVE MID-KVBUFF-OF-IN (RAD-IX) TO WS-ANTAL             
169600                   SUBTRACT WS-ANTAL FROM ARTD-SALDO-KVBUFF-OF            
169700                 END-IF                                                   
169800                 IF MID-KVKOLLI-F-IN (RAD-IX) NOT = ALL '+'               
169900                   MOVE MID-KVKOLLI-F-IN (RAD-IX) TO WS-ANTAL             
170000                   SUBTRACT WS-ANTAL FROM ARTD-SALDO-KVKOLLI-F            
170100                 END-IF                                                   
170200                 IF MID-KVKOLLI-OF-IN (RAD-IX) NOT = ALL '+'              
170300                   MOVE MID-KVKOLLI-OF-IN (RAD-IX) TO WS-ANTAL            
170400                   SUBTRACT WS-ANTAL FROM ARTD-SALDO-KVKOLLI-OF           
170500                 END-IF                                                   
170600               ELSE                                                       
170700                 MOVE WS-KVBUFF                  TO WS-ANTAL              
170800                 SUBTRACT WS-ANTAL FROM ARTD-SALDO-KVBUFF-F               
170900               END-IF                                                     
171000               IF (ARTD-SALDO-ADBUFFOMR = 13 OR 21 OR 22 OR 11 OR         
171100                                       30 OR 31 OR 32 OR 33 OR            
                                             50 OR 52 OR                        
171200                                       20 OR 24 OR 28 OR 46) AND          
171300                  ARTD-SALDO-KVBUFF-F  = ZERO  AND                        
171400                  ARTD-SALDO-KVBUFF-OF = ZERO  AND                        
171500                  ARTD-SALDO-KVKOLLI-F = ZERO  AND                        
171600                  ARTD-SALDO-KVKOLLI-OF = ZERO                            
171700                 PERFORM IMS-DLET-ARTD                                    
171800               ELSE                                                       
171900                 PERFORM IMS-REPL-ARTD                                    
172000               END-IF                                                     
172100             END-IF                                                       
172200           END-IF                                                         
172300         END-IF                                                           
172400         ADD +1 TO RAD-IX                                                 
172500       END-PERFORM                                                        
172600       IF MID-KDBRIST-IN = ALL '+' AND                                    
172700          MID-KDPAF-IN = ALL '+'                                          
172800         CONTINUE                                                         
172900       ELSE                                                               
173000         MOVE +1              TO W-ADBUFFOMR                              
173100         MOVE ZERO            TO W-ADBUFFGANG                             
173200                                 W-ADBUFFPL                               
173300         PERFORM IMS-GHU-ARTD11-PAF-O-BRIST                               
173400         IF SEGMENT-FINNS                                                 
173500           IF MID-KDBRIST-IN NOT = ALL '+'                                
173600             MOVE MID-KDBRIST-IN      TO ARTD-SALDO-KDBRIST               
173700           END-IF                                                         
173800           IF MID-KDPAF-IN NOT = ALL '+'                                  
173900             MOVE MID-KDPAF-IN        TO ARTD-SALDO-KDPAF                 
174000           END-IF                                                         
174100           PERFORM IMS-REPL-ARTD                                          
174200         END-IF                                                           
174300       END-IF                                                             
174400     ELSE                                                                 
174500       MOVE MID-ADBUFFOMR-UPD TO ARTD-SALDO-ADBUFFOMR                     
174600       IF MID-ADBUFFGANG-UPD NOT = ALL '+'                                
174700         MOVE MID-ADBUFFGANG-UPD TO ARTD-SALDO-ADBUFFGANG                 
174800       ELSE                                                               
174900         MOVE ZERO               TO ARTD-SALDO-ADBUFFGANG                 
175000       END-IF                                                             
175100       IF MID-ADBUFFPL-UPD NOT = ALL '+'                                  
175200         MOVE MID-ADBUFFPL-UPD TO ARTD-SALDO-ADBUFFPL                     
175300       ELSE                                                               
175400         MOVE ZERO             TO ARTD-SALDO-ADBUFFPL                     
175500       END-IF                                                             
175600       IF MID-KVBUFF-F-UPD NOT = ALL '+'                                  
175700         MOVE MID-KVBUFF-F-UPD TO ARTD-SALDO-KVBUFF-F                     
175800       ELSE                                                               
175900         MOVE ZERO             TO ARTD-SALDO-KVBUFF-F                     
176000       END-IF                                                             
176100       IF MID-KVBUFF-OF-UPD NOT = ALL '+'                                 
176200         MOVE MID-KVBUFF-OF-UPD TO ARTD-SALDO-KVBUFF-OF                   
176300       ELSE                                                               
176400         MOVE ZERO              TO ARTD-SALDO-KVBUFF-OF                   
176500       END-IF                                                             
176600       IF MID-KVKOLLI-F-UPD NOT = ALL '+'                                 
176700         MOVE MID-KVKOLLI-F-UPD TO ARTD-SALDO-KVKOLLI-F                   
176800       ELSE                                                               
176900         MOVE ZERO              TO ARTD-SALDO-KVKOLLI-F                   
177000       END-IF                                                             
177100       IF MID-KVKOLLI-OF-UPD NOT = ALL '+'                                
177200         MOVE MID-KVKOLLI-OF-UPD TO ARTD-SALDO-KVKOLLI-OF                 
177300       ELSE                                                               
177400         MOVE ZERO               TO ARTD-SALDO-KVKOLLI-OF                 
177500       END-IF                                                             
177600       IF MID-ADBUFFOMR-UPD = 01                                          
177700         IF MID-KDPAF-IN NOT = ALL '+'                                    
177800           MOVE MID-KDPAF-IN TO ARTD-SALDO-KDPAF                          
177900         ELSE                                                             
178000           MOVE ZERO        TO ARTD-SALDO-KDPAF                           
178100         END-IF                                                           
178200         IF MID-KDBRIST-IN NOT = ALL '+'                                  
178300           MOVE MID-KDBRIST-IN TO ARTD-SALDO-KDBRIST                      
178400         ELSE                                                             
178500           MOVE ZERO        TO ARTD-SALDO-KDBRIST                         
178600         END-IF                                                           
178700       ELSE                                                               
178800         MOVE ZERO        TO ARTD-SALDO-KDPAF                             
178900         MOVE ZERO        TO ARTD-SALDO-KDBRIST                           
179000       END-IF                                                             
179100       MOVE WC-CDC-SE TO ARTD-SALDO-IDDC                                  
179200       IF ARTD-SALDO-ADBUFFOMR = 13 OR 21 OR 22 OR 11 OR                  
179300                                 30 OR 31 OR 32 OR 33 OR                  
179400                                 20 OR 24 OR 28 OR 46 OR                  
                                       50 OR 52                                 
179500         MOVE FUNCTION CURRENT-DATE (1:8) TO ARTD-SALDO-DABUFPAF          
179600       ELSE                                                               
179700         MOVE ZERO                        TO ARTD-SALDO-DABUFPAF          
179800       END-IF                                                             
179900       PERFORM IMS-ISRT-ARTD11                                            
180000***    UPPLÄGG AV NY BUFFERTPLATS (WDJ9) NEDRE RADEN                      
180100       PERFORM IMS-GU-LOCB01                                              
180200       IF SEGMENT-SAKNAS                                                  
180300         MOVE W-IDARTNR TO LOCB-ART-IDARTNR                               
180400         PERFORM IMS-ISRT-LOCB01                                          
180500         PERFORM IMS-GU-LOCB01                                            
180600       END-IF                                                             
180700       IF SEGMENT-FINNS                                                   
180800         MOVE FUNCTION CURRENT-DATE(1:8)  TO LOGG-DATUM                   
180900         MOVE FUNCTION CURRENT-DATE(9:6)  TO LOGG-TID                     
181000         COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -                   
181100                                                    LOGG-DATUM            
181200         COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - LOGG-TID            
181300         MOVE WC-CDC-SE          TO LOCB-HIST-IDDC                        
181400         MOVE MID-ADBUFFOMR-UPD  TO LOCB-HIST-ADLAGOMR                    
181500         IF MID-ADBUFFGANG-UPD NOT = ALL '+'                              
181600           MOVE MID-ADBUFFGANG-UPD TO LOCB-HIST-ADGANG                    
181700         ELSE                                                             
181800           MOVE ZERO               TO LOCB-HIST-ADGANG                    
181900         END-IF                                                           
182000         IF MID-ADBUFFPL-UPD NOT = ALL '+'                                
182100           MOVE MID-ADBUFFPL-UPD   TO LOCB-HIST-ADPLATS                   
182200         ELSE                                                             
182300           MOVE ZERO               TO LOCB-HIST-ADPLATS                   
182400         END-IF                                                           
182500         MOVE BUFFER-LOCATION    TO LOCB-HIST-KDLOC                       
182600         MOVE MSGI-IDUSER        TO LOCB-HIST-IDUSER                      
182700         MOVE SPACE              TO LOCB-HIST-IDUSER-STO                  
182800         MOVE ZERO               TO LOCB-HIST-DASTODAT                    
182900                                                                          
183000         PERFORM IMS-ISRT-LOCB11                                          
183100       END-IF                                                             
183200     END-IF                                                               
183300     IF UPD-FRAN-NYCKELRAD                                                
183400       MOVE WS-ADBUFFOMR    TO ARTD-SALDO-ADBUFFOMR                       
183500       MOVE WS-ADBUFFGANG   TO ARTD-SALDO-ADBUFFGANG                      
183600       MOVE WS-ADBUFFPL     TO ARTD-SALDO-ADBUFFPL                        
183700       MOVE WS-KVBUFF       TO ARTD-SALDO-KVBUFF-F                        
183800       IF WS-ADBUFFOMR = 13 OR 21 OR 22 OR 30 OR 31 OR 32 OR 33           
183900                            OR 20 OR 24 OR 28 OR 46 OR 11 OR 50           
                                  OR 52                                         
184000         MOVE +1            TO ARTD-SALDO-KVKOLLI-F                       
184100         MOVE FUNCTION CURRENT-DATE (1:8) TO ARTD-SALDO-DABUFPAF          
184200       ELSE                                                               
184300         MOVE ZERO          TO ARTD-SALDO-KVKOLLI-F                       
184400                               ARTD-SALDO-DABUFPAF                        
184500       END-IF                                                             
184600       MOVE ZERO            TO ARTD-SALDO-KVBUFF-OF                       
184700                               ARTD-SALDO-KVKOLLI-OF                      
184800                               ARTD-SALDO-KDPAF                           
184900                               ARTD-SALDO-KDBRIST                         
185000       MOVE WC-CDC-SE       TO ARTD-SALDO-IDDC                            
185100       PERFORM IMS-ISRT-ARTD11                                            
185200***    UPPLÄGG AV NY BUFFERTPLATS (WDJ9) ÖVRE RADEN                       
185300       PERFORM IMS-GU-LOCB01                                              
185400       IF SEGMENT-SAKNAS                                                  
185500         MOVE W-IDARTNR TO LOCB-ART-IDARTNR                               
185600         PERFORM IMS-ISRT-LOCB01                                          
185700         PERFORM IMS-GU-LOCB01                                            
185800       END-IF                                                             
185900       IF SEGMENT-FINNS                                                   
186000         MOVE FUNCTION CURRENT-DATE(1:8)  TO LOGG-DATUM                   
186100         MOVE FUNCTION CURRENT-DATE(9:6)  TO LOGG-TID                     
186200         COMPUTE LOCB-HIST-DASTADAT-9KOMPL = 99999999 -                   
186300                                                    LOGG-DATUM            
186400         COMPUTE LOCB-HIST-TISTATID-9KOMPL = 999999 - LOGG-TID            
186500         MOVE WC-CDC-SE          TO LOCB-HIST-IDDC                        
186600         MOVE WS-ADBUFFOMR       TO LOCB-HIST-ADLAGOMR                    
186700         IF WS-ADBUFFGANG NOT = ALL '+'                                   
186800           MOVE WS-ADBUFFGANG    TO LOCB-HIST-ADGANG                      
186900         ELSE                                                             
187000           MOVE ZERO             TO LOCB-HIST-ADGANG                      
187100         END-IF                                                           
187200         IF WS-ADBUFFPL NOT = ALL '+'                                     
187300           MOVE WS-ADBUFFPL      TO LOCB-HIST-ADPLATS                     
187400         ELSE                                                             
187500           MOVE ZERO             TO LOCB-HIST-ADPLATS                     
187600         END-IF                                                           
187700         MOVE BUFFER-LOCATION    TO LOCB-HIST-KDLOC                       
187800         MOVE MSGI-IDUSER        TO LOCB-HIST-IDUSER                      
187900         MOVE SPACE              TO LOCB-HIST-IDUSER-STO                  
188000         MOVE ZERO               TO LOCB-HIST-DASTODAT                    
188100                                                                          
188200         PERFORM IMS-ISRT-LOCB11                                          
188300       END-IF                                                             
188400     END-IF                                                               
188500     END-IF                                                               
188600                                                                          
188700     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
188800     CALL WMEDKONV USING MED-WMEDAREA                                     
188900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
189000     PERFORM MFS-FORM-ATTR                                                
189100     PERFORM MFS-RENSA-FAELT-IN                                           
189200     .                                                                    
189300     EJECT                                                                
189400 S01-ADBUFFOMR-KOLL  SECTION.                                             
189500                                                                          
189600     IF MSGI-IDUSER = 'PCSSE01'                                           
189700       IF WX-ADBUFFOMR = 42 OR                                            
189800         (WY-ADBUFFOMR = 42 AND WX-ADBUFFOMR = ZERO) OR                   
189900          WX-ADBUFFOMR = 43 OR                                            
190000         (WY-ADBUFFOMR = 43 AND WX-ADBUFFOMR = ZERO)                      
190100         CONTINUE                                                         
190200       ELSE                                                               
190300         MOVE NEJ        TO INDATA-SW                                     
190400                            BUFFOMR-SW                                    
190500       END-IF                                                             
190600       IF WY-ADBUFFOMR = 42 OR 43 OR ZERO                                 
190700         CONTINUE                                                         
190800       ELSE                                                               
190900         MOVE NEJ        TO INDATA-SW                                     
191000                            BUFFOMR-SW                                    
191100       END-IF                                                             
191200     END-IF                                                               
191300     .                                                                    
191400     EJECT                                                                
191500 S02-TRANS-W60143 SECTION.                                                
191600*----------INITIERA TILL TRANS BAKGRUNDS-MPP ' W60143'                    
191700*    MOVE IDLEVNR-KOLLI TO T43-MID-IDLEVNR-KOLLI-IN                       
191800*    MOVE IDOKOLLI TO T43-MID-IDOKOLLI-IN                                 
191900     MOVE ALL '+' TO T43-MID-W6I14301                                     
192000     MOVE REQU-IDLEVNR-KOLLI-SYNQ TO T43-MID-IDLEVNR-KOLLI-IN             
192100     MOVE REQU-IDOKOLLI-SYNQ TO T43-MID-IDOKOLLI-IN                       
192200     MOVE 11 TO T43-MID-IDDC-IN                                           
192300     MOVE 88 TO PTOP1-LL                                                  
192400     PERFORM S03-SKICKA-W60143                                            
192500     .                                                                    
192600     EJECT                                                                
192700 S03-SKICKA-W60143 SECTION.                                               
192800     MOVE MFS-KDMFSFOR         TO PTOP1-KDMFSFOR                          
192900     PERFORM IMS-ISRT-MSG-ALT1-6143                                       
193000     .                                                                    
193100     EJECT                                                                
193200 MFS-RENSA-FAELT-UT SECTION.                                              
193300                                                                          
193400*    --- ALLA UTDATA-FÄLT                                                 
193500     MOVE MFS-RENSA-FAELT TO MOD-ADLAGOMR                                 
193600                             MOD-ADGANG                                   
193700                             MOD-ADPLATS                                  
193800                             MOD-FLERPL                                   
193900                             MOD-BEART                                    
194000                             MOD-KVLS                                     
                                   MOD-KVQPACK-3                                
194100                             MOD-KDBRIST-UT                               
194200                             MOD-KDPAF-UT                                 
194300                             MOD-SUBUFF-F                                 
194400                             MOD-SUBUFF-OF                                
194500                             MOD-SUKOLLI-F                                
194600                             MOD-SUKOLLI-OF                               
194700     MOVE +1 TO RAD-IX                                                    
194800     PERFORM UNTIL RAD-IX > MAX-IX                                        
194900       MOVE MFS-RENSA-FAELT TO MOD-ADBUFFOMR (RAD-IX)                     
195000                               MOD-ADBUFFGANG (RAD-IX)                    
195100                               MOD-ADBUFFPL (RAD-IX)                      
195200                               MOD-DABUFPAF (RAD-IX)                      
195300                               MOD-KVBUFF-F (RAD-IX)                      
195400                               MOD-KVBUFF-OF (RAD-IX)                     
195500                               MOD-KVKOLLI-F (RAD-IX)                     
195600                               MOD-KVKOLLI-OF (RAD-IX)                    
195700       ADD +1 TO RAD-IX                                                   
195800     END-PERFORM                                                          
195900     MOVE MFS-RENSA-FAELT TO MOD-MOD-LEDTEXT                              
196000                             MOD-IDARTNR                                  
196100     .                                                                    
196200     SKIP3                                                                
196300 MFS-RENSA-FAELT-UT-BUFFERT SECTION.                                      
196400                                                                          
196500*    --- ALLA UTDATA-FÄLT UTOM ARTIKEL UPPGIFTER                          
196600                                                                          
196700     MOVE MFS-RENSA-FAELT TO MOD-KDBRIST-UT                               
196800                             MOD-KDPAF-UT                                 
196900                             MOD-SUBUFF-F                                 
197000                             MOD-SUBUFF-OF                                
197100                             MOD-SUKOLLI-F                                
197200                             MOD-SUKOLLI-OF                               
197300     MOVE +1 TO RAD-IX                                                    
197400     PERFORM UNTIL RAD-IX > MAX-IX                                        
197500       MOVE MFS-RENSA-FAELT TO MOD-ADBUFFOMR (RAD-IX)                     
197600                               MOD-ADBUFFGANG (RAD-IX)                    
197700                               MOD-ADBUFFPL (RAD-IX)                      
197800                               MOD-DABUFPAF (RAD-IX)                      
197900                               MOD-KVBUFF-F (RAD-IX)                      
198000                               MOD-KVBUFF-OF (RAD-IX)                     
198100                               MOD-KVKOLLI-F (RAD-IX)                     
198200                               MOD-KVKOLLI-OF (RAD-IX)                    
198300       ADD +1 TO RAD-IX                                                   
198400     END-PERFORM                                                          
198500     .                                                                    
198600     SKIP3                                                                
198700 MFS-RENSA-FAELT-IN SECTION.                                              
198800                                                                          
198900*    --- ALLA INDATA-FÄLT                                                 
199000     MOVE MFS-RENSA-FAELT TO MOD-KDBRIST-IN                               
199100                             MOD-KDPAF-IN                                 
199200                             MOD-ADBUFFOMR-UPD                            
199300                             MOD-ADBUFFGANG-UPD                           
199400                             MOD-ADBUFFPL-UPD                             
199500                             MOD-KVBUFF-F-UPD                             
199600                             MOD-KVBUFF-OF-UPD                            
199700                             MOD-KVKOLLI-F-UPD                            
199800                             MOD-KVKOLLI-OF-UPD                           
199900                             MOD-KVANTAL-UPD                              
200000                             MOD-TREATED-UPD                              
200100                             MOD-LOCATION-UPD                             
200200     MOVE +1 TO RAD-IX                                                    
200300     PERFORM UNTIL RAD-IX > MAX-IX                                        
200400       MOVE MFS-RENSA-FAELT TO MOD-KDCMDVAL (RAD-IX)                      
200500                               MOD-KVBUFF-F-IN (RAD-IX)                   
200600                               MOD-KVBUFF-OF-IN (RAD-IX)                  
200700                               MOD-KVKOLLI-F-IN (RAD-IX)                  
200800                               MOD-KVKOLLI-OF-IN (RAD-IX)                 
200900       ADD +1 TO RAD-IX                                                   
201000     END-PERFORM                                                          
201100     .                                                                    
201200     EJECT                                                                
201300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
201400                                                                          
201500*    --- ALLA UTDATA-FÄLT                                                 
201600     MOVE MFS-ROER-EJ-FAELT TO MOD-ADLAGOMR                               
201700                               MOD-ADGANG                                 
201800                               MOD-ADPLATS                                
201900                               MOD-FLERPL                                 
202000                               MOD-BEART                                  
202100                               MOD-KVLS                                   
                                     MOD-KVQPACK-3                              
202200                               MOD-KDBRIST-UT                             
202300                               MOD-KDPAF-UT                               
202400                               MOD-SUBUFF-F                               
202500                               MOD-SUBUFF-OF                              
202600                               MOD-SUKOLLI-F                              
202700                               MOD-SUKOLLI-OF                             
202800     MOVE +1 TO RAD-IX                                                    
202900     PERFORM UNTIL RAD-IX > MAX-IX                                        
203000       MOVE MFS-ROER-EJ-FAELT TO MOD-ADBUFFOMR (RAD-IX)                   
203100                                 MOD-ADBUFFGANG (RAD-IX)                  
203200                                 MOD-ADBUFFPL (RAD-IX)                    
203300                                 MOD-DABUFPAF (RAD-IX)                    
203400                                 MOD-KVBUFF-F (RAD-IX)                    
203500                                 MOD-KVBUFF-OF (RAD-IX)                   
203600                                 MOD-KVKOLLI-F (RAD-IX)                   
203700                                 MOD-KVKOLLI-OF (RAD-IX)                  
203800       ADD +1 TO RAD-IX                                                   
203900     END-PERFORM                                                          
204000***  MOVE MFS-ROER-EJ-FAELT TO MOD-MOD-LEDTEXT                            
204100***                            MOD-IDARTNR                                
204200     .                                                                    
204300     SKIP3                                                                
204400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
204500                                                                          
204600*    --- ALLA INDATA-FÄLT                                                 
204700     MOVE MFS-ROER-EJ-FAELT TO MOD-KDBRIST-IN                             
204800                               MOD-KDPAF-IN                               
204900                               MOD-ADBUFFOMR-UPD                          
205000                               MOD-ADBUFFGANG-UPD                         
205100                               MOD-ADBUFFPL-UPD                           
205200                               MOD-KVBUFF-F-UPD                           
205300                               MOD-KVBUFF-OF-UPD                          
205400                               MOD-KVKOLLI-F-UPD                          
205500                               MOD-KVKOLLI-OF-UPD                         
205600                               MOD-KVANTAL-UPD                            
205700                               MOD-TREATED-UPD                            
205800                               MOD-LOCATION-UPD                           
205900     MOVE +1 TO RAD-IX                                                    
206000     PERFORM UNTIL RAD-IX > MAX-IX                                        
206100       MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMDVAL (RAD-IX)                    
206200                                 MOD-KVBUFF-F-IN (RAD-IX)                 
206300                                 MOD-KVBUFF-OF-IN (RAD-IX)                
206400                                 MOD-KVKOLLI-F-IN (RAD-IX)                
206500                                 MOD-KVKOLLI-OF-IN (RAD-IX)               
206600       ADD +1 TO RAD-IX                                                   
206700     END-PERFORM                                                          
206800     .                                                                    
206900     EJECT                                                                
207000 MFS-FORM-ATTR SECTION.                                                   
207100                                                                          
207200*    --- ALLA INDATA-FÄLT                                                 
207300     MOVE MFS-FORMATETS-ATTR TO MOD-KDBRIST-IN-ATTR                       
207400                                MOD-KDPAF-IN-ATTR                         
207500                                MOD-ADBUFFOMR-UPD-ATTR                    
207600                                MOD-ADBUFFGANG-UPD-ATTR                   
207700                                MOD-ADBUFFPL-UPD-ATTR                     
207800                                MOD-KVBUFF-F-UPD-ATTR                     
207900                                MOD-KVBUFF-OF-UPD-ATTR                    
208000                                MOD-KVKOLLI-F-UPD-ATTR                    
208100                                MOD-KVKOLLI-OF-UPD-ATTR                   
208200                                MOD-KVANTAL-UPD-ATTR                      
208300                                MOD-TREATED-UPD-ATTR                      
208400                                MOD-LOCATION-UPD-ATTR                     
208500     MOVE +1 TO RAD-IX                                                    
208600     PERFORM UNTIL RAD-IX > MAX-IX                                        
208700       MOVE MFS-FORMATETS-ATTR TO MOD-KDCMDVAL-ATTR (RAD-IX)              
208800                                  MOD-KVBUFF-F-IN-ATTR (RAD-IX)           
208900                                  MOD-KVBUFF-OF-IN-ATTR (RAD-IX)          
209000                                  MOD-KVKOLLI-F-IN-ATTR (RAD-IX)          
209100                                  MOD-KVKOLLI-OF-IN-ATTR (RAD-IX)         
209200       ADD +1 TO RAD-IX                                                   
209300     END-PERFORM                                                          
209400     .                                                                    
209500     SKIP2                                                                
209600*MFS-LAES-IN-IGEN SECTION.                                                
209700*                                                                         
209800*    --- ALLA INDATA-FÄLT                                                 
209900*    MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBRIST-IN-ATTR                    
210000*                                  MOD-KDPAF-IN-ATTR                      
210100*                                  MOD-ADBUFFOMR-UPD-ATTR                 
210200*                                  MOD-ADBUFFGANG-UPD-ATTR                
210300*                                  MOD-ADBUFFPL-UPD-ATTR                  
210400*                                  MOD-KVBUFF-F-UPD-ATTR                  
210500*                                  MOD-KVBUFF-OF-UPD-ATTR                 
210600*                                  MOD-KVKOLLI-F-UPD-ATTR                 
210700*                                  MOD-KVKOLLI-OF-UPD-ATTR                
210800*    MOVE +1 TO RAD-IX                                                    
210900*    PERFORM UNTIL RAD-IX > MAX-IX                                        
211000*      MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMDVAL-ATTR (RAD-IX)           
211100*                                    MOD-KVBUFF-F-IN-ATTR (RAD-IX)        
211200*                                  MOD-KVBUFF-OF-IN-ATTR (RAD-IX)         
211300*                                  MOD-KVKOLLI-F-IN-ATTR (RAD-IX)         
211400*                                  MOD-KVKOLLI-OF-IN-ATTR (RAD-IX)        
211500*      ADD +1 TO RAD-IX                                                   
211600*    END-PERFORM                                                          
211700*    .                                                                    
211800*    EJECT                                                                
211900* --- IMS SEKTIONER ---                                                   
212000     SKIP3                                                                
212100 IMS-GET-MSG SECTION.                                                     
212200                                                                          
212300     MOVE '  QC' TO GODK-STATUSKODER                                      
212400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
212500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
212600     PERFORM IMS-STATUSKONTROLL                                           
212700     .                                                                    
212800     SKIP3                                                                
212900 IMS-INSERT-MSG SECTION.                                                  
213000                                                                          
213100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
213200       MOVE '0' TO MFS-KDHUVOMR                                           
213300     END-IF                                                               
213400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
213500     MOVE SPACE TO GODK-STATUSKODER                                       
213600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
213700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
213800     PERFORM IMS-STATUSKONTROLL                                           
213900     .                                                                    
214000     EJECT                                                                
214100 IMS-GU-ARTC11   SECTION.                                                 
214200                                                                          
214300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
214400          DELIMITED BY SIZE INTO SSA1                                     
214500     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
214600          DELIMITED BY SIZE INTO SSA2                                     
214700     MOVE '  GE' TO GODK-STATUSKODER                                      
214800     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-AREA SSA1 SSA2                
214900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
215000     PERFORM IMS-STATUSKONTROLL                                           
215100     .                                                                    
215200     EJECT                                                                
215300 IMS-GU-WDD8A1 SECTION.                                                   
215400     STRING 'WDD8A1  (WDD8A1KY>=' W-WDD8A1KY-MIN-X                        
215500                    '&WDD8A1KY=<' W-WDD8A1KY-MAX-X ')'                    
215600            DELIMITED BY SIZE INTO SSA1                                   
215700     MOVE '  GE' TO GODK-STATUSKODER                                      
215800     CALL CBLTDLI USING GU WDD8A-PCB DLI-IO-AREA3 SSA1                    
215900     MOVE WDD8A-STATUS-CODE TO STATUS-WS                                  
216000     PERFORM IMS-STATUSKONTROLL                                           
216100     .                                                                    
216200     SKIP3                                                                
216300 IMS-GU-ARTD01    SECTION.                                                
216400                                                                          
216500     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
216600          DELIMITED BY SIZE INTO SSA1                                     
216700     MOVE '  GE' TO GODK-STATUSKODER                                      
216800     CALL CBLTDLI USING GHU ARTD-PCB DLI-IO-AREA SSA1                     
216900     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
217000     PERFORM IMS-STATUSKONTROLL                                           
217100     .                                                                    
217200     SKIP3                                                                
217300 IMS-GNP-ARTD11     SECTION.                                              
217400                                                                          
217500     STRING 'WLARTD11(WDD811KY=>' W-WDD811KY-MIN-X                        
217600                    '&WDD811KY=<' W-WDD811KY-MAX-X ')'                    
217700          DELIMITED BY SIZE INTO SSA1                                     
217800     MOVE '  GE' TO GODK-STATUSKODER                                      
217900     CALL CBLTDLI USING GNP ARTD-PCB DLI-IO-AREA SSA1                     
218000     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
218100     PERFORM IMS-STATUSKONTROLL                                           
218200     .                                                                    
218300     SKIP3                                                                
218400 IMS-GU-ARTD11     SECTION.                                               
218500                                                                          
218600     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
218700          DELIMITED BY SIZE INTO SSA1                                     
218800     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
218900          DELIMITED BY SIZE INTO SSA2                                     
219000     MOVE '  GE' TO GODK-STATUSKODER                                      
219100     CALL CBLTDLI USING GU ARTD-PCB DLI-IO-AREA SSA1 SSA2                 
219200     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
219300     PERFORM IMS-STATUSKONTROLL                                           
219400     .                                                                    
219500     SKIP3                                                                
219600 IMS-GHU-ARTD11     SECTION.                                              
219700                                                                          
219800     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
219900          DELIMITED BY SIZE INTO SSA1                                     
220000     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
220100          DELIMITED BY SIZE INTO SSA2                                     
220200     MOVE '  ' TO GODK-STATUSKODER                                        
220300     CALL CBLTDLI USING GHU ARTD-PCB DLI-IO-AREA SSA1 SSA2                
220400     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
220500     PERFORM IMS-STATUSKONTROLL                                           
220600     .                                                                    
220700     SKIP3                                                                
220800 IMS-GHU-ARTD11-PAF-O-BRIST SECTION.                                      
220900                                                                          
221000     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
221100          DELIMITED BY SIZE INTO SSA1                                     
221200     STRING 'WLARTD11(WDD811KY =' W-WDD811KY-X ')'                        
221300          DELIMITED BY SIZE INTO SSA2                                     
221400     MOVE '  GE' TO GODK-STATUSKODER                                      
221500     CALL CBLTDLI USING GHU ARTD-PCB DLI-IO-AREA SSA1 SSA2                
221600     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
221700     PERFORM IMS-STATUSKONTROLL                                           
221800     .                                                                    
221900     SKIP3                                                                
222000 IMS-ISRT-ARTD01 SECTION.                                                 
222100                                                                          
222200     MOVE 'WLARTD01 ' TO SSA1                                             
222300     MOVE '  II' TO GODK-STATUSKODER                                      
222400     CALL CBLTDLI USING ISRT ARTD-PCB DLI-IO-AREA SSA1                    
222500     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
222600     PERFORM IMS-STATUSKONTROLL                                           
222700     .                                                                    
222800     SKIP3                                                                
222900 IMS-ISRT-ARTD11 SECTION.                                                 
223000                                                                          
223100     STRING 'WLARTD01(IDARTNR  =' W-IDARTNR-X ')'                         
223200          DELIMITED BY SIZE INTO SSA1                                     
223300     MOVE 'WLARTD11 ' TO SSA2                                             
223400     MOVE '  II' TO GODK-STATUSKODER                                      
223500     CALL CBLTDLI USING ISRT ARTD-PCB DLI-IO-AREA SSA1 SSA2               
223600     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
223700     PERFORM IMS-STATUSKONTROLL                                           
223800     .                                                                    
223900     SKIP3                                                                
224000 IMS-REPL-ARTD SECTION.                                                   
224100                                                                          
224200     MOVE '  ' TO GODK-STATUSKODER                                        
224300     CALL CBLTDLI USING REPL ARTD-PCB DLI-IO-AREA                         
224400     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
224500     PERFORM IMS-STATUSKONTROLL                                           
224600     .                                                                    
224700     EJECT                                                                
224800 IMS-DLET-ARTD SECTION.                                                   
224900                                                                          
225000     MOVE '  ' TO GODK-STATUSKODER                                        
225100     CALL CBLTDLI USING DLET ARTD-PCB DLI-IO-AREA                         
225200     MOVE ARTD-STATUS-CODE TO STATUS-WS                                   
225300     PERFORM IMS-STATUSKONTROLL                                           
225400     .                                                                    
225500     EJECT                                                                
225600 IMS-GU-BENA11    SECTION.                                                
225700                                                                          
225800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
225900          DELIMITED BY SIZE INTO SSA1                                     
226000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
226100          DELIMITED BY SIZE INTO SSA2                                     
226200     MOVE '  GE' TO GODK-STATUSKODER                                      
226300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
226400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
226500     PERFORM IMS-STATUSKONTROLL                                           
226600     .                                                                    
226700     EJECT                                                                
226800                                                                          
226900 IMS-GU-LOCB01 SECTION.                                                   
227000                                                                          
227100     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
227200          DELIMITED BY SIZE INTO SSA1                                     
227300     MOVE '  GE' TO GODK-STATUSKODER                                      
227400     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-AREA SSA1                      
227500     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
227600     PERFORM IMS-STATUSKONTROLL                                           
227700     .                                                                    
227800     EJECT                                                                
227900                                                                          
228000 IMS-ISRT-LOCB01 SECTION.                                                 
228100                                                                          
228200     MOVE 'WLLOCB01 ' TO SSA1                                             
228300     MOVE '  ' TO GODK-STATUSKODER                                        
228400     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA SSA1                    
228500     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
228600     PERFORM IMS-STATUSKONTROLL                                           
228700     .                                                                    
228800     SKIP3                                                                
228900                                                                          
229000 IMS-GHNP-LOCB11 SECTION.                                                 
229100                                                                          
229200     STRING 'WLLOCB11(IDDC     =' W-IDDC-X ')'                            
229300             DELIMITED BY SIZE INTO SSA1                                  
229400     MOVE '  GE' TO GODK-STATUSKODER                                      
229500     CALL CBLTDLI USING GHNP LOCB-PCB DLI-IO-AREA SSA1                    
229600     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
229700     PERFORM IMS-STATUSKONTROLL                                           
229800                                                                          
229900     EJECT                                                                
230000     .                                                                    
230100                                                                          
230200 IMS-REPL-LOCB11 SECTION.                                                 
230300                                                                          
230400     MOVE '  ' TO GODK-STATUSKODER                                        
230500     CALL CBLTDLI USING REPL LOCB-PCB DLI-IO-AREA                         
230600     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
230700     PERFORM IMS-STATUSKONTROLL                                           
230800     .                                                                    
230900     EJECT                                                                
231000     SKIP3                                                                
231100                                                                          
231200 IMS-ISRT-LOCB11 SECTION.                                                 
231300                                                                          
231400     STRING 'WLLOCB01(IDARTNR  =' W-IDARTNR-X ')'                         
231500          DELIMITED BY SIZE INTO SSA1                                     
231600     MOVE 'WLLOCB11 ' TO SSA2                                             
231700     MOVE '  II' TO GODK-STATUSKODER                                      
231800     CALL CBLTDLI USING ISRT LOCB-PCB DLI-IO-AREA SSA1 SSA2               
231900     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
232000     PERFORM IMS-STATUSKONTROLL                                           
232100     .                                                                    
232200     SKIP3                                                                
232300 IMS-ISRT-MSG-ALT1-6143 SECTION.                                          
232400                                                                          
232500      MOVE SPACE              TO GODK-STATUSKODER                         
232600      CALL CBLTDLI USING      ISRT ALT1-PCB                               
232700                                   P-TO-P-T43                             
232800      MOVE ALT1-STATUS-CODE   TO STATUS-WS                                
232900      PERFORM IMS-STATUSKONTROLL                                          
233000      .                                                                   
233100      SKIP3                                                               
233200                                                                          
233300 IMS-STATUSKONTROLL SECTION.                                              
233400                                                                          
233500     SET STATUS-IX TO 1                                                   
233600     SEARCH GODK-STATUS                                                   
233700       AT END                                                             
233800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
233900         DELIMITED BY SIZE INTO FELTEXT                                   
234000         CALL FELLOG                                                      
234100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
234200         CONTINUE                                                         
234300     END-SEARCH                                                           
234400     .                                                                    

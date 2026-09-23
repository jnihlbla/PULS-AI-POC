001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W4634100.                                                
001400 AUTHOR.         KJELLSON GÖRAN.                                          
001500 DATE-WRITTEN.   VINTERN 2017/18                                          
001510                                                                          
001600 DATE-COMPILED.                                                           
001900*    FUNKTION:                                                            
002000*        UPPDATERING WDF2 (DDGS-SALDO)                                    
002100*                                                                         
002300*                                                                         
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003101                                                                          
003102*          --- SALDO-POSTER FÖR UPPDATERING                               
003110     SELECT W46343                     ASSIGN TO W46341D1.                
003300                                                                          
003301*          --- KVITTOPOSTER D&P TILL LEVERANTÖREN                         
003302     SELECT W46344                     ASSIGN TO W46341D2.                
003312                                                                          
003320                                                                          
003400 DATA DIVISION.                                                           
003600 FILE SECTION.                                                            
003701                                                                          
003702 FD  W46343                                                               
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY W46343      -L.                                                
003800                                                                          
003810                                                                          
003820 FD  W46344                                                               
003830     RECORDING       V                                                    
003840     BLOCK CONTAINS  0.                                                   
003850                                                                          
003851 01  KVITTO-POST     PIC X(80).                                           
003870                                                                          
003880                                                                          
003920 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W4634100'.            
004200 01  CHKP-VAR.                                                            
004300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004800     03 CHKP-MAX                 PIC S9(3)   VALUE +700 COMP-3.           
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005010 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005020 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005040 77  POST-ANT                    PIC S9(5)   VALUE +0   COMP-3.           
005050 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
005100                                                                          
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005601                                                                          
005602 77  W46343-EOF-SW               PIC X       VALUE 'N'.                   
005610     88  END-OF-W46343                       VALUE 'J'.                   
005900                                                                          
005910                                                                          
005920 77  INTRO-WRITTEN-SW            PIC X       VALUE 'N'.                   
005930     88  INTRO-WRITTEN                       VALUE 'J'.                   
005931                                                                          
005932 01  WS-NOLLOR                   PIC 9(2)    VALUE ZERO.                  
005933 01  WS-START                    PIC 9(2)    VALUE ZERO.                  
005934 01  WS-LANGD                    PIC 9(2)    VALUE ZERO.                  
005935                                                                          
005936 01  CURRENT-DATE                PIC 9(6).                                
005940                                                                          
006300 01  WS-DATUM-X.                                                          
006311     03  FILLER                  PIC X(2).                                
006320     03  WS-DATUM                PIC X(6).                                
006512                                                                          
006513 01  MAIL-AREA.                                                           
006514     03  MAIL-BLANKRAD           PIC X       VALUE SPACE.                 
006515                                                                          
006516     03  MAIL-INTRO-1.                                                    
006517         05  FILLER              PIC X(05)   VALUE                        
006518            'Hello'.                                                      
006519     03  MAIL-INTRO-2.                                                    
006520         05  FILLER              PIC X(43)   VALUE                        
006521            'BP2T7 has received an EDI transaction from '.                
006522         05  INTRO-IDLEVNR       PIC X(05)   VALUE SPACE.                 
006523         05  FILLER              PIC X(26)   VALUE                        
006524            ' with updated stock value.'.                                 
006525     03  MAIL-INTRO-3.                                                    
006526         05  FILLER              PIC X(25)   VALUE                        
006527            'The message was received '.                                  
006528         05  INTRO-DATE          PIC X(06)   VALUE SPACE.                 
006529         05  FILLER              PIC X(01)   VALUE SPACE.                 
006530         05  INTRO-TIME          PIC X(04)   VALUE SPACE.                 
006531     03  MAIL-ERROR.                                                      
006532         05  FILLER              PIC X(09)   VALUE                        
006533           'Part no: '.                                                   
006534         05  MAIL-IDARTNR        PIC 9(08)   VALUE ZERO.                  
006535         05  FILLER              PIC X(38)   VALUE                        
006536           ' is not set up as a DDGS-part in PULS.'.                      
006537     03  MAIL-END-OK.                                                     
006538         05  FILLER              PIC X(30)   VALUE                        
006539           'New values updated OK in PULS.'.                              
006540     03  MAIL-END-ERROR-1.                                                
006541         05  FILLER              PIC X(29)   VALUE                        
006542           'New values updated OK in PULS'.                               
006543     03  MAIL-END-ERROR-2.                                                
006544         05  FILLER              PIC X(41)   VALUE                        
006545           'with exception for Part nos listed above.'.                   
006550                                                                          
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007020     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007101                                                                          
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007402                                                                          
007403 01  IN-AREA-START               PIC X(24)   VALUE                        
007404                                             'IN-AREA-START'.             
007410*01  AREA -COPY W46343     -PRE IN-                                       
007500*                                                                         
007610                                                                          
007696                                                                          
007697 01  KVITTO-AREA-START           PIC X(24)   VALUE                        
007698                                             'KVITTO-AREA-START'.         
007699 01  KVITTO-AREA                 PIC X(80).                               
007700*                                                                         
007710                                                                          
007720                                                                          
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007810                                                                          
007811*01  -COPY WDGX01                                                         
007820                                                                          
007900 01  NYCKLAR-TILL-DLI.                                                    
007901     03  W-WDF2A1KY.                                                      
007902         05  W-IDARTNR-A1        PIC S9(9)   VALUE ZERO COMP-3.           
007903         05  W-IDLEVNR-A1        PIC  X(5)   VALUE SPACE.                 
008002                                                                          
008003     03  W-WDF201KY-X.                                                    
008004         05  W-IDLEVNR-WDF2      PIC  X(5)   VALUE SPACE.                 
008005         05  W-IDDIRGRP-WDF2     PIC X(10)   VALUE SPACE.                 
008006     03  W-IDARTNR-X.                                                     
008007         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008100                                                                          
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008800     88  IMS-EJ-OK                           VALUE 'XD'.                  
008900                                                                          
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200                                                                          
009300 01  ALL-SSA.                                                             
009310     03 SSA1                     PIC X(64).                               
009400     03 SSA2                     PIC X(64).                               
009500                                                                          
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800                                                                          
009900                                                                          
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100                                                                          
010201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF201'.                      
010202 01  DLI-IO-WDF201.                                                       
010203*    03  -COPY WDF201                                                     
010204                                                                          
010205 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF212'.                      
010206 01  DLI-IO-WDF212.                                                       
010207*    03  -COPY WDF212                                                     
010208                                                                          
010209                                                                          
010210 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF2A1'.                      
010211 01  DLI-IO-WDF2A1.                                                       
010212*    03  -COPY WDF2A1                                                     
010213                                                                          
010220                                                                          
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0009  -PRE MSG-                                               
011101                                                                          
011102*01  -COPY W0008  -PRE WDF2-                                              
011110     05  FILLER              PIC X.                                       
011111                                                                          
011112*01  -COPY W0008  -PRE WDF2A-                                             
011113     05  FILLER              PIC X.                                       
011120                                                                          
011500                                                                          
011501 PROCEDURE DIVISION  USING MSG-PCB WDF2-PCB WDF2A-PCB.                    
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB WDF2-PCB WDF2A-PCB.                    
011600                                                                          
011900     PERFORM A-INIT                                                       
012002                                                                          
012007     PERFORM S01-LAES-W46343                                              
012020                                                                          
012100     PERFORM UNTIL END-OF-W46343                                          
012200        IF CHKP-ANT > CHKP-MAX                                            
012300           PERFORM X-TAG-CHECKPOINT                                       
012400        END-IF                                                            
012410                                                                          
012443        PERFORM B-UPPDATERA-DDGS-SALDO                                    
012445                                                                          
013110        PERFORM S01-LAES-W46343                                           
013200     END-PERFORM                                                          
013400                                                                          
013410     IF INTRO-WRITTEN                                                     
013500        PERFORM S12-SKRIV-MAIL-END                                        
013501     END-IF                                                               
013510     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014010                                                                          
014100 A-INIT SECTION.                                                          
014200     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
014300                                                                          
014400     PERFORM IMS-RESTART                                                  
014601                                                                          
014610     OPEN INPUT  W46343                                                   
014620     OPEN OUTPUT W46344                                                   
015200                                                                          
015210                                                                          
015220     ACCEPT CURRENT-DATE  FROM DATE                                       
015300     MOVE +0            TO POST-ANT                                       
015301                           CHKP-ANT                                       
015302                                                                          
015310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
016030                                                                          
016436 B-UPPDATERA-DDGS-SALDO SECTION.                                          
016437     MOVE 'B-UPD-DDGS-SALDO' TO CURRENT-SECTION                           
016438                                                                          
016439     MOVE IN-IDARTNR         TO W-IDARTNR-A1                              
016440     MOVE IN-IDLEVNR         TO W-IDLEVNR-A1                              
016444                                                                          
016446     PERFORM IMS-GU-WDF2A1                                                
016447     IF SEGMENT-FINNS                                                     
016448        MOVE SEQA-IDLEVNR     TO W-IDLEVNR-WDF2                           
016449        MOVE SEQA-IDDIRGRP    TO W-IDDIRGRP-WDF2                          
016450        MOVE SEQA-IDARTNR     TO W-IDARTNR                                
016451        PERFORM IMS-GHU-WDF212                                            
016452        IF IN-KVLS-DLEV < ZERO                                            
016453           MOVE ZERO         TO ART-KVLS-DLEV                             
016454        ELSE                                                              
016455           MOVE IN-KVLS-DLEV TO ART-KVLS-DLEV                             
016456        END-IF                                                            
016457        MOVE IN-TIINLMOT     TO ART-TIINLMOT                              
016458        MOVE CURRENT-DATE    TO ART-TIREGDAT                              
016459        PERFORM IMS-REPL-WDF2                                             
016460     ELSE                                                                 
016461        IF NOT INTRO-WRITTEN                                              
016462           PERFORM S10-SKRIV-MAIL-INTRO                                   
016463           MOVE JA TO INTRO-WRITTEN-SW                                    
016464        END-IF                                                            
016465        MOVE IN-IDARTNR    TO MAIL-IDARTNR                                
016466        PERFORM S11-SKRIV-MAIL-ERROR                                      
016470     END-IF                                                               
016473     .                                                                    
016474                                                                          
016475                                                                          
016502 Z-FINIT SECTION.                                                         
016503     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
016506                                                                          
016507     CLOSE W46343                                                         
016508           W46344                                                         
016602                                                                          
016611     MOVE 'S'        TO POSTSUM-OPKOD                                     
016620     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016901                                                                          
016902                                                                          
016903 S01-LAES-W46343  SECTION.                                                
016904                                                                          
016905     READ W46343 INTO IN-AREA                                             
016906     AT END                                                               
016908        SET END-OF-W46343 TO TRUE                                         
016909                                                                          
016910     NOT AT END                                                           
016911        MOVE 'W46343'   TO POSTSUM-FDNAMN                                 
016912        MOVE 'W46341D1' TO POSTSUM-DDNAMN2                                
016913        MOVE '    '     TO POSTSUM-TRANSTYP                               
016914        CALL POSTSUM USING POSTSUM-PARM                                   
016915                                                                          
016916        ADD +1          TO POST-ANT                                       
016918     END-READ                                                             
016920     .                                                                    
017200                                                                          
017210                                                                          
017220 S10-SKRIV-MAIL-INTRO SECTION.                                            
017230                                                                          
017231                                                                          
017232     MOVE '¤DAPDDGSSTOCK'  TO KVITTO-AREA                                 
017233     PERFORM S20-SKRIV-W46344                                             
017234                                                                          
017235     MOVE SPACE            TO KVITTO-AREA                                 
017236     STRING '¤DAP' IN-IDLEVNR                                             
017237     DELIMITED BY SIZE   INTO KVITTO-AREA                                 
017238     PERFORM S20-SKRIV-W46344                                             
017239                                                                          
017240     MOVE MAIL-INTRO-1     TO KVITTO-AREA                                 
017241     PERFORM S20-SKRIV-W46344                                             
017242                                                                          
017243     MOVE IN-IDLEVNR       TO INTRO-IDLEVNR                               
017244     MOVE MAIL-INTRO-2     TO KVITTO-AREA                                 
017245     PERFORM S20-SKRIV-W46344                                             
017246                                                                          
017247     MOVE IN-DASUPREF      TO WS-DATUM-X                                  
017248     MOVE WS-DATUM         TO INTRO-DATE                                  
017249     MOVE IN-TISUPTID      TO INTRO-TIME                                  
017250     MOVE MAIL-INTRO-3     TO KVITTO-AREA                                 
017251     PERFORM S20-SKRIV-W46344                                             
017252                                                                          
017253     MOVE MAIL-BLANKRAD    TO KVITTO-AREA                                 
017254     PERFORM S20-SKRIV-W46344                                             
017255     .                                                                    
017256                                                                          
017257                                                                          
017258 S11-SKRIV-MAIL-ERROR SECTION.                                            
017259                                                                          
017260                                                                          
017272     MOVE IN-IDARTNR       TO MAIL-IDARTNR                                
017273     MOVE MAIL-ERROR       TO KVITTO-AREA                                 
017274     PERFORM S20-SKRIV-W46344                                             
017284     .                                                                    
017285                                                                          
017286                                                                          
017287 S12-SKRIV-MAIL-END   SECTION.                                            
017288                                                                          
017289     MOVE MAIL-BLANKRAD       TO KVITTO-AREA                              
017290     PERFORM S20-SKRIV-W46344                                             
017291                                                                          
017292     IF MAIL-IDARTNR = ZERO                                               
017293        MOVE MAIL-END-OK      TO KVITTO-AREA                              
017294        PERFORM S20-SKRIV-W46344                                          
017295     ELSE                                                                 
017296        MOVE MAIL-END-ERROR-1 TO KVITTO-AREA                              
017297        PERFORM S20-SKRIV-W46344                                          
017298        MOVE MAIL-END-ERROR-2 TO KVITTO-AREA                              
017299        PERFORM S20-SKRIV-W46344                                          
017300     END-IF                                                               
017301     .                                                                    
017302                                                                          
017303                                                                          
017304 S20-SKRIV-W46344 SECTION.                                                
017305                                                                          
017306     WRITE KVITTO-POST FROM KVITTO-AREA                                   
017307                                                                          
017308     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
017309     MOVE 'W46344'   TO POSTSUM-FDNAMN                                    
017310     MOVE 'W46341D2' TO POSTSUM-DDNAMN2                                   
017311     CALL POSTSUM USING POSTSUM-PARM                                      
017312     .                                                                    
017313                                                                          
017320                                                                          
017370 X-TAG-CHECKPOINT   SECTION.                                              
017400                                                                          
018000     PERFORM IMS-CHECKPOINT                                               
018100     MOVE ZERO TO CHKP-ANT                                                
018300     .                                                                    
018420                                                                          
018500* --- IMS SEKTIONER ---                                                   
018600                                                                          
018900 IMS-RESTART SECTION.                                                     
019000                                                                          
019100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019200     MOVE '  ' TO GODK-STATUSKODER                                        
019300     CALL CBLTDLI USING XRST MSG-PCB                                      
019400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019500                        CHKP-AREA-LENGTH CHKP-AREA                        
019600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019700     PERFORM IMS-STATUSKONTROLL                                           
019800     .                                                                    
019900                                                                          
019910                                                                          
020000 IMS-CHECKPOINT SECTION.                                                  
020100                                                                          
020200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020300     MOVE '  XD' TO GODK-STATUSKODER                                      
020400     CALL CBLTDLI USING CHKP MSG-PCB                                      
020500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020600                        CHKP-AREA-LENGTH CHKP-AREA                        
020700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020800     PERFORM IMS-STATUSKONTROLL                                           
020900                                                                          
021000     IF IMS-EJ-OK                                                         
021100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021200       DISPLAY FELTEXT                                                    
021300       CALL FELLOG                                                        
021400     END-IF                                                               
021500     .                                                                    
021600                                                                          
021601                                                                          
021641 IMS-GU-WDF2A1 SECTION.                                                   
021642     MOVE 'IMS-GU-WDF2A1   ' TO CURRENT-IMS-SECTION                       
021643                                                                          
021644     MOVE SPACE                 TO ALL-SSA                                
021645     STRING 'WDF2A1  (WDF2A1KY =' W-WDF2A1KY ')'                          
021646            DELIMITED BY SIZE INTO SSA1                                   
021647     MOVE '  GE' TO GODK-STATUSKODER                                      
021648     CALL CBLTDLI USING GU  WDF2A-PCB DLI-IO-WDF2A1 SSA1                  
021649     MOVE WDF2A-STATUS-CODE TO STATUS-WS                                  
021650     PERFORM IMS-STATUSKONTROLL                                           
021651     .                                                                    
021652                                                                          
021653                                                                          
021668 IMS-GHU-WDF212 SECTION.                                                  
021669     MOVE 'IMS-GHU-WDF212  ' TO CURRENT-IMS-SECTION                       
021670                                                                          
021671     MOVE SPACE                 TO ALL-SSA                                
021672     STRING 'WDF201  (WDF201KY =' W-WDF201KY-X ')'                        
021673          DELIMITED BY SIZE INTO SSA1                                     
021680     STRING 'WDF212  (IDARTNR  =' W-IDARTNR-X ')'                         
021690          DELIMITED BY SIZE INTO SSA2                                     
021691     MOVE '  ' TO GODK-STATUSKODER                                        
021692     CALL CBLTDLI USING GHU WDF2-PCB DLI-IO-WDF212 SSA1 SSA2              
021693     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
021694     PERFORM IMS-STATUSKONTROLL                                           
021695     .                                                                    
021696                                                                          
021697                                                                          
021698 IMS-REPL-WDF2 SECTION.                                                   
021699     MOVE 'IMS-REPL-WDF2   ' TO CURRENT-IMS-SECTION                       
021700                                                                          
021701     MOVE '  ' TO GODK-STATUSKODER                                        
021702     CALL CBLTDLI USING REPL WDF2-PCB DLI-IO-WDF212                       
021703     MOVE WDF2-STATUS-CODE TO STATUS-WS                                   
021704     PERFORM IMS-STATUSKONTROLL                                           
021705     ADD +1    TO CHKP-ANT                                                
021706     .                                                                    
021707                                                                          
021708                                                                          
021710 IMS-STATUSKONTROLL SECTION.                                              
021800                                                                          
021900     SET STATUS-IX TO 1                                                   
022000     SEARCH GODK-STATUS                                                   
022100       AT END                                                             
022200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022300           DELIMITED BY SIZE INTO FELTEXT                                 
022400         DISPLAY FELTEXT                                                  
022500         CALL FELLOG                                                      
022600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022700         CONTINUE                                                         
022800     END-SEARCH                                                           
022900     .                                                                    

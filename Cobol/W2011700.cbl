000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2011700.                                                
000300 AUTHOR.         RAHUL JAIN. UPPDATERINGSPROGRAM (MPP).                   
000400 DATE-WRITTEN.   NOVEMBER 2011.                                           
000500     REMARKS.                                                             
000600*    THIS PROGRAM IS A COPY OF W2011300 PROGRAM.                          
000700*    FUNKTION.                                                            
000800*       UPPDATERAR DFU-ANSLUTNA LEVERANTÖRER PÅ WDGX-BASEN.               
000900*       KDVECKOSL KAN UPPDATERAS.                                         
001000*       SAMT FL-PERIODSLUT OCH BEGÄRD (PERIOD-)SÄNDNINGSDATUM.            
001100*       IDHTYP=2205.                                                      
001200*                                                                         
001210*       PROGRAMMET UPPDATERAR WDR2  LEVERANTÖRSINFORMATION                
001211*                                   PLANER VIA EDI                        
001220*                                   HTYP=2205, SEGMENT=WDGX2206           
001221*                                                                         
001230*       PROGRAMMET LÄSER WDB6                                             
001240*                                                                         
001250*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W2T117                                              
001500*                     W2T117U                                             
001510*                                                                         
001600*        MID:         W2I11701                                            
001700*    UTDATA.                                                              
001800*        MOD:         W2O11701                                            
001900*    SUBPROGRAM.                                                          
002000*        FELLOG                                                           
002100*        CBLTDLI                                                          
002200*        WDATKONV                                                         
002210*                                                                         
002220*                                                                         
002230*    ÄNDRINGAR:                                                           
002240*    2012-09-03  E-TRACKER 10143273 LOCAL SOURCING CHINA                  
002250*                                                                         
002251*    2017-06-30  E-TRACKER 10302687 LOCAL SOURCING NA                     
002252*                REWRITE ALL SECTIONS WITHOUT G- AND H- SECTION           
002270*                                                                         
002300*    SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP3                                                                
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900*    -COPY WY2000W1                                                       
003000     SKIP3                                                                
003100 77  PROGRAM-NAMN                PIC X(08) VALUE 'W2011700'.              
003110 77  CURRENT-SECTION             PIC X(80) VALUE SPACE.                   
003120 77  IMS-SECTION                 PIC X(80) VALUE SPACE.                   
003310 77  JA                          PIC  X(01)   VALUE 'J'.                  
003400 77  NEJ                         PIC  X(01)   VALUE 'N'.                  
003410 77  YES                         PIC  X(01)   VALUE 'Y'.                  
003420 77  NOO                         PIC  X(01)   VALUE 'N'.                  
003500 77  OCH                         PIC  X(01)   VALUE '&'.                  
003610 77  INDX                        PIC S9(04)  VALUE +0   COMP SYNC.        
003700 77  WS-IDLEVNR-SPAR             PIC  X(05).                              
003800 77  WS-IDDC-SPAR                PIC  X(02)  VALUE SPACES.                
004000 77  MAX-IND                     PIC S9(09)  VALUE +10  COMP SYNC.        
004006                                                                          
004010 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004020     88  KEYS-OK                             VALUE 'J'.                   
004030     88  KEYS-WRONG                          VALUE 'N'.                   
004040                                                                          
004100 77  WS-IDTRANS                  PIC  X(04).                              
004200     88  OWN-MID                            VALUE '2117'.                 
004210     88  GOOD-MID                           VALUE '2111' '2112'           
004211                                                  '2113' '2114'           
004212                                                  '2115' '2116'           
004213                                                  '2117' '2118'.          
004220     88  HELP-MID                           VALUE '0551'.                 
004300                                                                          
004400*------------------------------- SWITCHAR                                 
004500 77  SW-INPUT-RAETT              PIC X(01)  VALUE 'J'.                    
004501                                                                          
004502 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
004503*01 -COPY WWIDFTG                                                         
004504     EJECT                                                                
004510*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
004520*01 -COPY WMEDAREA                                                        
004530     SKIP3                                                                
004540 01  MESSAGE-CODES.                                                       
004550     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
004560     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
004570     03  INF-PRESS-PF9           PIC X(3)    VALUE '127'.                 
004571     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
004572     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
004573     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
005145     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
005146     03  ERR-NOT-AUTH            PIC X(3)    VALUE '405'.                 
005147     03  ERR-WRONG-SEL-CODE      PIC X(3)    VALUE '416'.                 
005148     03  ERR-NO-LINE-SELECTED    PIC X(3)    VALUE '362'.                 
005149     EJECT                                                                
005150                                                                          
005151 01  DYNAMISKA-SUBPROGRAM.                                                
005152     03  WMEDKONV                PIC X(08)  VALUE 'WMEDKONV'.             
005153     03  W005INIT                PIC X(08)  VALUE 'W005INIT'.             
005154     03  CBLTDLI                 PIC X(08)  VALUE 'CBLTDLI '.             
005155     03  FELLOG                  PIC X(08)  VALUE 'FELLOG  '.             
005156     EJECT                                                                
005157                                                                          
005158*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
005159*                                                                         
005160 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
005161     SKIP3                                                                
005162*01 -COPY WMSGINIT                                                        
005163     EJECT                                                                
005164*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
005165*                                                                         
005166 01  SAVE-AREA.                                                           
005167     03  SAVE-IDTRANS             PIC X(4)   VALUE '2117'.                
005168     03  SAVE-IDLEVNR-ENTER       PIC X(05)  VALUE SPACE.                 
005169     03  SAVE-IDDC-ENTER          PIC X(02)  VALUE SPACE.                 
005170     03  SAVE-IDLEVNR-NEXT        PIC X(05)  VALUE SPACE.                 
005171     03  SAVE-IDDC-NEXT           PIC X(02)  VALUE SPACE.                 
005172     EJECT                                                                
005180 01  NYCKLAR-TILL-DLI.                                                    
005200      03  W-WDGXKEY-ROT.                                                  
005300          05  FILLER             PIC X(04)  VALUE '2205'.                 
005400          05  FILLER             PIC X(26)  VALUE LOW-VALUE.              
005500      03  W-WDGX2206-X.                                                   
005510          05  W-IDLEVNR          PIC X(5).                                
005520          05  W-IDDC             PIC X(2)   VALUE SPACES.                 
005921                                                                          
005922      03  W1-IDDC-MIN-X.                                                  
005925          05 W1-IDDC1-MIN        PIC  X(01).                              
005926          05 W1-IDDC2-MIN        PIC  X(01).                              
005927      03  W1-IDDC-MAX-X.                                                  
005929          05 FILLER              PIC  X(01).                              
005930          05 W1-IDDC2-MAX        PIC  X(01).                              
005932                                                                          
005933      03  W-WDGXKEY-IDDC.                                                 
005940          05  W-IDDC-B6          PIC X(2)   VALUE SPACES.                 
006000                                                                          
006200 01  MEDDELANDE.                                                          
006300     03  FEL-1                   PIC X(40) VALUE                          
006400            'PAGE 1 SHOWN, NO MORE SUPPLIERS         '.                   
006500     03  FEL-2                   PIC X(32) VALUE                          
006600            'HIGHLIGHTED FIELDS INCORRECT    '.                           
006700     03  FEL-3                   PIC X(40) VALUE                          
006800            'J DISALLOWED IN BOTH WEEKLY & PERIODBTCH'.                   
006810     03  FEL-4                   PIC X(40) VALUE                          
006820            'DC INPUT IS MISSING '.                                       
006900     03  MED-1                   PIC X(32) VALUE                          
007000            'MORE INFO ON NEXT PAGE          '.                           
007100     03  MED-2                   PIC X(32) VALUE                          
007200            'PRESS PF11 FOR UPDATE           '.                           
007300     03  MED-3                   PIC X(32) VALUE                          
007400            'UPDATE PERFORMED                '.                           
007500                                                                          
007600 01  FLAGGA-FEL3                 PIC X     VALUE SPACE.                   
007610 01  FLAGGA-FEL4                 PIC X     VALUE SPACE.                   
007700                                                                          
007800 01  DAGENS-DATUM                PIC 9(06).                               
007900                                                                          
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100     05  WDATKONV                PIC X(08)  VALUE 'WDATKONV'.             
008200     EJECT                                                                
008300*01  -COPY WDATAREA.                                                      
008400                                                                          
008500     EJECT                                                                
008600*                        ****    MFS OCH SKÄRMHANTERING                   
008700 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
008800     SKIP2                                                                
008900*01  MID -COPY W2I11701                                                   
009000     EJECT                                                                
009100 01  FILLER              PIC X(16)   VALUE 'WMSGAREA'.                    
009200     SKIP2                                                                
009300*01  -COPY WMSGAREA                                                       
009400     EJECT                                                                
009500*    03  MOD -COPY W2O11701  -RED MSG-AREA.                               
009600     EJECT                                                                
009700*01  -COPY WMFSAREA.                                                      
009800     EJECT                                                                
009900******************************************************************        
010000*****                                                                     
010100*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010200*****                                                                     
010300 01  IMS-WS.                                                              
010400     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
010500     SKIP3                                                                
010600*****                    **** STATUS-KOD FRÅN IMS                         
010700     03  STATUS-WS               PIC X(2).                                
010800         88  SEGMENT-FINNS                   VALUE '  '.                  
010900         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
011000     SKIP3                                                                
011100     03  GODK-STATUSKODER.                                                
011200         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(128).                              
011500     EJECT                                                                
011600*                            IMS FUNKTIONSKODER                           
011700*01  -COPY W0003                                                          
011800     EJECT                                                                
011900*                            DLI INPUT-OUTPUT AREA                        
012000 01  DLI-IO-AREA.                                                         
012100     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
012200     SKIP3                                                                
012300*    03  WDR2   -COPY WDGX2206    -PRE WDR2-   -RED IO-AREA.              
012400     EJECT                                                                
012410*                                                                         
012420 01  FILLER                PIC X(16) VALUE 'DLI-IO-WDB601'.               
012430 01  DLI-IO-WDB601.                                                       
012500*    03  -COPY WDB601   -PRE WDB601-                                      
012600     EJECT                                                                
012700 LINKAGE SECTION.                                                         
012800     SKIP2                                                                
012900*01  -COPY W0009     -PRE MSG-                                            
013000     EJECT                                                                
013310*01  -COPY W0008     -PRE WDP7-                                           
013320         05  FILLER              PIC X.                                   
013330     EJECT                                                                
013340*01  -COPY W0008     -PRE WDR2-                                           
013350         05  FILLER              PIC X.                                   
013360     EJECT                                                                
013400*01  -COPY W0008     -PRE WDB6-                                           
013500         05  FILLER              PIC X.                                   
013600     EJECT                                                                
013700 PROCEDURE DIVISION USING  MSG-PCB WDP7-PCB WDR2-PCB WDB6-PCB.            
013800     SKIP1                                                                
013900     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDR2-PCB WDB6-PCB.            
014000                                                                          
014100     PERFORM IMS-GET-MSG                                                  
014200     IF SEGMENT-FINNS                                                     
014300        PERFORM A-INIT                                                    
014310        PERFORM B-CHECK-KEYS                                              
014320        IF KEYS-OK                                                        
014322           IF MFS-UPDATE                                                  
014324              PERFORM G-CHECK-INPUT                                       
014325              IF SW-INPUT-RAETT = JA                                      
014327                 PERFORM H-UPDATE                                         
014328              END-IF                                                      
014329           ELSE                                                           
014331              IF MFS-FIRST                                                
014340                 PERFORM C-FIRST-PAGE                                     
014350              ELSE                                                        
014360                 IF MFS-NEXT                                              
014370                    PERFORM D-NEXT-PAGE                                   
014380                 ELSE                                                     
014390                    PERFORM E-SAME-PAGE                                   
014391                 END-IF                                                   
014392              END-IF                                                      
014397           END-IF                                                         
014398           PERFORM F-READ-SHOW-INFO                                       
014399        END-IF                                                            
016600                                                                          
016710        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O11701 + 4                     
016800        PERFORM IMS-INSERT-MSG                                            
016900     END-IF                                                               
017000                                                                          
017100     MOVE ZERO TO RETURN-CODE                                             
017200     GOBACK                                                               
017300     .                                                                    
017400     EJECT                                                                
017500 A-INIT SECTION.                                                          
017510     MOVE 'A-INIT            '  TO CURRENT-SECTION                        
017600     SKIP2                                                                
017700     IF MSG-DUBBLA-TRANSKODER                                             
017800         MOVE MSG-INDATA-MINUS-2-TRANSKODER                               
017900                                   TO MID-W2I11701                        
018000         MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                         
018200         MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                        
018500     ELSE                                                                 
018600         MOVE MSG-INDATA-MINUS-1-TRANSKOD                                 
018700                                   TO MID-W2I11701                        
018800         MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                         
019000         MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                        
019300     END-IF                                                               
019400                                                                          
019410     MOVE MSG-KDTRTYP              TO MFS-KDTRTYP                         
019420     MOVE MSG-IDPFK                TO MFS-IDPFK                           
019421     MOVE MFS-IDTRANS              TO WS-IDTRANS                          
019430                                                                          
019500     ACCEPT DAGENS-DATUM         FROM DATE                                
019600                                                                          
019700     MOVE LOW-VALUE                TO MOD-W2O11701                        
019800     MOVE 'W2O117N1'               TO MFS-IDMOD                           
019900     MOVE '2117'                   TO MOD-IDTRANS                         
020000                                                                          
020100     MOVE MFS-RENSA-FAELT          TO MOD-TEMFSFEL                        
020200                                      MOD-TEMFSINF                        
020310                                                                          
020320     IF OWN-MID OR HELP-MID                                               
020330        CONTINUE                                                          
020340     ELSE                                                                 
020350        MOVE SPACE                TO MFS-KDTRTYP                          
020360        MOVE '7'                  TO MFS-IDPFK                            
020370     END-IF                                                               
020900     .                                                                    
021000     EJECT                                                                
021001 B-CHECK-KEYS SECTION.                                                    
021002     MOVE 'B-CHECK-KEYS      '  TO CURRENT-SECTION                        
021013                                                                          
021014     MOVE ALL '+'                TO MSGI-WMSGINIT                         
021015     MOVE '001'                  TO MSGI-KDCALL                           
021016     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
021017     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
021018     MOVE '2117'                 TO MSGI-IDTRANS                          
021019     IF OWN-MID                                                           
021020       MOVE MID-IDLEVNR-IN   TO MSGI-IDLEVNR                              
021021       MOVE MID-IDDC-IN      TO MSGI-IDDC-KEY                             
021028     END-IF                                                               
021029                                                                          
021030     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021031     IF MSGI-SPAR-AREA (1:4) = '2117'                                     
021032        MOVE MSGI-SPAR-AREA  TO SAVE-AREA                                 
021037     END-IF                                                               
021042                                                                          
021043*    - LANGUAGE TO BE USED BY MEDKONV                                     
021044     MOVE MSGI-IDLAND-SPR    TO MED-IDSKYLT                               
021045                                                                          
021046     MOVE JA                 TO KEYS-SW                                   
021047                                                                          
021048*    -- KONTROLL AV IDLEVNR                                               
021049     MOVE MFS-RENSA-FAELT    TO MOD-IDLEVNR-IN                            
021050     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
021051        MOVE '7'             TO MFS-IDPFK                                 
021052        MOVE SPACE           TO MFS-KDTRTYP                               
021053     END-IF                                                               
021054     MOVE MSGI-IDLEVNR       TO W-IDLEVNR                                 
021056                                                                          
021057*    -- KONTROLL AV IDDC                                                  
021058     MOVE MFS-RENSA-FAELT    TO MOD-IDDC-IN                               
021059     IF MID-IDDC-IN NOT = ALL '+'                                         
021060        MOVE '7'             TO MFS-IDPFK                                 
021061        MOVE SPACE           TO MFS-KDTRTYP                               
021062     END-IF                                                               
021063                                                                          
021064     MOVE MSGI-IDDC-KEY      TO W-IDDC                                    
021065                                                                          
021066     IF W-IDDC = SPACE                                                    
021067        MOVE MSGI-IDFTG      TO WS-IDFTG                                  
021068        IF IDFTG-US                                                       
021069           MOVE '41'         TO W1-IDDC-MIN-X                             
021070           MOVE '49'         TO W1-IDDC-MAX-X                             
021071           MOVE '40'         TO MSGI-IDDC-KEY                             
021072        ELSE                                                              
021073           IF IDFTG-CN                                                    
021074              MOVE '71'      TO W1-IDDC-MIN-X                             
021075              MOVE '79'      TO W1-IDDC-MAX-X                             
021076              MOVE '70'      TO MSGI-IDDC-KEY                             
021077           ELSE                                                           
021078              MOVE NOO       TO KEYS-SW                                   
021079           END-IF                                                         
021080        END-IF                                                            
021081     ELSE                                                                 
021082        MOVE W-IDDC          TO W1-IDDC-MIN-X                             
021083                                W1-IDDC-MAX-X                             
021084        IF W-IDDC(2:1) = '0'                                              
021085           MOVE '1'          TO W1-IDDC2-MIN                              
021086           MOVE '9'          TO W1-IDDC2-MAX                              
021087        END-IF                                                            
021088     END-IF                                                               
021089                                                                          
021090     IF GOOD-MID OR KEYS-OK                                               
021091       MOVE MSGI-IDLEVNR     TO MOD-IDLEVNR-UT                            
021092       MOVE MSGI-IDDC-KEY    TO MOD-IDDC-UT                               
021094     ELSE                                                                 
021095       MOVE MFS-ERASE-FIELD  TO MOD-IDLEVNR-UT                            
021096                                MOD-IDDC-UT                               
021098     END-IF                                                               
021099                                                                          
021100     IF KEYS-WRONG                                                        
021101       MOVE ERR-WRONG-KEY    TO MED-IDMFSFEL                              
021102       CALL WMEDKONV USING MED-WMEDAREA                                   
021103       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
021104       PERFORM MFS-ERASE-FIELD-IN                                         
021105       PERFORM MFS-ERASE-FIELD-OUT                                        
021106     END-IF                                                               
021107     .                                                                    
021108     EJECT                                                                
021138 C-FIRST-PAGE SECTION.                                                    
021139     MOVE 'C-FIRST-PAGE      '  TO CURRENT-SECTION                        
021140                                                                          
021141     MOVE INF-FIRST-PAGE         TO MED-IDMFSINF                          
021142     CALL WMEDKONV            USING MED-WMEDAREA                          
021143     MOVE MED-MFSINF             TO MOD-TEMFSFEL                          
021144     PERFORM MFS-ERASE-FIELD-IN                                           
021145     PERFORM MFS-ERASE-FIELD-OUT                                          
021146     .                                                                    
021147     EJECT                                                                
021148 D-NEXT-PAGE SECTION.                                                     
021149     MOVE 'D-NEXT-PAGE      '  TO CURRENT-SECTION                         
021150                                                                          
021151     IF SAVE-IDTRANS = '2117'                                             
021152        MOVE SAVE-IDLEVNR-NEXT  TO W-IDLEVNR                              
021153        MOVE SAVE-IDDC-NEXT     TO W-IDDC                                 
021154     ELSE                                                                 
021155        PERFORM MFS-ERASE-FIELD-IN                                        
021156     END-IF                                                               
021157     .                                                                    
021158     EJECT                                                                
021159 E-SAME-PAGE SECTION.                                                     
021160     MOVE 'E-SAME-PAGE       '  TO CURRENT-SECTION                        
021161                                                                          
021162     IF SAVE-IDTRANS = '2117' OR '0551'                                   
021163        MOVE SAVE-IDLEVNR-ENTER TO W-IDLEVNR                              
021164        MOVE SAVE-IDDC-ENTER    TO W-IDDC                                 
021165        IF MID-INFO-RAD (01)= ALL '+'                                     
021166       AND MID-INFO-RAD (02)= ALL '+'                                     
021167       AND MID-INFO-RAD (03)= ALL '+'                                     
021168       AND MID-INFO-RAD (04)= ALL '+'                                     
021169       AND MID-INFO-RAD (05)= ALL '+'                                     
021170       AND MID-INFO-RAD (06)= ALL '+'                                     
021171       AND MID-INFO-RAD (07)= ALL '+'                                     
021172       AND MID-INFO-RAD (08)= ALL '+'                                     
021173       AND MID-INFO-RAD (09)= ALL '+'                                     
021174       AND MID-INFO-RAD (10)= ALL '+'                                     
021175       AND MID-IDLEVNR      = ALL '+'                                     
021176       AND MID-IDDC         = ALL '+'                                     
021177       AND MID-KDVECKOSL    = ALL '+'                                     
021180           PERFORM MFS-ERASE-FIELD-IN                                     
021181        ELSE                                                              
021182           MOVE INF-PRESS-PF11  TO MED-IDMFSINF                           
021183           CALL WMEDKONV     USING MED-WMEDAREA                           
021184           MOVE MED-MFSINF      TO MOD-TEMFSFEL                           
021185           PERFORM EA-MID-INDATA-FOR-MOD                                  
021186        END-IF                                                            
021187     ELSE                                                                 
021188        PERFORM MFS-ERASE-FIELD-IN                                        
021189     END-IF                                                               
021190     .                                                                    
021191     EJECT                                                                
021192 EA-MID-INDATA-FOR-MOD SECTION.                                           
021193     MOVE 'EA-MID-INDATA-FOR-MOD' TO CURRENT-SECTION                      
021194                                                                          
021195     IF MID-IDLEVNR NOT = ALL '+'                                         
021196        MOVE MID-IDLEVNR             TO MOD-IDLEVNR-UPD                   
021197        MOVE MFS-ADD-READ-FIELD      TO MOD-IDLEVNR-UPD-ATTR              
021198        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDLEVNR-UPD                   
021199     ELSE                                                                 
021200        MOVE MFS-ERASE-FIELD         TO MOD-IDLEVNR-UPD-ATTR              
021201     END-IF                                                               
021202                                                                          
021203     IF MID-IDDC NOT = ALL '+'                                            
021204        MOVE MID-IDDC                TO MOD-IDDC-UPD                      
021205        MOVE MFS-ADD-READ-FIELD      TO MOD-IDDC-UPD-ATTR                 
021206        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDC-UPD                      
021207     ELSE                                                                 
021208        MOVE MFS-ERASE-FIELD         TO MOD-IDDC-UPD-ATTR                 
021209     END-IF                                                               
021210                                                                          
021211     IF MID-KDVECKOSL NOT = ALL '+'                                       
021212        MOVE MID-KDVECKOSL           TO MOD-KDVECKOSL-UPD                 
021213        MOVE MFS-ADD-READ-FIELD      TO MOD-KDVECKOSL-UPD-ATTR            
021214        MOVE MFS-ROER-EJ-FAELT       TO MOD-KDVECKOSL-UPD                 
021215     ELSE                                                                 
021216        MOVE MFS-ERASE-FIELD         TO MOD-KDVECKOSL-UPD-ATTR            
021217     END-IF                                                               
021218                                                                          
021269     .                                                                    
021270     EJECT                                                                
032100 G-CHECK-INPUT SECTION.                                                   
032110     MOVE 'G-CHECK-INPUT        ' TO CURRENT-SECTION                      
032200                                                                          
032201     MOVE JA                             TO SW-INPUT-RAETT                
032202                                                                          
032203     PERFORM GA-CHECK-DC-FTG-USER                                         
032204                                                                          
032205     IF SW-INPUT-RAETT = JA                                               
032206       PERFORM GB-KOLLA-INPUT                                             
032207                                                                          
032208       IF SW-INPUT-RAETT = JA                                             
032210         PERFORM IMS-GET-WDR201                                           
032300         MOVE MID-IDLEVNR                TO W-IDLEVNR                     
032400         MOVE MID-IDDC                   TO W-IDDC                        
032500         PERFORM IMS-GET-WDGX2206-UNIK                                    
032510         IF SEGMENT-FINNS                                                 
032520           MOVE MFS-ALFA-FAELT-RAETT     TO MOD-IDLEVNR-UPD-ATTR          
032530                                            MOD-IDDC-UPD-ATTR             
032701         ELSE                                                             
032702           MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDLEVNR-UPD-ATTR          
032703                                            MOD-IDDC-UPD-ATTR             
032704           MOVE NEJ                      TO SW-INPUT-RAETT                
032706         END-IF                                                           
032707       END-IF                                                             
032800                                                                          
032900       IF SW-INPUT-RAETT = NEJ                                            
033000          PERFORM S02-ROER-EJ-FAELT                                       
033104          IF FLAGGA-FEL3 = JA                                             
033105             MOVE FEL-3 TO MOD-TEMFSFEL                                   
033106          ELSE                                                            
033108             MOVE FEL-2 TO MOD-TEMFSFEL                                   
033109          END-IF                                                          
033111       END-IF                                                             
033112     END-IF                                                               
033113                                                                          
033200     .                                                                    
033300     EJECT                                                                
033310 GA-CHECK-DC-FTG-USER SECTION.                                            
033311     MOVE 'GA-CHECK-DC-FTG-USER ' TO CURRENT-SECTION                      
033320                                                                          
033321     IF MID-IDDC = ALL '+'                                                
033322       PERFORM S02-ROER-EJ-FAELT                                          
033323       MOVE FEL-4                   TO MOD-TEMFSFEL                       
033324       MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDDC-UPD-ATTR                  
033326       MOVE NEJ                     TO SW-INPUT-RAETT                     
033328     ELSE                                                                 
033330       MOVE MID-IDDC                TO W-IDDC-B6                          
033340       PERFORM IMS-GU-WDB601                                              
033350       IF SEGMENT-FINNS                                                   
033360          IF  WDB601-DCS-NDC-CN                                           
033370          OR (WDB601-DCS-NDC-NA AND WDB601-DCS-USA)                       
033380             IF (MSGI-IDDC(1:1) = WDB601-DCS-IDDC(1:1) AND                
033381                 MSGI-IDFTG     = WDB601-DCS-IDFTG)                       
033382             OR MSGI-IDFTG      = +57                                     
033392                CONTINUE                                                  
033393             ELSE                                                         
033395                MOVE NEJ            TO SW-INPUT-RAETT                     
033397             END-IF                                                       
033398          ELSE                                                            
033399             MOVE NEJ               TO SW-INPUT-RAETT                     
033401          END-IF                                                          
033402       ELSE                                                               
033403          MOVE NEJ                  TO SW-INPUT-RAETT                     
033405       END-IF                                                             
033414       IF SW-INPUT-RAETT = NEJ                                            
033415          PERFORM S02-ROER-EJ-FAELT                                       
033416          MOVE ERR-NOT-AUTH         TO MED-IDMFSFEL                       
033417          CALL WMEDKONV          USING MED-WMEDAREA                       
033418          MOVE MED-MFSFEL           TO MOD-TEMFSFEL                       
033419       END-IF                                                             
033420     END-IF                                                               
033421     .                                                                    
033422     EJECT                                                                
033423 GB-KOLLA-INPUT SECTION.                                                  
033424     MOVE 'GB-KOLLA-INPUT       ' TO CURRENT-SECTION                      
033425                                                                          
033430     MOVE JA   TO SW-INPUT-RAETT                                          
033440     MOVE NEJ  TO FLAGGA-FEL4                                             
033450                                                                          
033460     SET MID-INFO-IND TO 1                                                
033470****                                                                      
033480* SÄTT UPP INDEX MED 1 SÅ LÄNGE:                                          
033490*  -INDEX < MAX   OCH                                                     
033491*  -INFO-IDLEVNR (INDEX) ÄR NUMERISKT  OCH                                
033492*  -INMATAT IDLEVNR INTE ÄR LIKA MED INFO-IDLEVNR (INDEX)                 
033493****                                                                      
033494     MOVE MID-INFO-IDLEVNR (MID-INFO-IND) TO WS-IDLEVNR-SPAR              
033495     MOVE MID-INFO-IDDC (MID-INFO-IND)    TO WS-IDDC-SPAR                 
033498     PERFORM UNTIL MID-INFO-IND = MAX-IND OR                              
033499             (MID-IDLEVNR = MID-INFO-IDLEVNR (MID-INFO-IND) AND           
033500             MID-IDDC = MID-INFO-IDDC (MID-INFO-IND))                     
033501                                                                          
033502       SET MID-INFO-IND UP BY 1                                           
033503       MOVE MID-INFO-IDLEVNR (MID-INFO-IND) TO WS-IDLEVNR-SPAR            
033504       MOVE MID-INFO-IDDC (MID-INFO-IND)    TO WS-IDDC-SPAR               
033507     END-PERFORM                                                          
033508     IF MID-INFO-IND < MAX-IND                                            
033509     OR MID-INFO-IND = MAX-IND                                            
033510        MOVE MFS-ALFA-FAELT-RAETT     TO MOD-IDLEVNR-UPD-ATTR             
033511                                         MOD-IDDC-UPD-ATTR                
033512     ELSE                                                                 
033513        MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDLEVNR-UPD-ATTR             
033514                                         MOD-IDDC-UPD-ATTR                
033515        MOVE NEJ                      TO SW-INPUT-RAETT                   
033517     END-IF                                                               
033518     MOVE MFS-ROER-EJ-FAELT           TO MOD-IDLEVNR-UPD                  
033519                                         MOD-IDDC-UPD                     
033520                                                                          
033521     IF MID-KDVECKOSL NOT = ALL '+'                                       
033522        IF MID-KDVECKOSL = 'D' OR 'N'                                     
033523           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDVECKOSL-UPD-ATTR            
033524        ELSE                                                              
033525           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDVECKOSL-UPD-ATTR            
033526           MOVE NEJ                  TO SW-INPUT-RAETT                    
033528        END-IF                                                            
033529        MOVE MFS-ROER-EJ-FAELT       TO MOD-KDVECKOSL-UPD                 
033530     ELSE                                                                 
033531        MOVE MFS-RENSA-FAELT         TO MOD-KDVECKOSL-UPD                 
033532     END-IF                                                               
033544                                                                          
033572     IF MID-IDDC NOT = ALL '+'                                            
033573        MOVE MID-IDDC                TO W-IDDC-B6                         
033574        PERFORM IMS-GU-WDB601                                             
033575        IF SEGMENT-FINNS AND                                              
033576          (WDB601-DCS-NDC-CN OR WDB601-DCS-NDC-NA)                        
033577          CONTINUE                                                        
033578        ELSE                                                              
033579           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-UPD-ATTR                 
033580           MOVE NEJ                  TO SW-INPUT-RAETT                    
033581        END-IF                                                            
033582     ELSE                                                                 
033583        MOVE FEL-4                   TO MOD-TEMFSFEL                      
033584        MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDDC-UPD-ATTR                 
033585        MOVE JA                      TO FLAGGA-FEL4                       
033586        MOVE NEJ                     TO SW-INPUT-RAETT                    
033587     END-IF                                                               
033588                                                                          
033589     IF MID-KDVECKOSL  = ALL '+'                                          
033591        MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDVECKOSL-UPD-ATTR            
033593        MOVE NEJ                     TO SW-INPUT-RAETT                    
033595     END-IF                                                               
033596                                                                          
033656     IF SW-INPUT-RAETT = NEJ                                              
033657        PERFORM S02-ROER-EJ-FAELT                                         
033658        IF FLAGGA-FEL4 = JA                                               
033659           MOVE FEL-4 TO MOD-TEMFSFEL                                     
033660        ELSE                                                              
033661           MOVE FEL-2 TO MOD-TEMFSFEL                                     
033662        END-IF                                                            
033663     END-IF                                                               
033664     .                                                                    
033665     EJECT                                                                
036700 H-UPDATE SECTION.                                                        
036710     MOVE 'H-UPDATE             ' TO CURRENT-SECTION                      
036800                                                                          
036900     PERFORM S02-ROER-EJ-FAELT                                            
037000                                                                          
037100     SET MOD-INFO-IND TO MID-INFO-IND                                     
037200                                                                          
037300     IF MID-KDVECKOSL NOT = ALL '+'                                       
037610        MOVE MID-KDVECKOSL      TO WDR2-2206-KDVECKOSL                    
037620                                   MOD-INFO-KDVECKOSL                     
037630                                       (MOD-INFO-IND)                     
037810        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-INFO-KDVECKOSL-ATTR(01)         
037811        IF WDR2-2206-KDVECKOSL   = 'D'                                    
037812           MOVE 'J'             TO WDR2-2206-FLLEVVB                      
037813        ELSE                                                              
037814           MOVE 'N'             TO WDR2-2206-FLLEVVB                      
037815        END-IF                                                            
037820        MOVE NEJ                TO WDR2-2206-FLLEVPLP                     
037900     END-IF                                                               
038000                                                                          
039700     PERFORM IMS-REPLACE                                                  
039800                                                                          
039900                                                                          
040000     MOVE MFS-RENSA-FAELT            TO MOD-IDLEVNR-UPD                   
040010                                        MOD-IDDC-UPD                      
040100                                        MOD-KDVECKOSL-UPD                 
040400                                                                          
040500     MOVE MFS-FORMATETS-ATTR         TO MOD-IDLEVNR-UPD-ATTR              
040600                                        MOD-IDDC-UPD-ATTR                 
040610                                        MOD-KDVECKOSL-UPD-ATTR            
040900     MOVE MED-3                      TO MOD-TEMFSINF                      
040901                                                                          
040904     IF MID-IDDC NOT = W1-IDDC-MIN-X                                      
040911        IF (MID-IDDC = MSGI-IDDC-KEY)                                     
040912        OR (MSGI-IDDC-KEY(2:1) = '0' AND                                  
040913            MID-IDDC(1:1) = MSGI-IDDC-KEY(1:1))                           
040917           CONTINUE                                                       
040918        ELSE                                                              
040920           MOVE MID-IDDC       TO W1-IDDC-MIN-X                           
040930                                  W1-IDDC-MAX-X                           
040940        END-IF                                                            
040950                                                                          
040960        MOVE ALL '+'           TO MSGI-WMSGINIT                           
040970        MOVE '001'             TO MSGI-KDCALL                             
040980        MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                       
040990        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
040991        MOVE '2117'            TO MSGI-IDTRANS                            
040992        MOVE MID-IDDC          TO MSGI-IDDC-KEY                           
040993                                                                          
040994        CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                        
040995        MOVE MSGI-SPAR-AREA TO SAVE-AREA                                  
040996                                                                          
040997        MOVE MSGI-IDDC-KEY     TO MOD-IDDC-UT                             
040999     END-IF                                                               
041000                                                                          
041001     MOVE MID-IDLEVNR          TO W-IDLEVNR                               
041002     MOVE MID-IDDC             TO W-IDDC                                  
041010     .                                                                    
041100     EJECT                                                                
041200 F-READ-SHOW-INFO SECTION.                                                
041210     MOVE 'F-READ-SHOW-INFO     ' TO CURRENT-SECTION                      
041311                                                                          
041312     PERFORM IMS-GET-WDR201                                               
041313                                                                          
041370     PERFORM IMS-GNP-WDGX2206-MIN-MAX                                     
042800                                                                          
042900     SET MOD-INFO-IND             TO 1                                    
043000                                                                          
043010     IF SEGMENT-FINNS                                                     
043011        MOVE WDR2-2206-IDLEVNR    TO SAVE-IDLEVNR-ENTER                   
043012                                     W-IDLEVNR                            
043013        MOVE WDR2-2206-IDDC       TO SAVE-IDDC-ENTER                      
043014                                     W-IDDC                               
043020     END-IF                                                               
043030                                                                          
043110     PERFORM UNTIL MOD-INFO-IND > MAX-IND                                 
043200        IF SEGMENT-FINNS                                                  
043900           MOVE WDR2-2206-IDLEVNR     TO MOD-INFO-IDLEVNR                 
044000                                     (MOD-INFO-IND)                       
044100           MOVE WDR2-2206-IDDC        TO MOD-INFO-IDDC                    
044200                                     (MOD-INFO-IND)                       
044300           MOVE WDR2-2206-IDOVERFNR TO MOD-INFO-IDOVERFNR                 
044400                                     (MOD-INFO-IND)                       
044602           MOVE WDR2-2206-KDVECKOSL  TO MOD-INFO-KDVECKOSL                
044603                                             (MOD-INFO-IND)               
045600           MOVE WDR2-2206-TISEND-SEN TO MOD-INFO-TISEND-SEN               
045700                                     (MOD-INFO-IND)                       
045710                                                                          
046179           PERFORM IMS-GNP-WDGX2206-MIN-MAX                               
046300        ELSE                                                              
046400           MOVE MFS-RENSA-FAELT TO MOD-INFO-IDLEVNR                       
046500                                     (MOD-INFO-IND)                       
046510                                   MOD-INFO-IDDC                          
046520                                     (MOD-INFO-IND)                       
046600                                   MOD-INFO-IDOVERFNR                     
046700                                     (MOD-INFO-IND)                       
047000                                   MOD-INFO-KDVECKOSL                     
047100                                     (MOD-INFO-IND)                       
047200                                   MOD-INFO-TISEND-SEN                    
047300                                     (MOD-INFO-IND)                       
047800        END-IF                                                            
047900                                                                          
048000        SET MOD-INFO-IND UP BY 1                                          
048100     END-PERFORM                                                          
050500                                                                          
050510     IF SEGMENT-FINNS                                                     
050511        MOVE WDR2-2206-IDLEVNR TO SAVE-IDLEVNR-NEXT                       
050512        MOVE WDR2-2206-IDDC    TO SAVE-IDDC-NEXT                          
050513        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
050514        CALL WMEDKONV       USING MED-WMEDAREA                            
050515        MOVE MED-TEMFSINF      TO MOD-TEMFSINF                            
050518     ELSE                                                                 
050519        MOVE SAVE-IDLEVNR-ENTER TO SAVE-IDLEVNR-NEXT                      
050520        MOVE SAVE-IDDC-ENTER    TO SAVE-IDDC-NEXT                         
050528     END-IF                                                               
050529                                                                          
050530     MOVE '002'             TO MSGI-KDCALL                                
050531     MOVE '2117'            TO SAVE-IDTRANS                               
050540     MOVE SAVE-AREA         TO MSGI-SPAR-AREA                             
050550     CALL W005INIT       USING MSGI-WMSGINIT WDP7-PCB                     
050600     .                                                                    
050700     EJECT                                                                
053000 S02-ROER-EJ-FAELT SECTION.                                               
053010     MOVE 'S02-ROER-EJ-FAELT    ' TO CURRENT-SECTION                      
053100     SKIP3                                                                
053400     MOVE MFS-ROER-EJ-FAELT      TO MOD-IDLEVNR-UPD                       
053401                                    MOD-IDDC-UPD                          
053402                                    MOD-KDVECKOSL-UPD                     
053405                                                                          
053406     SET MOD-INFO-IND                TO 1                                 
053407                                                                          
053410     PERFORM UNTIL MOD-INFO-IND > MAX-IND                                 
053500        MOVE MFS-ROER-EJ-FAELT  TO MOD-INFO-IDLEVNR                       
053600                                   (MOD-INFO-IND)                         
053700                                   MOD-INFO-IDDC                          
053800                                   (MOD-INFO-IND)                         
053900                                   MOD-INFO-IDOVERFNR                     
054000                                   (MOD-INFO-IND)                         
054100                                   MOD-INFO-KDVECKOSL                     
054200                                   (MOD-INFO-IND)                         
054300                                   MOD-INFO-TISEND-SEN                    
054400                                   (MOD-INFO-IND)                         
054900        SET MOD-INFO-IND UP BY 1                                          
055000     END-PERFORM                                                          
055001                                                                          
055010     MOVE SAVE-IDLEVNR-ENTER    TO W-IDLEVNR                              
055020     MOVE SAVE-IDDC-ENTER       TO W-IDDC                                 
055100     .                                                                    
055200     EJECT                                                                
055210 MFS-ERASE-FIELD-OUT SECTION.                                             
055211     MOVE 'MFS-ERASE-FIELD-OUT' TO CURRENT-SECTION                        
055220                                                                          
055230*    --- ALLA UTDATA-FÄLT                                                 
055240*    --- INCL. SCROLL KEYS                                                
055250                                                                          
055292*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
055293     SET MOD-INFO-IND        TO 1                                         
055296     PERFORM UNTIL MOD-INFO-IND > MAX-IND                                 
055297       MOVE MFS-ERASE-FIELD  TO MOD-INFO-IDLEVNR    (MOD-INFO-IND)        
055298                                MOD-INFO-IDDC       (MOD-INFO-IND)        
055299                                MOD-INFO-IDOVERFNR  (MOD-INFO-IND)        
055300                                MOD-INFO-KDVECKOSL  (MOD-INFO-IND)        
055301                                MOD-INFO-TISEND-SEN (MOD-INFO-IND)        
055304        SET MOD-INFO-IND UP BY 1                                          
055305     END-PERFORM                                                          
055306     .                                                                    
055307     SKIP3                                                                
055308 MFS-ERASE-FIELD-IN SECTION.                                              
055309     MOVE 'MFS-ERASE-FIELD-IN'  TO CURRENT-SECTION                        
055311                                                                          
055312*    --- ALLA INDATA-FÄLT                                                 
055313     MOVE MFS-ERASE-FIELD    TO MOD-IDLEVNR-UPD                           
055314                                MOD-IDDC-UPD                              
055315                                MOD-KDVECKOSL-UPD                         
055318                                                                          
055319     SET MOD-INFO-IND        TO 1                                         
055320     PERFORM UNTIL MOD-INFO-IND > MAX-IND                                 
055321       MOVE MFS-ERASE-FIELD  TO MOD-INFO-IDLEVNR    (MOD-INFO-IND)        
055322                                MOD-INFO-IDDC       (MOD-INFO-IND)        
055323                                MOD-INFO-IDOVERFNR  (MOD-INFO-IND)        
055324                                MOD-INFO-KDVECKOSL  (MOD-INFO-IND)        
055325                                MOD-INFO-TISEND-SEN (MOD-INFO-IND)        
055328        SET MOD-INFO-IND UP BY 1                                          
055329     END-PERFORM                                                          
055330     .                                                                    
055331     EJECT                                                                
055410* IMS SEKTIONER                                                           
055500     SKIP3                                                                
055600 IMS-GET-MSG SECTION.                                                     
055700                                                                          
055800     MOVE '  QC' TO GODK-STATUSKODER                                      
055900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
056000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056100     PERFORM IMS-STATUS-KONTROLL                                          
056200     SKIP3                                                                
056300     .                                                                    
056400 IMS-INSERT-MSG SECTION.                                                  
056500     SKIP2                                                                
056600     IF ENGLISH-TEXT                                                      
056700        MOVE 'N' TO MFS-KDHUVOMR                                          
056800     END-IF                                                               
056900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
057000     MOVE SPACE TO GODK-STATUSKODER                                       
057100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
057200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057300     PERFORM IMS-STATUS-KONTROLL                                          
057400     .                                                                    
057500     EJECT                                                                
057600 IMS-GET-WDR201 SECTION.                                                  
057610     MOVE 'IMS-GET-WDR201    '  TO IMS-SECTION                            
057700                                                                          
057800     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-ROT ')'                       
057900             DELIMITED BY SIZE INTO SSA1                                  
058000     MOVE '  ' TO GODK-STATUSKODER                                        
058100     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-AREA SSA1                     
058200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
058300     PERFORM IMS-STATUS-KONTROLL                                          
058500     .                                                                    
058510                                                                          
058600 IMS-GNP-WDGX2206-MIN-MAX SECTION.                                        
058700     MOVE 'IMS-GNP-WDGX2206-MIN-MAX' TO IMS-SECTION.                      
058800                                                                          
058900     STRING 'WDGX2206(KY2206  =>' W-WDGX2206-X                            
059000                 OCH 'IDDC    >=' W1-IDDC-MIN-X                           
059100                 OCH 'IDDC    <=' W1-IDDC-MAX-X ')'                       
059200             DELIMITED BY SIZE INTO SSA1                                  
059300     MOVE '  GE' TO GODK-STATUSKODER                                      
059400     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA SSA1                     
059500     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
059600     PERFORM IMS-STATUS-KONTROLL                                          
059620     .                                                                    
059632                                                                          
059700 IMS-GET-WDGX2206-UNIK SECTION.                                           
059710     MOVE 'IMS-GET-WDGX2206-UNIK'  TO IMS-SECTION                         
059800                                                                          
059910     STRING 'WDGX2206(KY2206   =' W-WDGX2206-X ')'                        
060000             DELIMITED BY SIZE INTO SSA1                                  
060100     MOVE '  GE' TO GODK-STATUSKODER                                      
060200     CALL CBLTDLI USING GHNP WDR2-PCB DLI-IO-AREA SSA1                    
060300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
060400     PERFORM IMS-STATUS-KONTROLL                                          
060500                                                                          
060600     .                                                                    
064100 IMS-REPLACE SECTION.                                                     
064110     MOVE 'IMS-REPLACE             ' TO IMS-SECTION                       
064200     SKIP2                                                                
065000     MOVE '  '   TO GODK-STATUSKODER                                      
065100     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-AREA                         
065200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
065300     PERFORM IMS-STATUS-KONTROLL                                          
065600     .                                                                    
065700     EJECT                                                                
065800 IMS-GU-WDB601 SECTION.                                                   
065810     MOVE 'IMS-GU-WDB601             ' TO IMS-SECTION                     
065900     SKIP2                                                                
066000     STRING 'WDB601  (IDDC     =' W-WDGXKEY-IDDC ')'                      
066100          DELIMITED BY SIZE INTO SSA1                                     
066200     MOVE '  GE'   TO GODK-STATUSKODER                                    
066300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
066400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
066500     PERFORM IMS-STATUS-KONTROLL                                          
066800     .                                                                    
066801                                                                          
066900 IMS-STATUS-KONTROLL SECTION.                                             
067000     SET STATUS-IX TO 1                                                   
067100     SEARCH GODK-STATUS AT END CALL FELLOG                                
067200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
067300     END-SEARCH                                                           
067400     CONTINUE                                                             
067500     .                                                                    
067600     EJECT                                                                

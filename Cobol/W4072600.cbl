001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W4072600.                                                
001500 AUTHOR.         JAN-ERIK FRANTZEN.                                       
001600 DATE-WRITTEN.   97/04/08.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        VISAR LEVERANSANMÄRKNINGENS NYCKLAR.                             
002100*                                                                         
002210*        PROGRAMMET LÄSER      WLKREK (WDA2F)                             
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T726                                              
002600*        MID:         W4I72601                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O72601                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W4072600'.            
003610 77  INDX                        PIC S9(4)   VALUE +0 COMP SYNC.          
003620 77  MAX-INDX                    PIC S9(4)   VALUE +4 COMP SYNC.          
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004210 77  WS-IDKNOTNR                 PIC X(7)    VALUE SPACE.                 
004300                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700                                                                          
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '4726'.                
005600     88  GODK-MID                            VALUE '4721' '4722'          
005700                                                   '4723' '4724'          
005800                                                   '4725' '4726'          
005900                                                   '4727' '4728'          
006000                                                   '4729'.                
006100     88  HELP-MID                            VALUE '0551'.                
006101                                                                          
006110 77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                   
006120     88  STARTA-ANNAN-BILD                   VALUE 'J'.                   
006130                                                                          
006160 77  SPAR-IDLEVANM               PIC X(14)   VALUE SPACE.                 
006170                                                                          
006200     EJECT                                                                
006210 01  BILD-HOPP-AREOR.                                                     
006220                                                                          
006230   03    W-BILD               PIC X(4)    VALUE SPACE.                    
006240   03    W-HOPP-IDTRANS.                                                  
006250     05  FILLER               PIC X(1)    VALUE 'W'.                      
006260     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
006270     05  FILLER               PIC X(1)    VALUE 'T'.                      
006280     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
006290     05  FILLER               PIC X(2)    VALUE SPACE.                    
006291                                                                          
006292                                                                          
006293   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
006294   03      P-TO-P-SW.                                                     
006295                                                                          
006296     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
006297     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
006298     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
006299     05  P-TO-P-KDTRANS          PIC X(8).                                
006300     05  P-TO-P-IDTRANS          PIC X(4).                                
006301     05  P-TO-P-KDMFSFOR         PIC X(1).                                
006302     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
006303                                                                          
006304     EJECT                                                                
006310*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  INF-MISSING             PIC X(3)    VALUE '010'.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W4I72601                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W4O72601                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011001     03  W-WDA2F1KY-MIN-X.                                                
011010         05  W-IDKNOTNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
011020         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
011041                                                                          
011050     03  W-WDA2F1KY-MAX-X.                                                
011060         05  W-IDKNOTNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
011061         05  FILLER              PIC X(22)   VALUE HIGH-VALUE.            
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011310     88  STATUS-OK                           VALUE '  '.                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011601     88  BASEN-SLUT                          VALUE 'GB'.                  
011610     88  TRANSKOD-FEL                        VALUE 'A1'.                  
011620     88  SECURITY-FEL                        VALUE 'A4'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(128).                              
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLKREK01'.                    
013002 01  DLI-IO-WLKREK01.                                                     
013010*    03  -COPY WDA2F1                                                     
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013510*01  -COPY W0009   -PRE ALT-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801     EJECT                                                                
013802*01  -COPY W0008  -PRE KREK-                                              
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB KREK-PCB.             
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB KREK-PCB.             
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014800         IF STARTA-ANNAN-BILD                                             
014900            CONTINUE                                                      
015000         ELSE                                                             
015200           PERFORM F-LAES-VISA-INFO                                       
015210         END-IF                                                           
015300       END-IF                                                             
015400       IF STARTA-ANNAN-BILD                                               
015500          CONTINUE                                                        
015510       ELSE                                                               
015511         COMPUTE MSG-KVLL = LENGTH OF MOD-W4O72601 + 4                    
015512         PERFORM IMS-INSERT-MSG                                           
015520       END-IF                                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO                           TO RETURN-CODE                   
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I72601                 
016900       MOVE MSG-IDTRANS-2                TO MFS-IDTRANS                   
017000       MOVE MSG-KDMFSFOR-2               TO MFS-KDMFSFOR                  
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I72601                  
017300       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
017400       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP                    TO MFS-KDTRTYP                   
017800     MOVE MSG-IDPFK                      TO MFS-IDPFK                     
017900     MOVE MFS-IDTRANS                    TO W-IDTRANS                     
018000                                                                          
018100     MOVE LOW-VALUE                      TO MSG-AREA                      
018200     MOVE 'W4O72601'                     TO MFS-IDMOD                     
018300     MOVE '4726'                         TO MOD-IDTRANS                   
018400     MOVE MFS-RENSA-FAELT                TO MOD-TEMFSFEL                  
018410                                            MOD-TEMFSINF                  
018500                                                                          
018530                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE                        TO MFS-KDTRTYP                   
019000       MOVE '7'                          TO MFS-IDPFK                     
019100     END-IF                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019800     MOVE ALL '+'                        TO MSGI-WMSGINIT                 
019900     MOVE '001'                          TO MSGI-KDCALL                   
020000     MOVE MSG-LTERM-NAME                 TO MSGI-IDLTERM-USER             
020100     MOVE MSG-SIGNON-USERID              TO MSGI-IDUSER                   
020200     MOVE '4726'                         TO MSGI-IDTRANS                  
020300     IF GODK-MID                                                          
020410         MOVE MID-IDKNOTNR-IN            TO MSGI-SPAR-AREA                
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020700                                                                          
020710     IF MSGI-IDLAND-SPR = 'GB'                                            
020720       MOVE 'GB'                TO MED-IDSKYLT                            
020730     ELSE                                                                 
020740       MOVE 'S '                TO MED-IDSKYLT                            
020750     END-IF                                                               
020760                                                                          
020800     MOVE JA                             TO NYCKLAR-SW                    
020900                                                                          
021001                                                                          
021002*    -- KONTROLL AV IDKNOTNR                                              
021003     MOVE MFS-RENSA-FAELT                TO MOD-IDKNOTNR-IN               
021007     IF MID-IDKNOTNR-IN NOT = ALL '+'                                     
021008       MOVE '7'                          TO MFS-IDPFK                     
021009       MOVE SPACE                        TO MFS-KDTRTYP                   
021010       MOVE MID-IDKNOTNR-IN              TO WS-IDKNOTNR                   
021011     ELSE                                                                 
021012       MOVE MID-IDKNOTNR-UT              TO WS-IDKNOTNR                   
021013     END-IF                                                               
021014     INSPECT WS-IDKNOTNR REPLACING ALL SPACE BY ZERO                      
021015     IF WS-IDKNOTNR NUMERIC AND WS-IDKNOTNR NOT = '0000000'               
021016       MOVE WS-IDKNOTNR                  TO W-IDKNOTNR-MIN                
021017                                            W-IDKNOTNR-MAX                
021018     ELSE                                                                 
021019       MOVE NEJ                          TO NYCKLAR-SW                    
021020     END-IF                                                               
021101                                                                          
021102     IF GODK-MID OR NYCKLAR-OK                                            
021104       MOVE WS-IDKNOTNR                  TO MOD-IDKNOTNR-UT               
021105       INSPECT MOD-IDKNOTNR-UT REPLACING LEADING ZERO BY SPACE            
021106     ELSE                                                                 
021107       MOVE MFS-RENSA-FAELT              TO MOD-IDKNOTNR-UT               
021110     END-IF                                                               
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021400       MOVE ERR-WRONG-KEY                TO MED-IDMFSFEL                  
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL                   TO MOD-TEMFSFEL                  
021700       PERFORM MFS-RENSA-FAELT-IN                                         
021800       PERFORM MFS-RENSA-FAELT-UT                                         
021900     ELSE                                                                 
021901       IF MID-INPUT =  ALL '+'                                            
021902          PERFORM MFS-RENSA-FAELT-IN                                      
021903       ELSE                                                               
021904         MOVE +1 TO INDX                                                  
021905         PERFORM UNTIL INDX > MAX-INDX                                    
021906           IF MID-KDCMD (INDX)  NUMERIC                                   
021907              PERFORM BA-STARTA-ANNAN-BILD                                
021908              MOVE JA                    TO SW-STARTA-ANNAN-BILD          
021909              MOVE MAX-INDX TO INDX                                       
021910           ELSE                                                           
021911              MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDCMD-ATTR (INDX)         
021912              MOVE MFS-ROER-EJ-FAELT     TO MOD-KDCMD (INDX)              
021913              MOVE NEJ                   TO SW-STARTA-ANNAN-BILD          
021915           END-IF                                                         
021916           ADD +1 TO INDX                                                 
021917         END-PERFORM                                                      
021919       END-IF                                                             
021920     END-IF                                                               
022000     .                                                                    
022200     EJECT                                                                
022300 BA-STARTA-ANNAN-BILD SECTION.                                            
022301                                                                          
022305     MOVE MID-IDDISTR (INDX)            TO MSGI-IDDISTR                   
022306     INSPECT MSGI-IDDISTR  REPLACING LEADING SPACE BY ZERO                
022307     MOVE MID-IDKUNDNR (INDX)           TO MSGI-IDKUNDNR                  
022308     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
022309     MOVE MID-IDRAPPNR (INDX)           TO MSGI-IDRAPPNR                  
022310     INSPECT MSGI-IDRAPPNR REPLACING LEADING SPACE BY ZERO                
022312                                                                          
022313     MOVE '001'                          TO MSGI-KDCALL                   
022314     MOVE MSG-SIGNON-USERID              TO MSGI-IDUSER                   
022315     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
022316                                                                          
022317     MOVE LOW-VALUE                      TO P-TO-P-KDZ1                   
022318     MOVE LOW-VALUE                      TO P-TO-P-KDZ2                   
022319     MOVE MID-KDCMD(INDX) (1:1)          TO W-HOPP-IDTRANS-2              
022320     MOVE MID-KDCMD(INDX) (2:3)          TO W-HOPP-IDTRANS-4-6            
022321     MOVE W-HOPP-IDTRANS                 TO P-TO-P-KDTRANS                
022322     MOVE '4726'                         TO P-TO-P-IDTRANS                
022323     MOVE MFS-KDMFSFOR                   TO P-TO-P-KDMFSFOR               
022324                                                                          
022325     PERFORM S01-INSERT-ALTMSG                                            
022326     .                                                                    
022330     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022610     PERFORM IMS-GET-KREK                                                 
022700                                                                          
022801     IF SEGMENT-SAKNAS                                                    
022810        MOVE INF-MISSING                 TO MED-IDMFSFEL                  
023000        CALL WMEDKONV USING MED-WMEDAREA                                  
023100        MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                  
023110        PERFORM MFS-RENSA-FAELT-IN                                        
023200        PERFORM MFS-RENSA-FAELT-UT                                        
023300     ELSE                                                                 
023310       MOVE +1                           TO INDX                          
023320       PERFORM UNTIL INDX > MAX-INDX                                      
023330         IF SEGMENT-FINNS AND                                             
023340           ( SPAR-IDLEVANM NOT = SEQF-IDLEVANM )                          
023350                                                                          
023400           MOVE SEQF-IDDISTR             TO MOD-IDDISTR (INDX)            
023410           MOVE SEQF-IDKUNDNR            TO MOD-IDKUNDNR(INDX)            
023420           MOVE SEQF-IDRAPPNR            TO MOD-IDRAPPNR(INDX)            
023430           MOVE SEQF-DALEVANM (3:6)      TO MOD-TILEVANM(INDX)            
023431                                                                          
023432           MOVE SEQF-IDLEVANM            TO SPAR-IDLEVANM                 
023440           PERFORM IMS-GET-KREK                                           
023441           ADD +1                        TO INDX                          
023450         ELSE                                                             
023451           IF SEGMENT-SAKNAS                                              
023452             MOVE MFS-RENSA-FAELT        TO MOD-IDDISTR (INDX)            
023454                                            MOD-IDKUNDNR(INDX)            
023455                                            MOD-IDRAPPNR(INDX)            
023456                                            MOD-TILEVANM(INDX)            
023457             ADD +1                      TO INDX                          
023458           ELSE                                                           
023459             PERFORM IMS-GET-KREK                                         
023460           END-IF                                                         
023470         END-IF                                                           
023480       END-PERFORM                                                        
023500     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
024710 S01-INSERT-ALTMSG SECTION.                                               
024720                                                                          
024730     MOVE P-TO-P-SW                      TO MSG-IO-AREA                   
024740     PERFORM IMS-CHANGE-ALTMSG                                            
024750     IF STATUS-OK                                                         
024760       PERFORM IMS-INSERT-ALTMSG                                          
024770     ELSE                                                                 
024780       MOVE LOW-VALUE                    TO MSG-AREA                      
024790       MOVE 'W4O72601'                   TO MFS-IDMOD                     
024791       MOVE '4726'                       TO MOD-IDTRANS                   
024792       MOVE P-TO-P-KDTRANS (2:1)         TO W-BILD (1:1)                  
024793       MOVE P-TO-P-KDTRANS (4:3)         TO W-BILD (2:3)                  
024794       IF SECURITY-FEL                                                    
024795         STRING 'NOT AUTHORIZED TO USE '                                  
024796                W-BILD                                                    
024797                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
024798       ELSE                                                               
024799         STRING 'WRONG PICTURE '                                          
024800                 W-BILD                                                   
024801                DELIMITED BY SIZE INTO MOD-TEMFSINF                       
024802       END-IF                                                             
024803       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O72601 + 4                      
024804       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024805       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024806       PERFORM IMS-INSERT-MSG                                             
024807     END-IF                                                               
024808     .                                                                    
024809     EJECT                                                                
024810 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025000*    --- ALLA UTDATA-FÄLT                                                 
025100     MOVE +1  TO INDX                                                     
025110     PERFORM UNTIL INDX > MAX-INDX                                        
025200       MOVE MFS-RENSA-FAELT              TO MOD-IDDISTR (INDX)            
025310                                            MOD-IDKUNDNR(INDX)            
025320                                            MOD-IDRAPPNR(INDX)            
025330                                            MOD-TILEVANM(INDX)            
025340       ADD +1  TO INDX                                                    
025350     END-PERFORM                                                          
025400     .                                                                    
025600     SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025900*    --- ALLA INDATA-FÄLT                                                 
026000     MOVE +1  TO INDX                                                     
026010     PERFORM UNTIL INDX > MAX-INDX                                        
026100       MOVE MFS-RENSA-FAELT              TO MOD-KDCMD (INDX)              
026110       ADD +1  TO INDX                                                    
026120     END-PERFORM                                                          
026200     .                                                                    
026300     EJECT                                                                
026400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026500                                                                          
026600*    --- ALLA UTDATA-FÄLT                                                 
026800     MOVE MFS-ROER-EJ-FAELT              TO MOD-IDDISTR (INDX)            
026900                                            MOD-IDKUNDNR(INDX)            
027000                                            MOD-IDRAPPNR(INDX)            
027010                                            MOD-TILEVANM(INDX)            
027100     .                                                                    
027200     SKIP3                                                                
027300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027400                                                                          
027500*    --- ALLA INDATA-FÄLT                                                 
027600     MOVE MFS-ROER-EJ-FAELT              TO MOD-KDCMD (INDX)              
027800     .                                                                    
027900     EJECT                                                                
029400* --- IMS SEKTIONER ---                                                   
029500     SKIP3                                                                
029600 IMS-GET-MSG SECTION.                                                     
029700                                                                          
029800     MOVE '  QC' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-INSERT-MSG SECTION.                                                  
030500                                                                          
030600     IF MSGI-IDLAND-SPR = 'GB'                                            
030700       MOVE 'N' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-CHANGE-ALTMSG SECTION.                                               
031503     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031504     MOVE '  A1A4' TO GODK-STATUSKODER                                    
031505     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
031506     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
031507     PERFORM IMS-STATUSKONTROLL                                           
031508     .                                                                    
031509     SKIP3                                                                
031510 IMS-INSERT-ALTMSG SECTION.                                               
031511     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031512     MOVE SPACE TO GODK-STATUSKODER                                       
031513     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
031514     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
031515     PERFORM IMS-STATUSKONTROLL                                           
031516     .                                                                    
031517     EJECT                                                                
031518                                                                          
031519 IMS-GET-KREK SECTION.                                                    
031520                                                                          
031533     STRING 'WLKREK01(WDA2F1KY>=' W-WDA2F1KY-MIN-X                        
031534                    '&WDA2F1KY<=' W-WDA2F1KY-MAX-X ')'                    
031535          DELIMITED BY SIZE INTO SSA1                                     
031536     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031537     CALL CBLTDLI USING GN KREK-PCB DLI-IO-WLKREK01 SSA1                  
031538     MOVE KREK-STATUS-CODE TO STATUS-WS                                   
031539     PERFORM IMS-STATUSKONTROLL                                           
031540     .                                                                    
031600     EJECT                                                                
031700 IMS-STATUSKONTROLL SECTION.                                              
031800                                                                          
031900     SET STATUS-IX TO 1                                                   
032000     SEARCH GODK-STATUS                                                   
032100       AT END                                                             
032200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032300         DELIMITED BY SIZE INTO FELTEXT                                   
032400         CALL FELLOG                                                      
032500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    

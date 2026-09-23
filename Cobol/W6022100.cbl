000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6022100.                                                
000400*AUTHOR.         ELAINE CURTSSON.                                         
000500*DATE-WRITTEN.   AUGUSTI 92.                                              
000600                                                                          
000700*    REMARKS.   BERÖR EJ SDC                                              
000800*        UPPDATERING AV LEVERANTÖR.                                       
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        UPPDATERING, NYUPPLÄGGNING OCH FRÅGEPROGRAM                      
001200*        PROGRAMMET UPPDATERAR W6LEVAK (W6F1)                             
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W6T221                                              
001600*        MID:         W6I22101                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W6O22101                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002510                                                                          
002600*    -- CHECKED BY WY2000                                                 
002610 77  IDPGM                       PIC X(08)   VALUE 'W6022100'.            
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300                                                                          
003400 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003600                                                                          
003700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003800 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
003900 77  WS-IDLEVG                   PIC X(5)    VALUE SPACE.                 
003910 77  WS-IDMAIL1                  PIC X(60)   VALUE SPACE.                 
003930 77  WS-IDMAIL2                  PIC X(60)   VALUE SPACE.                 
003950 77  WS-IDMAIL3                  PIC X(60)   VALUE SPACE.                 
004000                                                                          
004100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004200     88  INDATA-OK                           VALUE 'J'.                   
004300     88  INDATA-FEL                          VALUE 'N'.                   
004400                                                                          
004410 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004420     88  ALLT-OK                             VALUE 'J'.                   
004440                                                                          
004450 77  UPPDATERA-LEV-SW            PIC X       VALUE 'J'.                   
004460     88  UPPDATERA-LEV                       VALUE 'J'.                   
004480                                                                          
004500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004600     88  NYCKLAR-OK                          VALUE 'J'.                   
004700     88  NYCKLAR-FEL                         VALUE 'N'.                   
005100                                                                          
005200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005300     88  EGEN-MID                            VALUE '6221'.                
005400     88  GODK-MID                            VALUE '6222' '6223'          
005500                                                   '6224' '6225'          
005600                                                   '6226' '6227'.         
005610     88  HELP-MID                            VALUE '0551'.                
005700     EJECT                                                                
005800*    --- ARBETSFÄLT                                                       
005900 77  W-ADRESS                    PIC X       VALUE 'N'.                   
006000     88  ADRESS-FINNS                        VALUE 'J'.                   
006100     88  ADRESS-SAKNAS                       VALUE 'N'.                   
006200                                                                          
006300 77  W-KONTORSADRESS             PIC X       VALUE 'N'.                   
006400     88  KONTORSADRESS-FINNS                 VALUE 'J'.                   
006500     88  KONTORSADRESS-SAKNAS                VALUE 'N'.                   
006600                                                                          
006700 77  W-GODSADRESS                PIC X       VALUE 'N'.                   
006800     88  GODSADRESS-FINNS                    VALUE 'J'.                   
006900     88  GODSADRESS-SAKNAS                   VALUE 'N'.                   
007000                                                                          
007010 01  W-LEVNAMN                   PIC X(35)   VALUE SPACE.                 
007020                                                                          
007100     EJECT                                                                
007200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007300 01  GENERELLA-SUBPROGRAM.                                                
007400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007410     03  W009EMAD                PIC X(8)    VALUE 'W009EMAD'.            
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007900*01 -COPY WMEDAREA                                                        
008000     SKIP3                                                                
008100 01  MESSAGE-CODES.                                                       
008200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008201     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008210     03  ERR-GODSADR-SAKNAS      PIC X(3)    VALUE '166'.                 
008211     03  ERR-LEVNR-SAKNAS        PIC X(3)    VALUE '092'.                 
008220     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008301     03  INF-PRESS-PF23          PIC X(3)    VALUE '206'.                 
008302     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008303     03  INF-UPPDAT-OTILLATEN    PIC X(3)    VALUE '007'.                 
008305     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008310     03  INF-MORE-INFO-FINNS     PIC X(3)    VALUE '105'.                 
008320     03  INF-INFO-SAKNAS         PIC X(3)    VALUE '413'.                 
008900     EJECT                                                                
008910*    --- PARAMETRAR TILL SUBPROGRAM W009EMAD (E-ADDRESS VALIDITY)         
008920*                                                                         
008930 01  FILLER                      PIC X(16)  VALUE 'W009EMAD-AREA'.        
008940                                                                          
008950*01 -COPY W009EMAD                                                        
008960     EJECT                                                                
008970                                                                          
009000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009300     SKIP3                                                                
009400*01  MID -COPY W6I22101                                                   
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009700     SKIP3                                                                
009800*01  -COPY WMSGAREA                                                       
009900     EJECT                                                                
010000     03  MOD REDEFINES MSG-AREA.                                          
010100*      05  -COPY W6O22101                                                 
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010400     SKIP3                                                                
010500*01  -COPY WMFSAREA                                                       
010600     EJECT                                                                
010700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010800*                                                                         
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011100     SKIP3                                                                
011200 01  NYCKLAR-TILL-DLI.                                                    
011300     03  W-IDLEVNR-X.                                                     
011400         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
011500     03  W-IDLEVG-X.                                                      
011600         05  W-IDLEVG            PIC S9(5)   VALUE ZERO COMP-3.           
011700     SKIP2                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900*                                                                         
013000     EJECT                                                                
013100*    --- IMS FUNKTIONSKODER                                               
013200*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013600     SKIP3                                                                
013700 01  DLI-IO-AREA1.                                                        
013800     03  IO-AREA1                PIC X(300)  VALUE SPACE.                 
013900     SKIP3                                                                
014000     03  WLLEVA01 REDEFINES IO-AREA1.                                     
014100*        05  -COPY WDF101  -PRE LEVA-                                     
014200     SKIP3                                                                
014300 02  DLI-IO-AREA2.                                                        
014400     03  IO-AREA2                PIC X(300)  VALUE SPACE.                 
014500     SKIP3                                                                
014600     03  WLLEVA14 REDEFINES IO-AREA2.                                     
014700*        05  -COPY WDF106  -PRE LEVA-                                     
014701     SKIP3                                                                
014710 02  DLI-IO-AREA3.                                                        
014720     03  IO-AREA3                PIC X(300)  VALUE SPACE.                 
014730     SKIP3                                                                
014740     03  W6LEVA01 REDEFINES IO-AREA3.                                     
014750*        05  -COPY W6F101                                                 
014800     SKIP3                                                                
014900 02  DLI-IO-AREA4.                                                        
015000     03  IO-AREA4                PIC X(275)  VALUE SPACE.                 
015100     SKIP3                                                                
015200     03  W6LEVA11 REDEFINES IO-AREA4.                                     
015210*        05  -COPY W6F111                                                 
015220     SKIP3                                                                
015300     EJECT                                                                
015400 LINKAGE SECTION.                                                         
015500                                                                          
015600*01  -COPY W0009   -PRE MSG-                                              
015700     EJECT                                                                
015800*01  -COPY W0008  -PRE WDF1-                                              
015900     05  FILLER                  PIC X.                                   
016000     EJECT                                                                
016010*01  -COPY W0008  -PRE W6F1-                                              
016020     05  FILLER                  PIC X.                                   
016030     EJECT                                                                
016100 PROCEDURE DIVISION  USING MSG-PCB WDF1-PCB W6F1-PCB.                     
016200     ENTRY 'DLITCBL' USING MSG-PCB WDF1-PCB W6F1-PCB.                     
016300                                                                          
016400     PERFORM IMS-GET-MSG                                                  
016500     IF SEGMENT-FINNS                                                     
016600       PERFORM A-INIT                                                     
016700       PERFORM B-KOLLA-NYCKLAR                                            
016800       IF NYCKLAR-OK                                                      
016900         IF MFS-UPDATE  OR MFS-UPD-V                                      
017000           PERFORM G-KOLLA-INPUT                                          
017100           IF ALLT-OK                                                     
017200             PERFORM H-UPPDATERA                                          
017300           END-IF                                                         
017400         ELSE                                                             
017410           IF MFS-FIRST                                                   
017420             PERFORM C-FOERSTA-SIDA                                       
017430           ELSE                                                           
017440             IF MFS-NEXT                                                  
017450               PERFORM D-NAESTA-SIDA                                      
017460             ELSE                                                         
017470               PERFORM E-SAMMA-SIDA                                       
017480             END-IF                                                       
017490           END-IF                                                         
017491         END-IF                                                           
017492         IF ALLT-OK                                                       
017493            PERFORM F-LAES-VISA-INFO                                      
017494         END-IF                                                           
017700       END-IF                                                             
017710       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O22101 + 4                      
017900       PERFORM IMS-INSERT-MSG                                             
018000     END-IF                                                               
018100                                                                          
018200     MOVE ZERO TO RETURN-CODE                                             
018300     GOBACK                                                               
018400     .                                                                    
018500     EJECT                                                                
018600 A-INIT SECTION.                                                          
018700                                                                          
018800     IF MSG-DUBBLA-TRANSKODER                                             
018900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W6I22101                 
019000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
019100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019200     ELSE                                                                 
019300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I22101                  
019400       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
019500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019600     END-IF                                                               
019700                                                                          
019800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
019900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
020000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020100                                                                          
020200     MOVE LOW-VALUE TO MSG-AREA                                           
020300     MOVE 'W6O221N1' TO MFS-IDMOD                                         
020400     MOVE '6221' TO MOD-IDTRANS                                           
020500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
020600                                                                          
020700     IF EGEN-MID OR HELP-MID                                              
020710       CONTINUE                                                           
020720     ELSE                                                                 
020800       MOVE SPACE TO MFS-KDTRTYP                                          
020900       MOVE '7' TO MFS-IDPFK                                              
021000     END-IF                                                               
021100                                                                          
021200     IF ENGLISH-TEXT                                                      
021300       MOVE +2 TO SPRAK-IX                                                
021400       MOVE 'B  ' TO MED-IDSKYLT                                          
021500     ELSE                                                                 
021600       MOVE +1 TO SPRAK-IX                                                
021700       MOVE 'S  ' TO MED-IDSKYLT                                          
021800     END-IF                                                               
021900     .                                                                    
022000     EJECT                                                                
022100 B-KOLLA-NYCKLAR SECTION.                                                 
022200                                                                          
022300     MOVE JA TO NYCKLAR-SW                                                
022400                                                                          
022500*    -- KONTROLL AV IDLEVNR                                               
022600     MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-IN                               
022700                                                                          
022800     IF MID-IDLEVNR-IN = ALL '+'                                          
022900       MOVE MID-IDLEVNR-UT TO WS-IDLEVNR                                  
023100     ELSE                                                                 
023200       MOVE MID-IDLEVNR-IN TO WS-IDLEVNR                                  
023300       MOVE '7'         TO MFS-IDPFK                                      
023400       MOVE SPACE       TO MFS-KDTRTYP                                    
023500     END-IF                                                               
023600     IF WS-IDLEVNR NOT = SPACE                                            
023700       MOVE WS-IDLEVNR TO W-IDLEVNR                                       
023800     ELSE                                                                 
023900       MOVE NEJ TO NYCKLAR-SW                                             
024000     END-IF                                                               
024100                                                                          
024200*    -- KONTROLL AV IDLEVG                                                
024300     MOVE MFS-RENSA-FAELT TO MOD-IDLEVG-IN                                
024400                                                                          
024410     IF NYCKLAR-OK                                                        
024500        IF MID-IDLEVG-IN = ALL '+'                                        
024510          IF MFS-FIRST                                                    
024511            IF MID-IDLEVG-SPAR NOT NUMERIC                                
024512               MOVE ZERO         TO MID-IDLEVG-SPAR                       
024513            END-IF                                                        
024520            MOVE MID-IDLEVG-SPAR TO WS-IDLEVG                             
024522            INSPECT WS-IDLEVG REPLACING LEADING SPACE BY ZERO             
024523            MOVE MID-IDLEVG-SPAR TO MOD-IDLEVG-SPAR                       
024530          ELSE                                                            
024600            MOVE MID-IDLEVG-UT TO WS-IDLEVG                               
024700            INSPECT WS-IDLEVG REPLACING LEADING SPACE BY ZERO             
024701            MOVE MID-IDLEVG-SPAR TO MOD-IDLEVG-SPAR                       
024710          END-IF                                                          
024800        ELSE                                                              
024900          MOVE MID-IDLEVG-IN TO WS-IDLEVG                                 
024901          IF WS-IDLEVG NUMERIC                                            
024910            MOVE WS-IDLEVG TO MOD-IDLEVG-SPAR                             
024920          END-IF                                                          
025000          MOVE '7'         TO MFS-IDPFK                                   
025100          MOVE SPACE       TO MFS-KDTRTYP                                 
025200        END-IF                                                            
025300        IF WS-IDLEVG NUMERIC                                              
025400           MOVE WS-IDLEVG  TO W-IDLEVG                                    
025500        ELSE                                                              
025600           MOVE NEJ TO NYCKLAR-SW                                         
025700        END-IF                                                            
025710     END-IF                                                               
025800                                                                          
025900     IF GODK-MID OR NYCKLAR-OK                                            
026000       MOVE WS-IDLEVNR TO MOD-IDLEVNR-UT                                  
026200       MOVE WS-IDLEVG TO MOD-IDLEVG-UT                                    
026300       INSPECT MOD-IDLEVG-UT REPLACING LEADING ZERO BY SPACE              
026400     ELSE                                                                 
026500       MOVE MFS-RENSA-FAELT TO MOD-IDLEVNR-UT                             
026600       MOVE MFS-RENSA-FAELT TO MOD-IDLEVG-UT                              
026700     END-IF                                                               
026800                                                                          
026900     IF NYCKLAR-FEL                                                       
027000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
027100       CALL WMEDKONV USING MED-WMEDAREA                                   
027200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
027300       PERFORM MFS-RENSA-FAELT-UT-IN                                      
027400     END-IF                                                               
027500     .                                                                    
027600     EJECT                                                                
027610 C-FOERSTA-SIDA SECTION.                                                  
027612     PERFORM IMS-GET-W6LEVA01                                             
027613     IF SEGMENT-FINNS                                                     
027617        PERFORM IMS-GET-W6LEVA11-OKVAL                                    
027618        IF SEGMENT-FINNS                                                  
027620           MOVE INF-FIRST-PAGE   TO MED-IDMFSINF                          
027621           CALL WMEDKONV USING MED-WMEDAREA                               
027622           MOVE MED-MFSINF       TO MOD-TEMFSINF                          
027623                                                                          
027624           MOVE ZERO             TO MOD-IDLEVG-ENTER                      
027625                                   MOD-IDLEVG-NEXT                        
027626           IF MID-IDLEVG-IN = ALL '+'                                     
027627              MOVE GADR-IDLEVG   TO WS-IDLEVG W-IDLEVG                    
027628           END-IF                                                         
027629        END-IF                                                            
027631     END-IF                                                               
027632                                                                          
027633     .                                                                    
027634     EJECT                                                                
027640 D-NAESTA-SIDA  SECTION.                                                  
027641     MOVE MID-IDLEVG-NEXT  TO W-IDLEVG                                    
027650     .                                                                    
027651     EJECT                                                                
027681 E-SAMMA-SIDA SECTION.                                                    
027682                                                                          
027683     IF EGEN-MID OR HELP-MID                                              
027684       MOVE MID-IDLEVG-ENTER TO W-IDLEVG                                  
027687                                                                          
027688       IF MID-INPUT = ALL '+'                                             
027689         PERFORM MFS-RENSA-FAELT-UT-IN                                    
027690       ELSE                                                               
027691         IF NOT (MID-FLSKPLOT = ALL '+')                                  
027692         OR NOT (MID-KDCMD-LEV = ALL '+')                                 
027693            MOVE INF-PRESS-PF23 TO MED-IDMFSFEL                           
027694         ELSE                                                             
027695            MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                           
027696         END-IF                                                           
027707         CALL WMEDKONV USING MED-WMEDAREA                                 
027708         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
027709         PERFORM EA-MID-INDATA-TILL-MOD                                   
027710       END-IF                                                             
027711     ELSE                                                                 
027712       PERFORM MFS-RENSA-FAELT-UT-IN                                      
027713     END-IF                                                               
027714     .                                                                    
027715     EJECT                                                                
027716 EA-MID-INDATA-TILL-MOD SECTION.                                          
027717                                                                          
027718* * * * * FÖR VARJE MID-FÄLT                                              
027719* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
027720* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
027721* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
027722                                                                          
027723     IF MID-FLSKPLOT NOT = ALL '+'                                        
027724        MOVE MID-FLSKPLOT          TO MOD-FLSKPLOT                        
027725     ELSE                                                                 
027726        MOVE MFS-RENSA-FAELT       TO MOD-FLSKPLOT                        
027727     END-IF                                                               
027728                                                                          
027735     IF MID-IDMAIL1 NOT = ALL '+'                                         
027736        MOVE MID-IDMAIL1           TO MOD-IDMAIL1                         
027737     ELSE                                                                 
027738        MOVE MFS-RENSA-FAELT       TO MOD-IDMAIL1                         
027739     END-IF                                                               
027740                                                                          
027747     IF MID-IDMAIL2 NOT = ALL '+'                                         
027748        MOVE MID-IDMAIL2           TO MOD-IDMAIL2                         
027749     ELSE                                                                 
027750        MOVE MFS-RENSA-FAELT       TO MOD-IDMAIL2                         
027751     END-IF                                                               
027752                                                                          
027759     IF MID-IDMAIL3 NOT = ALL '+'                                         
027760        MOVE MID-IDMAIL3           TO MOD-IDMAIL3                         
027761     ELSE                                                                 
027762        MOVE MFS-RENSA-FAELT       TO MOD-IDMAIL3                         
027763     END-IF                                                               
027764                                                                          
027765     IF MID-BELEV NOT = ALL '+'                                           
027766        MOVE MID-BELEV             TO MOD-BELEV                           
027767     ELSE                                                                 
027768        MOVE MFS-RENSA-FAELT       TO MOD-BELEV                           
027769     END-IF                                                               
027770                                                                          
027771     IF MID-ADLEV-RAD1 NOT = ALL '+'                                      
027772        MOVE MID-ADLEV-RAD1       TO MOD-ADLEV-RAD1                       
027773     ELSE                                                                 
027774        MOVE MFS-RENSA-FAELT       TO MOD-ADLEV-RAD1                      
027775     END-IF                                                               
027776                                                                          
027777     IF MID-ADLEV-RAD2 NOT = ALL '+'                                      
027778        MOVE MID-ADLEV-RAD2       TO MOD-ADLEV-RAD2                       
027779     ELSE                                                                 
027780        MOVE MFS-RENSA-FAELT       TO MOD-ADLEV-RAD2                      
027781     END-IF                                                               
027782                                                                          
027783     IF MID-ADLEV-ORT NOT = ALL '+'                                       
027784        MOVE MID-ADLEV-ORT       TO MOD-ADLEV-ORT                         
027785     ELSE                                                                 
027786        MOVE MFS-RENSA-FAELT       TO MOD-ADLEV-ORT                       
027787     END-IF                                                               
027788                                                                          
027789     IF MID-ADLEVLND NOT = ALL '+'                                        
027790        MOVE MID-ADLEVLND        TO MOD-ADLEVLND                          
027791     ELSE                                                                 
027792        MOVE MFS-RENSA-FAELT       TO MOD-ADLEVLND                        
027793     END-IF                                                               
027794                                                                          
027795     IF MID-IDLEVTLF NOT = ALL '+'                                        
027796        MOVE MID-IDLEVTLF        TO MOD-IDLEVTLF                          
027797     ELSE                                                                 
027798        MOVE MFS-RENSA-FAELT       TO MOD-IDLEVTLF                        
027799     END-IF                                                               
027800                                                                          
027801     IF MID-IDLEVFAX-1 NOT = ALL '+'                                      
027802        MOVE MID-IDLEVFAX-1      TO MOD-IDLEVFAX-1                        
027803     ELSE                                                                 
027804        MOVE MFS-RENSA-FAELT     TO MOD-IDLEVFAX-1                        
027805     END-IF                                                               
027806                                                                          
027807     IF MID-IDLEVFAX-2 NOT = ALL '+'                                      
027808        MOVE MID-IDLEVFAX-2      TO MOD-IDLEVFAX-2                        
027809     ELSE                                                                 
027810        MOVE MFS-RENSA-FAELT     TO MOD-IDLEVFAX-2                        
027811     END-IF                                                               
027812                                                                          
027813     IF MID-IDLEVFAX-3 NOT = ALL '+'                                      
027814        MOVE MID-IDLEVFAX-3      TO MOD-IDLEVFAX-3                        
027815     ELSE                                                                 
027816        MOVE MFS-RENSA-FAELT     TO MOD-IDLEVFAX-3                        
027817     END-IF                                                               
027818                                                                          
027819     IF MID-IDLEVFAX-4 NOT = ALL '+'                                      
027820        MOVE MID-IDLEVFAX-4      TO MOD-IDLEVFAX-4                        
027821     ELSE                                                                 
027822        MOVE MFS-RENSA-FAELT     TO MOD-IDLEVFAX-4                        
027823     END-IF                                                               
027824                                                                          
027825     IF MID-ADATTENT-Q NOT = ALL '+'                                      
027826        MOVE MID-ADATTENT-Q      TO MOD-ADATTENT-Q                        
027827     ELSE                                                                 
027828        MOVE MFS-RENSA-FAELT     TO MOD-ADATTENT-Q                        
027829     END-IF                                                               
027830                                                                          
027831     IF MID-ADATTENT-A NOT = ALL '+'                                      
027832        MOVE MID-ADATTENT-A      TO MOD-ADATTENT-A                        
027833     ELSE                                                                 
027834        MOVE MFS-RENSA-FAELT     TO MOD-ADATTENT-A                        
027835     END-IF                                                               
027836                                                                          
027837     IF MID-KDCMD-LEV NOT = ALL '+'                                       
027838        MOVE MID-KDCMD-LEV           TO MOD-KDCMD-LEV                     
027839     ELSE                                                                 
027840        MOVE MFS-RENSA-FAELT       TO MOD-KDCMD-LEV                       
027841     END-IF                                                               
027842                                                                          
027843     IF MID-KDCMD-GADR NOT = ALL '+'                                      
027844        MOVE MID-KDCMD-GADR          TO MOD-KDCMD-GADR                    
027845     ELSE                                                                 
027846        MOVE MFS-RENSA-FAELT       TO MOD-KDCMD-GADR                      
027847     END-IF                                                               
027848                                                                          
027849     .                                                                    
027850     EJECT                                                                
027851 F-LAES-VISA-INFO SECTION.                                                
027852                                                                          
027853     IF MID-INPUT NOT = ALL '+' AND MFS-ENTER AND NOT                     
027854        MFS-UPDATE AND NOT MFS-UPD-V                                      
027855        PERFORM MFS-ROER-EJ-FAELT-UT-IN                                   
027856        IF NOT (MID-FLSKPLOT  = ALL '+')                                  
027857        OR NOT (MID-KDCMD-LEV = ALL '+')                                  
027858           MOVE INF-PRESS-PF23 TO MED-IDMFSFEL                            
027859        ELSE                                                              
027860           MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                            
027861        END-IF                                                            
027862        CALL WMEDKONV USING MED-WMEDAREA                                  
027863        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
027864        PERFORM MFS-LAES-IN-IGEN                                          
027865     ELSE                                                                 
027866                                                                          
027867       PERFORM IMS-GET-WLLEVA01                                           
027868       IF SEGMENT-FINNS                                                   
027869           PERFORM IMS-GET-WLLEVA14                                       
027870           IF SEGMENT-FINNS                                               
027871              MOVE LEVA-ADR-BELEV TO W-LEVNAMN                            
027880           ELSE                                                           
027881              MOVE SPACE          TO W-LEVNAMN                            
027882           END-IF                                                         
027890       END-IF                                                             
027891                                                                          
027900       PERFORM IMS-GET-W6LEVA01                                           
028100       IF SEGMENT-FINNS                                                   
028110          PERFORM FA-FLYTTA-LEV-INFO                                      
028120          IF WS-IDLEVG > ZERO                                             
028200             PERFORM IMS-GET-W6LEVA11                                     
028300             IF SEGMENT-FINNS                                             
028310                MOVE GADR-IDLEVG      TO MOD-IDLEVG-ENTER                 
028320                                         MOD-IDLEVG-NEXT                  
028330                                         MOD-IDLEVG-UT                    
028340                INSPECT MOD-IDLEVG-UT REPLACING LEADING ZERO              
028350                                                BY SPACE                  
028400                PERFORM FB-FLYTTA-GODSADRESS                              
028500             ELSE                                                         
028501                MOVE ERR-GODSADR-SAKNAS   TO MED-IDMFSFEL                 
028520                CALL WMEDKONV USING MED-WMEDAREA                          
028530                MOVE MED-MFSFEL TO MOD-TEMFSFEL                           
029700             END-IF                                                       
029710          END-IF                                                          
029800       ELSE                                                               
029801          PERFORM MFS-RENSA-FAELT-UT-IN                                   
029822          MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                              
029824          CALL WMEDKONV USING MED-WMEDAREA                                
029825          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
030300       END-IF                                                             
030301       IF WS-IDLEVG = ZERO                                                
030302          PERFORM MFS-SPAERRA-ADRESS-FAELT                                
030303       END-IF                                                             
030310     END-IF                                                               
030400     .                                                                    
030500     EJECT                                                                
030510 FA-FLYTTA-LEV-INFO   SECTION.                                            
030511                                                                          
030512     MOVE LEV-FLSKPLOT         TO MOD-FLSKPLOT                            
030514     MOVE LEV-IDMAIL(1)        TO MOD-IDMAIL1                             
030516     MOVE LEV-IDMAIL(2)        TO MOD-IDMAIL2                             
030518     MOVE LEV-IDMAIL(3)        TO MOD-IDMAIL3                             
030519     MOVE LEV-IDLEVFAX(1)      TO MOD-IDLEVFAX-1                          
030520     MOVE LEV-IDLEVFAX(2)      TO MOD-IDLEVFAX-2                          
030521     MOVE LEV-IDLEVFAX(3)      TO MOD-IDLEVFAX-3                          
030522     MOVE LEV-IDLEVFAX(4)      TO MOD-IDLEVFAX-4                          
030523     MOVE W-LEVNAMN            TO MOD-LEVNAMN                             
030524     .                                                                    
030530     EJECT                                                                
030600 FB-FLYTTA-GODSADRESS SECTION.                                            
030700                                                                          
030800     MOVE GADR-BELEV            TO MOD-BELEV                              
030900     MOVE GADR-ADLEV-RAD1       TO MOD-ADLEV-RAD1                         
031000     MOVE GADR-ADLEV-RAD2       TO MOD-ADLEV-RAD2                         
031100     MOVE GADR-ADLEV-ORT        TO MOD-ADLEV-ORT                          
031200     MOVE GADR-ADLEVLND         TO MOD-ADLEVLND                           
031300     MOVE GADR-IDLEVTLF         TO MOD-IDLEVTLF                           
031400     MOVE GADR-ADATTENT(1)      TO MOD-ADATTENT-Q                         
031410     MOVE GADR-ADATTENT(2)      TO MOD-ADATTENT-A                         
031500                                                                          
031510     PERFORM IMS-GET-W6LEVA11-OKVAL                                       
031520     IF SEGMENT-FINNS                                                     
031530       MOVE GADR-IDLEVG        TO MOD-IDLEVG-NEXT                         
031531       IF NOT MFS-UPDATE AND NOT MFS-UPD-V                                
031532          MOVE INF-MORE-INFO-FINNS TO MED-IDMFSINF                        
031533          CALL WMEDKONV USING MED-WMEDAREA                                
031534          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
031535       END-IF                                                             
031536     END-IF                                                               
031600     .                                                                    
031700     EJECT                                                                
032900 G-KOLLA-INPUT SECTION.                                                   
033000                                                                          
033010     PERFORM IMS-GET-WLLEVA01                                             
033011     IF SEGMENT-FINNS                                                     
033020        PERFORM IMS-GET-WLLEVA14                                          
033030        IF SEGMENT-FINNS                                                  
033040           MOVE JA TO W-KONTORSADRESS                                     
033050        ELSE                                                              
033060           MOVE NEJ TO W-KONTORSADRESS                                    
033070        END-IF                                                            
033080     ELSE                                                                 
033090        MOVE NEJ TO W-KONTORSADRESS                                       
033091     END-IF                                                               
033100     MOVE JA  TO INDATA-SW                                                
033110     MOVE JA  TO ALLT-SW                                                  
033200     PERFORM IMS-GET-W6LEVA01                                             
033300     IF SEGMENT-FINNS                                                     
033310        MOVE LEV-IDMAIL(1)   TO WS-IDMAIL1                                
033330        MOVE LEV-IDMAIL(2)   TO WS-IDMAIL2                                
033350        MOVE LEV-IDMAIL(3)   TO WS-IDMAIL3                                
033400        PERFORM IMS-GET-W6LEVA11                                          
033500        IF SEGMENT-FINNS                                                  
033600           MOVE JA TO W-GODSADRESS                                        
033700        ELSE                                                              
033710           MOVE NEJ TO W-GODSADRESS                                       
034300        END-IF                                                            
034310     ELSE                                                                 
034330        PERFORM IMS-GET-WLLEVA01                                          
034340        IF SEGMENT-SAKNAS                                                 
034350           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
034360           CALL WMEDKONV USING MED-WMEDAREA                               
034370           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
034380           PERFORM MFS-RENSA-FAELT-UT-IN                                  
034381           MOVE NEJ TO ALLT-SW                                            
034390        END-IF                                                            
034410     END-IF                                                               
034420     IF INDATA-OK AND ALLT-OK                                             
034500        IF MID-INPUT = ALL '+'                                            
034600           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
034700           CALL WMEDKONV USING MED-WMEDAREA                               
034800           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
034900           PERFORM MFS-ROER-EJ-FAELT-UT-IN                                
035000           MOVE NEJ TO ALLT-SW                                            
035100        ELSE                                                              
035200                                                                          
035210          IF MFS-UPDATE                                                   
035211            IF MID-FLSKPLOT NOT = ALL '+' OR                              
035212               MID-KDCMD-LEV NOT = ALL '+'                                
035230               MOVE INF-UPPDAT-OTILLATEN TO MED-IDMFSINF                  
035240               CALL WMEDKONV USING MED-WMEDAREA                           
035250               MOVE MED-TEMFSINF         TO MOD-TEMFSINF                  
035260               PERFORM MFS-ROER-EJ-FAELT-UT-IN                            
035270               PERFORM MFS-LAES-IN-IGEN                                   
035271               IF MID-FLSKPLOT NOT = ALL '+'                              
035272                  MOVE MFS-ALFA-FAELT-FEL TO MOD-FLSKPLOT-ATTR            
035273               END-IF                                                     
035274               IF MID-KDCMD-LEV NOT = ALL '+'                             
035275                  MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-LEV-ATTR           
035276               END-IF                                                     
035280               MOVE NEJ TO ALLT-SW                                        
035290            ELSE                                                          
035295              IF MID-IDMAIL1   NOT = ALL '+'                              
035296                MOVE MID-IDMAIL1    TO WS-IDMAIL1                         
035297                IF WS-IDMAIL1 > SPACE                                     
035298                  MOVE WS-IDMAIL1  TO EMAD-IDMAIL                         
035299                  CALL W009EMAD USING EMAD-W009EMAD                       
035300                  IF EMAD-KDSVAR > SPACE                                  
035301                    MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMAIL1-ATTR           
035302                    MOVE NEJ         TO INDATA-SW                         
035303                  ELSE                                                    
035304                    MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDMAIL1-ATTR         
035305                    MOVE EMAD-IDMAIL                                      
035306                      TO MID-IDMAIL1 WS-IDMAIL1                           
035307                  END-IF                                                  
035308                END-IF                                                    
035310              END-IF                                                      
035314              IF MID-IDMAIL2   NOT = ALL '+'                              
035315                MOVE MID-IDMAIL2    TO WS-IDMAIL2                         
035316                IF WS-IDMAIL2 > SPACE                                     
035317                  MOVE WS-IDMAIL2  TO EMAD-IDMAIL                         
035318                  CALL W009EMAD USING EMAD-W009EMAD                       
035319                  IF EMAD-KDSVAR > SPACE                                  
035320                    MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMAIL2-ATTR           
035321                    MOVE NEJ         TO INDATA-SW                         
035322                  ELSE                                                    
035323                    MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDMAIL2-ATTR         
035324                    MOVE EMAD-IDMAIL                                      
035325                      TO MID-IDMAIL2 WS-IDMAIL2                           
035326                  END-IF                                                  
035327                END-IF                                                    
035330              END-IF                                                      
035333              IF MID-IDMAIL3   NOT = ALL '+'                              
035334                MOVE MID-IDMAIL3    TO WS-IDMAIL3                         
035335                IF WS-IDMAIL3 > SPACE                                     
035336                  MOVE WS-IDMAIL3  TO EMAD-IDMAIL                         
035337                  CALL W009EMAD USING EMAD-W009EMAD                       
035338                  IF EMAD-KDSVAR > SPACE                                  
035339                    MOVE MFS-ALFA-FAELT-FEL TO MOD-IDMAIL3-ATTR           
035340                    MOVE NEJ         TO INDATA-SW                         
035341                  ELSE                                                    
035342                    MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDMAIL3-ATTR         
035343                    MOVE EMAD-IDMAIL                                      
035344                      TO MID-IDMAIL3 WS-IDMAIL3                           
035345                  END-IF                                                  
035346                END-IF                                                    
035347              END-IF                                                      
035349              IF KONTORSADRESS-SAKNAS AND GODSADRESS-SAKNAS               
035350                AND WS-IDLEVG > ZERO                                      
035400                IF MID-BELEV = ALL '+' OR MID-BELEV = SPACE               
035500                   MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR              
035600                   MOVE NEJ TO INDATA-SW                                  
035700                ELSE                                                      
035800                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEV-ATTR            
035900                END-IF                                                    
036000                                                                          
036100               MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-RAD1-ATTR           
036200               MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-RAD2-ATTR           
036300                                                                          
036800               IF MID-ADLEV-ORT = ALL '+' OR MID-ADLEV-ORT = SPACE        
036900                   MOVE MFS-ALFA-FAELT-FEL TO MOD-ADLEV-ORT-ATTR          
037000                   MOVE NEJ TO INDATA-SW                                  
037100               ELSE                                                       
037200                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-ORT-ATTR        
037300                END-IF                                                    
037400                                                                          
037500                IF MID-ADLEVLND = ALL '+' OR MID-ADLEVLND = SPACE         
037600                   MOVE MFS-ALFA-FAELT-FEL TO MOD-ADLEVLND-ATTR           
037700                   MOVE NEJ TO INDATA-SW                                  
037800                ELSE                                                      
037900                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEVLND-ATTR         
038000                END-IF                                                    
038100              ELSE                                                        
038110                IF WS-IDLEVG > ZERO                                       
038200                   IF GODSADRESS-FINNS                                    
038300                      PERFORM GA-KOLLA-GODSADRESS                         
038400                   ELSE                                                   
038500                      IF KONTORSADRESS-FINNS                              
038600                         PERFORM GB-KOLLA-KONTORSADRESS                   
038700                      END-IF                                              
038800                   END-IF                                                 
038810                END-IF                                                    
038900              END-IF                                                      
039190              IF MID-KDCMD-GADR NOT =  ALL '+'                            
039191                 IF (MID-KDCMD-GADR = 'D' OR MID-KDCMD-GADR = 'B')        
039192                    OR MID-KDCMD-GADR = SPACE                             
039193                    IF MID-KDCMD-GADR = 'D' OR 'B'                        
039194                       IF GODSADRESS-SAKNAS                               
039195                          MOVE MFS-ALFA-FAELT-FEL   TO                    
039196                               MOD-KDCMD-GADR-ATTR                        
039197                          MOVE NEJ TO INDATA-SW                           
039198                       ELSE                                               
039199                          MOVE MFS-ALFA-FAELT-RAETT TO                    
039200                               MOD-KDCMD-GADR-ATTR                        
039201                       END-IF                                             
039202                    ELSE                                                  
039203                       MOVE MFS-ALFA-FAELT-RAETT TO                       
039204                            MOD-KDCMD-GADR-ATTR                           
039205                    END-IF                                                
039206                 ELSE                                                     
039207                    MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-GADR-ATTR        
039208                    MOVE NEJ TO INDATA-SW                                 
039209                 END-IF                                                   
039210              END-IF                                                      
039211            END-IF                                                        
039212          END-IF                                                          
039213          IF MFS-UPD-V                                                    
039216             IF MID-FLSKPLOT NOT =  ALL '+' OR                            
039217                MID-KDCMD-LEV NOT =  ALL '+'                              
039218              IF MID-FLSKPLOT NOT = ALL '+'                               
039246                IF MID-FLSKPLOT = JA OR NEJ                               
039247                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLSKPLOT-ATTR         
039248                ELSE                                                      
039249                   MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLSKPLOT-ATTR         
039250                   MOVE NEJ TO INDATA-SW                                  
039251                END-IF                                                    
039252              END-IF                                                      
039253              IF MID-KDCMD-LEV NOT =  ALL '+'                             
039254                 IF (MID-KDCMD-LEV = 'D' OR MID-KDCMD-LEV = 'B')          
039255                    OR MID-KDCMD-LEV = SPACE                              
039256                    IF MID-KDCMD-LEV = 'D' OR 'B'                         
039257                       PERFORM IMS-GET-W6LEVA01                           
039258                       IF SEGMENT-FINNS                                   
039259                          PERFORM IMS-GET-W6LEVA11-OKVAL                  
039260                          IF SEGMENT-FINNS                                
039261                             MOVE MFS-ALFA-FAELT-FEL TO                   
039262                                        MOD-KDCMD-LEV-ATTR                
039263                             MOVE NEJ TO INDATA-SW                        
039264                          ELSE                                            
039265                             MOVE MFS-ALFA-FAELT-RAETT TO                 
039266                                        MOD-KDCMD-LEV-ATTR                
039267                          END-IF                                          
039268                       ELSE                                               
039269                          MOVE MFS-ALFA-FAELT-FEL TO                      
039270                                        MOD-KDCMD-LEV-ATTR                
039271                          MOVE NEJ TO INDATA-SW                           
039272                       END-IF                                             
039273                    ELSE                                                  
039274                       MOVE MFS-ALFA-FAELT-RAETT TO                       
039275                                     MOD-KDCMD-LEV-ATTR                   
039276                    END-IF                                                
039277                 ELSE                                                     
039278                    MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-LEV-ATTR        
039279                    MOVE NEJ TO INDATA-SW                                 
039280                 END-IF                                                   
039281              END-IF                                                      
039282             ELSE                                                         
039286                MOVE INF-UPPDAT-OTILLATEN TO MED-IDMFSINF                 
039287                CALL WMEDKONV USING MED-WMEDAREA                          
039288                MOVE MED-TEMFSINF         TO MOD-TEMFSINF                 
039289                PERFORM MFS-ROER-EJ-FAELT-UT-IN                           
039291                PERFORM MFS-LAES-IN-IGEN                                  
039293                MOVE NEJ TO ALLT-SW                                       
039294             END-IF                                                       
039295          END-IF                                                          
039296        END-IF                                                            
039297     END-IF                                                               
039298     IF INDATA-FEL                                                        
039299        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
039301        CALL WMEDKONV USING MED-WMEDAREA                                  
039302        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
039304        PERFORM MFS-ROER-EJ-FAELT-UT-IN                                   
039305        MOVE NEJ TO ALLT-SW                                               
039306     END-IF                                                               
039307     IF WS-IDLEVG = ZERO                                                  
039308        PERFORM MFS-SPAERRA-ADRESS-FAELT                                  
039310     END-IF                                                               
039400                                                                          
040400     .                                                                    
040500     EJECT                                                                
040510 GA-KOLLA-GODSADRESS SECTION.                                             
040520                                                                          
040530     IF GADR-BELEV = SPACE                                                
040540         IF MID-BELEV = ALL '+' OR MID-BELEV = SPACE                      
040550            MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR                     
040560            MOVE NEJ TO INDATA-SW                                         
040570         ELSE                                                             
040580            MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEV-ATTR                   
040590         END-IF                                                           
040591     END-IF                                                               
040592                                                                          
040593     IF GADR-BELEV NOT = SPACE                                            
040594         IF MID-BELEV = SPACE                                             
040595            MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR                     
040596            MOVE NEJ TO INDATA-SW                                         
040597         ELSE                                                             
040598            MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEV-ATTR                   
040599         END-IF                                                           
040600     END-IF                                                               
040601                                                                          
040602     MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-RAD1-ATTR                     
040603     MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-RAD2-ATTR                     
040604                                                                          
040620     IF GADR-ADLEV-ORT = SPACE                                            
040621         IF MID-ADLEV-ORT = ALL '+' OR MID-ADLEV-ORT = SPACE              
040622            MOVE MFS-ALFA-FAELT-FEL TO MOD-ADLEV-ORT-ATTR                 
040623            MOVE NEJ TO INDATA-SW                                         
040624         ELSE                                                             
040625            MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-ORT-ATTR               
040626         END-IF                                                           
040627     END-IF                                                               
040628                                                                          
040629     IF GADR-ADLEV-ORT NOT = SPACE                                        
040630         IF MID-ADLEV-ORT =  SPACE                                        
040631            MOVE MFS-ALFA-FAELT-FEL TO MOD-ADLEV-ORT-ATTR                 
040632            MOVE NEJ TO INDATA-SW                                         
040633         ELSE                                                             
040634            MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-ORT-ATTR               
040635         END-IF                                                           
040636     END-IF                                                               
040637                                                                          
040639     IF GADR-ADLEVLND = SPACE                                             
040640         IF MID-ADLEVLND  = ALL '+' OR MID-ADLEVLND = SPACE               
040641            MOVE MFS-ALFA-FAELT-FEL TO MOD-ADLEVLND-ATTR                  
040642            MOVE NEJ TO INDATA-SW                                         
040643         ELSE                                                             
040644            MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEVLND-ATTR                
040645         END-IF                                                           
040646     END-IF                                                               
040647                                                                          
040648     IF GADR-ADLEVLND NOT = SPACE                                         
040649         IF MID-ADLEVLND = SPACE                                          
040650            MOVE MFS-ALFA-FAELT-FEL TO MOD-ADLEVLND-ATTR                  
040651            MOVE NEJ TO INDATA-SW                                         
040652         ELSE                                                             
040653            MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEVLND-ATTR                
040654         END-IF                                                           
040655     END-IF                                                               
040656     .                                                                    
040657     EJECT                                                                
040660 GB-KOLLA-KONTORSADRESS SECTION.                                          
040700                                                                          
040800     IF LEVA-ADR-BELEV = SPACE                                            
040900         IF MID-BELEV = ALL '+' OR MID-BELEV = SPACE                      
041000            MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR                     
041100            MOVE NEJ TO INDATA-SW                                         
041200         ELSE                                                             
041300            MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEV-ATTR                   
041400         END-IF                                                           
041500     ELSE                                                                 
041800         IF MID-BELEV = SPACE                                             
041900            MOVE MFS-ALFA-FAELT-FEL TO MOD-BELEV-ATTR                     
042000            MOVE NEJ TO INDATA-SW                                         
042100         ELSE                                                             
042200            MOVE MFS-ALFA-FAELT-RAETT TO MOD-BELEV-ATTR                   
042300         END-IF                                                           
042400     END-IF                                                               
042500                                                                          
042600     MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-RAD1-ATTR                     
042700     MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-RAD2-ATTR                     
042710     MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEV-ORT-ATTR                      
042720     MOVE MFS-ALFA-FAELT-RAETT TO MOD-ADLEVLND-ATTR                       
047900     .                                                                    
048000     EJECT                                                                
055600 H-UPPDATERA SECTION.                                                     
055700                                                                          
055800     PERFORM IMS-GHU-W6LEVA01                                             
055901     IF MFS-UPD-V                                                         
055902       IF MID-KDCMD-LEV = 'D' OR 'B'                                      
055903          PERFORM IMS-GET-W6LEVA11-OKVAL                                  
055904          IF SEGMENT-SAKNAS                                               
055905             PERFORM IMS-GHU-W6LEVA01                                     
055906             PERFORM IMS-DLET-W6LEVA01                                    
055907          END-IF                                                          
055908       ELSE                                                               
055909        IF SEGMENT-FINNS                                                  
055910           IF MID-FLSKPLOT NOT = ALL '+'                                  
055911              MOVE MID-FLSKPLOT  TO LEV-FLSKPLOT                          
055912           END-IF                                                         
055913           PERFORM IMS-REPL-W6LEVA01                                      
055914        ELSE                                                              
055915           MOVE SPACE TO LEV-W6F101                                       
055916           MOVE W-IDLEVNR TO LEV-IDLEVNR                                  
055917           IF MID-FLSKPLOT NOT = ALL '+'                                  
055918              MOVE MID-FLSKPLOT  TO LEV-FLSKPLOT                          
055919           END-IF                                                         
055920           PERFORM IMS-ISRT-W6LEVA01                                      
055921        END-IF                                                            
055922       END-IF                                                             
055923     END-IF                                                               
055930     IF MFS-UPDATE                                                        
055942        IF SEGMENT-FINNS                                                  
055943           PERFORM HA-FLYTTA-FRAN-MID-TILL-ROT                            
055950           PERFORM IMS-REPL-W6LEVA01                                      
055951        ELSE                                                              
055952           MOVE SPACE TO LEV-W6F101                                       
055953           MOVE W-IDLEVNR TO LEV-IDLEVNR                                  
055954           PERFORM HA-FLYTTA-FRAN-MID-TILL-ROT                            
055956           PERFORM IMS-ISRT-W6LEVA01                                      
055957        END-IF                                                            
055958        IF WS-IDLEVG > ZERO                                               
055959          PERFORM IMS-GHU-W6LEVA01                                        
055960          IF SEGMENT-FINNS                                                
055961             PERFORM IMS-GHNP-W6LEVA11                                    
055962             IF SEGMENT-FINNS                                             
055963                IF MID-KDCMD-GADR = 'D' OR 'B'                            
055965                   PERFORM IMS-DLET-W6LEVA11                              
055967                ELSE                                                      
055968                   PERFORM HB-FLYTTA-FRAN-MID-TILL-BARN                   
055969                   PERFORM IMS-REPL-W6LEVA11                              
055970                END-IF                                                    
055971             ELSE                                                         
055972                PERFORM IMS-GET-WLLEVA01                                  
055973                IF SEGMENT-FINNS                                          
055974                  PERFORM IMS-GET-WLLEVA14                                
055975                  IF SEGMENT-FINNS                                        
055976                     PERFORM IMS-GET-W6LEVA01                             
055977                     PERFORM HC-FLYTTA-KONTORSADR                         
055978                     PERFORM IMS-ISRT-W6LEVA11                            
055979                  ELSE                                                    
055980                     PERFORM HB-FLYTTA-FRAN-MID-TILL-BARN                 
055981                     PERFORM IMS-ISRT-W6LEVA11                            
055982                  END-IF                                                  
055983                END-IF                                                    
055984             END-IF                                                       
055985          END-IF                                                          
055986        END-IF                                                            
057300     END-IF                                                               
057500                                                                          
057600     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
057700     CALL WMEDKONV USING MED-WMEDAREA                                     
057800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
057900     PERFORM MFS-RENSA-FAELT-UT-IN                                        
058000     PERFORM MFS-FORM-ATTR                                                
058201     .                                                                    
058202     EJECT                                                                
058220 HA-FLYTTA-FRAN-MID-TILL-ROT SECTION.                                     
058230                                                                          
058243     IF MID-IDMAIL1 NOT = ALL '+'                                         
058244        MOVE MID-IDMAIL1         TO LEV-IDMAIL(1)                         
058247     END-IF                                                               
058248                                                                          
058257     IF MID-IDMAIL2 NOT = ALL '+'                                         
058258        MOVE MID-IDMAIL2         TO LEV-IDMAIL(2)                         
058259     END-IF                                                               
058260                                                                          
058265     IF MID-IDMAIL3 NOT = ALL '+'                                         
058266        MOVE MID-IDMAIL3         TO LEV-IDMAIL(3)                         
058267     END-IF                                                               
058268                                                                          
058273     IF MID-IDLEVFAX-1 NOT = ALL '+'                                      
058274        MOVE MID-IDLEVFAX-1   TO LEV-IDLEVFAX(1)                          
058275     END-IF                                                               
058276                                                                          
058277     IF MID-IDLEVFAX-2 NOT = ALL '+'                                      
058278        MOVE MID-IDLEVFAX-2   TO LEV-IDLEVFAX(2)                          
058279     END-IF                                                               
058280                                                                          
058281     IF MID-IDLEVFAX-3 NOT = ALL '+'                                      
058282        MOVE MID-IDLEVFAX-3   TO LEV-IDLEVFAX(3)                          
058283     END-IF                                                               
058284                                                                          
058285     IF MID-IDLEVFAX-4 NOT = ALL '+'                                      
058286        MOVE MID-IDLEVFAX-4   TO LEV-IDLEVFAX(4)                          
058287     END-IF                                                               
058288                                                                          
058289     IF SEGMENT-SAKNAS                                                    
058290        IF MID-FLSKPLOT = ALL '+'                                         
058291           MOVE JA TO LEV-FLSKPLOT                                        
058292        END-IF                                                            
058293     END-IF                                                               
058294                                                                          
058295     .                                                                    
058300     EJECT                                                                
058400 HB-FLYTTA-FRAN-MID-TILL-BARN SECTION.                                    
058500                                                                          
058600     MOVE W-IDLEVG            TO GADR-IDLEVG                              
058700                                                                          
058800     IF MID-BELEV NOT = ALL '+'                                           
058900        MOVE MID-BELEV TO GADR-BELEV MOD-BELEV                            
059200     END-IF                                                               
059300                                                                          
059400     IF MID-ADLEV-RAD1 NOT = ALL '+'                                      
059500        MOVE MID-ADLEV-RAD1 TO GADR-ADLEV-RAD1                            
059600                               MOD-ADLEV-RAD1                             
059900     END-IF                                                               
060000                                                                          
060100     IF MID-ADLEV-RAD2 NOT = ALL '+'                                      
060200        MOVE MID-ADLEV-RAD2 TO GADR-ADLEV-RAD2                            
060300                               MOD-ADLEV-RAD2                             
060600     END-IF                                                               
060700                                                                          
060800     IF MID-ADLEV-ORT NOT = ALL '+'                                       
060900        MOVE MID-ADLEV-ORT TO GADR-ADLEV-ORT                              
061000                              MOD-ADLEV-ORT                               
061300     END-IF                                                               
061400                                                                          
061500     IF MID-ADLEVLND NOT = ALL '+'                                        
061600        MOVE MID-ADLEVLND TO GADR-ADLEVLND                                
061700                             MOD-ADLEVLND                                 
062000     END-IF                                                               
062100                                                                          
062200     IF MID-IDLEVTLF NOT = ALL '+'                                        
062300        MOVE MID-IDLEVTLF TO GADR-IDLEVTLF                                
062400                             MOD-IDLEVTLF                                 
062700     END-IF                                                               
062800                                                                          
062900     IF MID-ADATTENT-Q NOT = ALL '+'                                      
063000        MOVE MID-ADATTENT-Q TO GADR-ADATTENT(1)                           
063100                               MOD-ADATTENT-Q                             
063400     END-IF                                                               
063401                                                                          
063410     IF MID-ADATTENT-A NOT = ALL '+'                                      
063420        MOVE MID-ADATTENT-A TO GADR-ADATTENT(2)                           
063430                               MOD-ADATTENT-A                             
063440     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800 HC-FLYTTA-KONTORSADR SECTION.                                            
063900                                                                          
063910     MOVE SPACE TO GADR-W6F111                                            
064000     MOVE W-IDLEVG            TO GADR-IDLEVG                              
064100                                                                          
064200     IF MID-BELEV NOT = ALL '+'                                           
064300        MOVE MID-BELEV TO GADR-BELEV MOD-BELEV                            
064400     ELSE                                                                 
064500        MOVE LEVA-ADR-BELEV   TO GADR-BELEV MOD-BELEV                     
064700     END-IF                                                               
064800                                                                          
064900     IF MID-ADLEV-RAD1 NOT = ALL '+'                                      
065000        MOVE MID-ADLEV-RAD1 TO GADR-ADLEV-RAD1                            
065100                               MOD-ADLEV-RAD1                             
065400     END-IF                                                               
065500                                                                          
065600     IF MID-ADLEV-RAD2 NOT = ALL '+'                                      
065700        MOVE MID-ADLEV-RAD2 TO GADR-ADLEV-RAD2                            
065800                               MOD-ADLEV-RAD2                             
066100     END-IF                                                               
066200                                                                          
066300     IF MID-ADLEV-ORT NOT = ALL '+'                                       
066400        MOVE MID-ADLEV-ORT TO GADR-ADLEV-ORT                              
066500                              MOD-ADLEV-ORT                               
066800     END-IF                                                               
066900                                                                          
067000     IF MID-ADLEVLND NOT = ALL '+'                                        
067100        MOVE MID-ADLEVLND TO GADR-ADLEVLND                                
067200                             MOD-ADLEVLND                                 
067500     END-IF                                                               
067600                                                                          
067700     IF MID-IDLEVTLF NOT = ALL '+'                                        
067800        MOVE MID-IDLEVTLF TO GADR-IDLEVTLF                                
067900                             MOD-IDLEVTLF                                 
068200     END-IF                                                               
068300                                                                          
068400     IF MID-ADATTENT-Q NOT = ALL '+'                                      
068500        MOVE MID-ADATTENT-Q TO GADR-ADATTENT(1)                           
068600                               MOD-ADATTENT-Q                             
068900     END-IF                                                               
069000                                                                          
069010     IF MID-ADATTENT-A NOT = ALL '+'                                      
069020        MOVE MID-ADATTENT-A TO GADR-ADATTENT(2)                           
069030                               MOD-ADATTENT-A                             
069070     END-IF                                                               
069100     .                                                                    
069200     EJECT                                                                
069310 MFS-RENSA-FAELT-UT-IN SECTION.                                           
069400                                                                          
069500     MOVE MFS-RENSA-FAELT TO MOD-LEVNAMN                                  
069501                             MOD-FLSKPLOT                                 
069511                             MOD-IDMAIL1                                  
069513                             MOD-IDMAIL2                                  
069515                             MOD-IDMAIL3                                  
069520                             MOD-BELEV                                    
069600                             MOD-ADLEV-RAD1                               
069700                             MOD-ADLEV-RAD2                               
069800                             MOD-ADLEV-ORT                                
069900                             MOD-ADLEVLND                                 
070000                             MOD-IDLEVTLF                                 
070010                             MOD-IDLEVFAX-1                               
070020                             MOD-IDLEVFAX-2                               
070030                             MOD-IDLEVFAX-3                               
070040                             MOD-IDLEVFAX-4                               
070100                             MOD-ADATTENT-Q                               
070101                             MOD-ADATTENT-A                               
070110                             MOD-KDCMD-LEV                                
070111                             MOD-KDCMD-GADR                               
070120                             MOD-IDLEVG-ENTER                             
070130                             MOD-IDLEVG-NEXT                              
070200     .                                                                    
070300     SKIP2                                                                
070400     EJECT                                                                
070410 MFS-SPAERRA-ADRESS-FAELT SECTION.                                        
070420                                                                          
070430     MOVE MFS-STAENG-FAELT-NOMOD TO MOD-BELEV-ATTR                        
070470                                    MOD-ADLEV-RAD1-ATTR                   
070480                                    MOD-ADLEV-RAD2-ATTR                   
070490                                    MOD-ADLEV-ORT-ATTR                    
070491                                    MOD-ADLEVLND-ATTR                     
070492                                    MOD-IDLEVTLF-ATTR                     
070493                                    MOD-IDLEVFAX-ATTR-1                   
070494                                    MOD-IDLEVFAX-ATTR-2                   
070495                                    MOD-IDLEVFAX-ATTR-3                   
070496                                    MOD-IDLEVFAX-ATTR-4                   
070497                                    MOD-ADATTENT-Q-ATTR                   
070498                                    MOD-ADATTENT-A-ATTR                   
070499                                    MOD-KDCMD-GADR-ATTR                   
070500     .                                                                    
070501     SKIP2                                                                
070510 MFS-ROER-EJ-FAELT-UT-IN  SECTION.                                        
070600                                                                          
070700     MOVE MFS-ROER-EJ-FAELT TO MOD-FLSKPLOT                               
070711                               MOD-IDMAIL1                                
070713                               MOD-IDMAIL2                                
070715                               MOD-IDMAIL3                                
070720                               MOD-BELEV                                  
070800                               MOD-ADLEV-RAD1                             
070900                               MOD-ADLEV-RAD2                             
071000                               MOD-ADLEV-ORT                              
071100                               MOD-ADLEVLND                               
071200                               MOD-IDLEVTLF                               
071210                               MOD-IDLEVFAX-1                             
071220                               MOD-IDLEVFAX-2                             
071230                               MOD-IDLEVFAX-3                             
071240                               MOD-IDLEVFAX-4                             
071300                               MOD-ADATTENT-Q                             
071301                               MOD-ADATTENT-A                             
071302                               MOD-LEVNAMN                                
071310                               MOD-KDCMD-LEV                              
071311                               MOD-KDCMD-GADR                             
071320                               MOD-IDLEVG-ENTER                           
071330                               MOD-IDLEVG-NEXT                            
071400     .                                                                    
071500     SKIP2                                                                
071600 MFS-FORM-ATTR SECTION.                                                   
071700                                                                          
071800     MOVE MFS-FORMATETS-ATTR TO MOD-FLSKPLOT-ATTR                         
071802                                MOD-IDMAIL1-ATTR                          
071804                                MOD-IDMAIL2-ATTR                          
071806                                MOD-IDMAIL3-ATTR                          
071810                                MOD-BELEV-ATTR                            
071900                                MOD-ADLEV-RAD1-ATTR                       
072000                                MOD-ADLEV-RAD2-ATTR                       
072100                                MOD-ADLEV-ORT-ATTR                        
072200                                MOD-ADLEVLND-ATTR                         
072300                                MOD-IDLEVTLF-ATTR                         
072310                                MOD-IDLEVFAX-ATTR-1                       
072320                                MOD-IDLEVFAX-ATTR-2                       
072330                                MOD-IDLEVFAX-ATTR-3                       
072340                                MOD-IDLEVFAX-ATTR-4                       
072400                                MOD-ADATTENT-Q-ATTR                       
072401                                MOD-ADATTENT-A-ATTR                       
072410                                MOD-KDCMD-GADR-ATTR                       
072420                                MOD-KDCMD-LEV-ATTR                        
072500     .                                                                    
072600     SKIP2                                                                
072700 MFS-LAES-IN-IGEN SECTION.                                                
072800                                                                          
072801     IF MID-FLSKPLOT NOT = ALL '+'                                        
072802        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLSKPLOT-ATTR                   
072803     END-IF                                                               
072804                                                                          
072809     IF MID-IDMAIL1 NOT = ALL '+'                                         
072810        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDMAIL1-ATTR                    
072811     END-IF                                                               
072813                                                                          
072818     IF MID-IDMAIL2 NOT = ALL '+'                                         
072819        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDMAIL2-ATTR                    
072820     END-IF                                                               
072821                                                                          
072826     IF MID-IDMAIL3 NOT = ALL '+'                                         
072827        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDMAIL3-ATTR                    
072828     END-IF                                                               
072829                                                                          
072830     IF MID-BELEV NOT = ALL '+'                                           
072831        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-BELEV-ATTR                      
072832     END-IF                                                               
072840                                                                          
072850     IF MID-ADLEV-RAD1 NOT = ALL '+'                                      
072860        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-RAD1-ATTR                 
072870     END-IF                                                               
072890                                                                          
072900     IF MID-ADLEV-RAD2 NOT = ALL '+'                                      
073000        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-RAD2-ATTR                 
073010     END-IF                                                               
073020                                                                          
073030     IF MID-ADLEV-ORT NOT = ALL '+'                                       
073040        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEV-ORT-ATTR                  
073050     END-IF                                                               
073060                                                                          
073070     IF MID-ADLEVLND NOT = ALL '+'                                        
073080        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADLEVLND-ATTR                   
073090     END-IF                                                               
073100                                                                          
073200     IF MID-IDLEVTLF NOT = ALL '+'                                        
073300        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVTLF-ATTR                   
073310     END-IF                                                               
073320                                                                          
073321     IF MID-IDLEVFAX-1 NOT = ALL '+'                                      
073322        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVFAX-ATTR-1                 
073323     END-IF                                                               
073324                                                                          
073325     IF MID-IDLEVFAX-2 NOT = ALL '+'                                      
073326        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVFAX-ATTR-2                 
073327     END-IF                                                               
073328                                                                          
073329     IF MID-IDLEVFAX-3 NOT = ALL '+'                                      
073330        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVFAX-ATTR-3                 
073331     END-IF                                                               
073332                                                                          
073333     IF MID-IDLEVFAX-4 NOT = ALL '+'                                      
073334        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDLEVFAX-ATTR-4                 
073335     END-IF                                                               
073336                                                                          
073337     IF MID-ADATTENT-Q NOT = ALL '+'                                      
073340        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADATTENT-Q-ATTR                 
073350     END-IF                                                               
073351                                                                          
073352     IF MID-ADATTENT-A NOT = ALL '+'                                      
073353        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADATTENT-A-ATTR                 
073354     END-IF                                                               
073355                                                                          
073360     IF MID-KDCMD-LEV NOT = ALL '+'                                       
073370        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-LEV-ATTR                  
073380     END-IF                                                               
073381                                                                          
073390     IF MID-KDCMD-GADR NOT = ALL '+'                                      
073400        MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-GADR-ATTR                 
073500     END-IF                                                               
073600     .                                                                    
073700     EJECT                                                                
073800* --- IMS SEKTIONER ---                                                   
073900     SKIP3                                                                
074000 IMS-GET-MSG SECTION.                                                     
074100                                                                          
074200     MOVE '  QC' TO GODK-STATUSKODER                                      
074300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
074400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
074500     PERFORM IMS-STATUSKONTROLL                                           
074600     .                                                                    
074700     SKIP3                                                                
074800 IMS-INSERT-MSG SECTION.                                                  
074900                                                                          
075000     IF NOT ENGLISH-TEXT                                                  
075100       MOVE '0' TO MFS-KDHUVOMR                                           
075200     END-IF                                                               
075300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
075400     MOVE SPACE TO GODK-STATUSKODER                                       
075500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
075600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075700     PERFORM IMS-STATUSKONTROLL                                           
075800     .                                                                    
075900     EJECT                                                                
076000 IMS-GET-WLLEVA01 SECTION.                                                
076100     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
076200          DELIMITED BY SIZE INTO SSA1                                     
076300     MOVE '  GE' TO GODK-STATUSKODER                                      
076400     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-AREA1 SSA1                     
076500     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
076600     PERFORM IMS-STATUSKONTROLL                                           
076700     .                                                                    
076800     EJECT                                                                
076900 IMS-GET-WLLEVA14 SECTION.                                                
077000                                                                          
077200     MOVE '  GE' TO GODK-STATUSKODER                                      
077300     CALL CBLTDLI USING GNP WDF1-PCB DLI-IO-AREA2                         
077400     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
077500     PERFORM IMS-STATUSKONTROLL                                           
077600     .                                                                    
077700     EJECT                                                                
077790 IMS-GET-W6LEVA01 SECTION.                                                
077791     STRING 'W6LEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
077792          DELIMITED BY SIZE INTO SSA1                                     
077793     MOVE '  GE' TO GODK-STATUSKODER                                      
077794     CALL CBLTDLI USING GU W6F1-PCB DLI-IO-AREA3 SSA1                     
077795     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
077796     PERFORM IMS-STATUSKONTROLL                                           
077797     .                                                                    
077798     EJECT                                                                
077799 IMS-GHU-W6LEVA01 SECTION.                                                
077800     STRING 'W6LEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
077801          DELIMITED BY SIZE INTO SSA1                                     
077802     MOVE '  GE' TO GODK-STATUSKODER                                      
077803     CALL CBLTDLI USING GHU W6F1-PCB DLI-IO-AREA3 SSA1                    
077804     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
077805     PERFORM IMS-STATUSKONTROLL                                           
077806     .                                                                    
077807     EJECT                                                                
077808 IMS-GET-W6LEVA11-OKVAL SECTION.                                          
077809                                                                          
077810     MOVE '  GE' TO GODK-STATUSKODER                                      
077811     CALL CBLTDLI USING GNP W6F1-PCB DLI-IO-AREA4                         
077812     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
077813     PERFORM IMS-STATUSKONTROLL                                           
077814     .                                                                    
077815     EJECT                                                                
077820 IMS-GET-W6LEVA11 SECTION.                                                
077900     STRING 'W6LEVA11(IDLEVG   =' W-IDLEVG-X ')'                          
078000          DELIMITED BY SIZE INTO SSA1                                     
078100     MOVE '  GE' TO GODK-STATUSKODER                                      
078200     CALL CBLTDLI USING GNP W6F1-PCB DLI-IO-AREA4 SSA1                    
078300     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
078400     PERFORM IMS-STATUSKONTROLL                                           
078500     .                                                                    
078600     SKIP3                                                                
078601 IMS-GHNP-W6LEVA11 SECTION.                                               
078602     STRING 'W6LEVA11(IDLEVG   =' W-IDLEVG-X ')'                          
078603          DELIMITED BY SIZE INTO SSA1                                     
078604     MOVE '  GE' TO GODK-STATUSKODER                                      
078605     CALL CBLTDLI USING GHNP W6F1-PCB DLI-IO-AREA4 SSA1                   
078606     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
078607     PERFORM IMS-STATUSKONTROLL                                           
078608     .                                                                    
078609     SKIP3                                                                
078610 IMS-ISRT-W6LEVA01 SECTION.                                               
078620                                                                          
078650     MOVE 'W6LEVA01 ' TO SSA1                                             
078660     MOVE '  II' TO GODK-STATUSKODER                                      
078670     CALL CBLTDLI USING ISRT W6F1-PCB DLI-IO-AREA3 SSA1                   
078680     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
078690     PERFORM IMS-STATUSKONTROLL                                           
078691     .                                                                    
078692     SKIP3                                                                
078700 IMS-ISRT-W6LEVA11 SECTION.                                               
078800                                                                          
078900     STRING 'W6LEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
079000          DELIMITED BY SIZE INTO SSA1                                     
079100     MOVE 'W6LEVA11 ' TO SSA2                                             
079200     MOVE '  II' TO GODK-STATUSKODER                                      
079300     CALL CBLTDLI USING ISRT W6F1-PCB DLI-IO-AREA4 SSA1 SSA2              
079400     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
079500     PERFORM IMS-STATUSKONTROLL                                           
079600     .                                                                    
079700     SKIP3                                                                
079710 IMS-REPL-W6LEVA01 SECTION.                                               
079720                                                                          
079730     MOVE '  ' TO GODK-STATUSKODER                                        
079740     CALL CBLTDLI USING REPL W6F1-PCB DLI-IO-AREA3                        
079750     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
079760     PERFORM IMS-STATUSKONTROLL                                           
079770     .                                                                    
079780     EJECT                                                                
079800 IMS-REPL-W6LEVA11 SECTION.                                               
079900                                                                          
080000     MOVE '  ' TO GODK-STATUSKODER                                        
080100     CALL CBLTDLI USING REPL W6F1-PCB DLI-IO-AREA4                        
080200     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
080300     PERFORM IMS-STATUSKONTROLL                                           
080400     .                                                                    
080500     EJECT                                                                
080501 IMS-DLET-W6LEVA01 SECTION.                                               
080502                                                                          
080503     MOVE '  ' TO GODK-STATUSKODER                                        
080504     CALL CBLTDLI USING DLET W6F1-PCB DLI-IO-AREA3                        
080505     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
080506     PERFORM IMS-STATUSKONTROLL                                           
080507     .                                                                    
080508     EJECT                                                                
080510 IMS-DLET-W6LEVA11 SECTION.                                               
080520                                                                          
080530     MOVE '  ' TO GODK-STATUSKODER                                        
080540     CALL CBLTDLI USING DLET W6F1-PCB DLI-IO-AREA4                        
080550     MOVE W6F1-STATUS-CODE TO STATUS-WS                                   
080560     PERFORM IMS-STATUSKONTROLL                                           
080570     .                                                                    
080580     EJECT                                                                
080600 IMS-STATUSKONTROLL SECTION.                                              
080700                                                                          
080800     SET STATUS-IX TO 1                                                   
080900     SEARCH GODK-STATUS                                                   
081000       AT END                                                             
081100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
081200         DELIMITED BY SIZE INTO FELTEXT                                   
081300         CALL FELLOG                                                      
081400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
081500     END-SEARCH                                                           
081600     .                                                                    

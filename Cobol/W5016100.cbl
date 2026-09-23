001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5016100.                                                
001500 AUTHOR.         SUSANNE OLSSON.                                          
001600 DATE-WRITTEN.   98/01/22.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMET TAR EMOT/VIDAREBEFORDRAR SOL-PARAMETRAR TILL          
002100*        SOP(VIA PGM W00606).                                             
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W5T161                                              
002700*        MID:         W5I16101                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W5O16101                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003310                                                                          
003500 DATA DIVISION.                                                           
003510     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W5016100'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004310 77  WS-ARTAL-NUM                PIC 9(4).                                
004311                                                                          
004340 01  W-TRANSDATUM-10             PIC 9(6)    VALUE ZERO.                  
004350 01  W-AAVV                      PIC 9(4)    VALUE ZERO.                  
004400                                                                          
004500*                                                                         
004510*01  -COPY WWDCKONS                                                       
004520                                                                          
004600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800                                                                          
004901 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004902     88  INDATA-OK                           VALUE 'J'.                   
004910     88  INDATA-FEL                          VALUE 'N'.                   
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '5161'.                
005700     88  GODK-MID                            VALUE '5161' '5162'          
005800                                                   '5163' '5164'          
005900                                                   '5165' '5166'          
006000                                                   '5167' '5168'          
006100                                                   '5169'.                
006200     88  HELP-MID                            VALUE '0551'.                
006300     EJECT                                                                
006400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006501     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006502     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     EJECT                                                                
007020 01  FILLER                     PIC X(24)  VALUE 'WDATAREA-START'.        
007030*01  -COPY WDATAREA                                                       
007040     EJECT                                                                
007050 01  FILLER                     PIC X(24)  VALUE 'WORKAREA-START'.        
007060*01  -COPY WORKAREA                                                       
007100     EJECT                                                                
007200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007300*01 -COPY WMEDAREA                                                        
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007601     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007602     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007603     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007610     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008000     EJECT                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400     SKIP3                                                                
008500*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008710 01  BMP-PARAMETRAR.                                                      
008720   03  SKICKA-IDARTNR            PIC  X(9).                               
008730   03  SKICKA-FLTYEAR            PIC  X(1).                               
008740   03  SKICKA-FLLYEAR            PIC  X(1).                               
008750   03  SKICKA-FLSOLLOAD          PIC  X(1).                               
008760   03  SKICKA-IDUSER             PIC  X(8).                               
008790     EJECT                                                                
008800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009100     SKIP3                                                                
009200*01  MID -COPY W5I16101                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009500     SKIP3                                                                
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
009800     03  MOD REDEFINES MSG-AREA.                                          
009900*      05  -COPY W5O16101                                                 
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010200     SKIP3                                                                
010300*01  -COPY WMFSAREA                                                       
010400     EJECT                                                                
010401 01  W-PROG-TO-PROG-SW.                                                   
010402*03  -COPY WMSGSOP                                                        
010410     EJECT                                                                
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011800     SKIP2                                                                
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(64).                               
012400     EJECT                                                                
012500*    --- IMS FUNKTIONSKODER                                               
012600*01  -COPY W0003                                                          
013400     EJECT                                                                
013500 LINKAGE SECTION.                                                         
013600*01  -COPY W0009   -PRE MSG-                                              
013610*01  -COPY W0009   -PRE ALT-                                              
013700*01  -COPY W0008   -PRE USEA-                                             
013800     05  FILLER                  PIC X.                                   
014000     EJECT                                                                
014101 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB.                      
014102 MAIN SECTION.                                                            
014110     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB.                      
014200                                                                          
014400     PERFORM IMS-GET-MSG                                                  
014500     IF SEGMENT-FINNS                                                     
014600       PERFORM A-INIT                                                     
014700       PERFORM B-KOLLA-NYCKLAR                                            
014800       IF NYCKLAR-OK                                                      
014901         IF MFS-UPDATE                                                    
014902           PERFORM G-KOLLA-INPUT                                          
014903           IF INDATA-OK                                                   
014904             PERFORM H-UPPDATERA                                          
014905           END-IF                                                         
014910         ELSE                                                             
014915           IF MID-SOL-URVALSPARAMETRAR-GRP = ALL '+'                      
014916             PERFORM MFS-RENSA-FAELT-IN                                   
014917           ELSE                                                           
014920             PERFORM G-KOLLA-INPUT                                        
015101             IF INDATA-OK                                                 
015102                MOVE INF-PRESS-PF11 TO MED-IDMFSINF                       
015103                CALL WMEDKONV USING MED-WMEDAREA                          
015104                MOVE MED-MFSINF TO MOD-TEMFSINF                           
015105                PERFORM S01-MID-INDATA-TILL-MOD                           
015214             END-IF                                                       
015215           END-IF                                                         
015220         END-IF                                                           
015400       END-IF                                                             
015700       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O16101 + 4                      
015800       PERFORM IMS-INSERT-MSG                                             
015900     END-IF                                                               
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
016800     IF MSG-DUBBLA-TRANSKODER                                             
016900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I16101                 
017000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017200     ELSE                                                                 
017300       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I16101                  
017400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017600     END-IF                                                               
017700                                                                          
017800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018100                                                                          
018120                                                                          
018200     MOVE LOW-VALUE TO MSG-AREA                                           
018300     MOVE 'W5O161N1' TO MFS-IDMOD                                         
018400     MOVE '5161' TO MOD-IDTRANS                                           
018500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018501                                                                          
018510     PERFORM MFS-RENSA-FAELT-IN                                           
018600                                                                          
018700     IF EGEN-MID OR HELP-MID                                              
018800       CONTINUE                                                           
018900     ELSE                                                                 
019000       MOVE SPACE TO MFS-KDTRTYP                                          
019100       MOVE '7' TO MFS-IDPFK                                              
019200     END-IF                                                               
019210                                                                          
019300     MOVE FUNCTION CURRENT-DATE(1:4) TO WS-ARTAL-NUM                      
019500     .                                                                    
019600     EJECT                                                                
019700 B-KOLLA-NYCKLAR SECTION.                                                 
019800                                                                          
019900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020000     MOVE '001'             TO MSGI-KDCALL                                
020100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020300     MOVE '5161'            TO MSGI-IDTRANS                               
020400                                                                          
020500     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
020600     MOVE '2'               TO MFS-KDMFSFOR                               
020610                                                                          
020620     MOVE MFS-RENSA-FAELT   TO MOD-IDUSER-IN                              
020630     MOVE MSG-SIGNON-USERID TO MOD-IDUSER-UT                              
020640                                                                          
020700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020701                                                                          
020702**** RESTEN AV SECTIONEN BEHÖVS FÖR ATT LÄGGA UT TVÅ ÅRTAL PÅ 5161        
020703**** HÄR KOLLAS OM RENAME AV ANALYSFILER SKETT (OM W555Y1 KÖRTS)          
020704**** FÖRST TA REDA PÅ FÖRSTA DAGEN I DENNA VECKA                          
020705     MOVE 'IDAG  '          TO DAT-KDDATFORM                              
020706     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
020707                         DAT-O-TIDATUM DAT-KDSVAR                         
020710     IF NOT DAT-KDSVAR-OK                                                 
020730         CALL  FELLOG                                                     
020740     END-IF                                                               
020741     MOVE 'AAVV  '          TO DAT-KDDATFORM                              
020742     MOVE DAT-TIAAVV-GRP    TO W-AAVV                                     
020743     MOVE W-AAVV            TO DAT-I-TIDATUM                              
020745     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
020746                         DAT-O-TIDATUM DAT-KDSVAR                         
020747     IF DAT-KDSVAR-OK                                                     
020748       MOVE DAT-O-TIDATUM   TO WORK-TIAAMMDD-TOM                          
020751     END-IF                                                               
020760                                                                          
020853**** TA SEDAN RÄTT PÅ DATUM FÖR 11 ARBETSDAGAR SEDAN                      
020854**** DVS KOMMANDE HELG RENSAS SOL FRÅN ALLT ÄLDRE ÄN 15 ARBETSDAG.        
020855     MOVE WC-CDC-SE         TO WORK-IDDC                                  
020856     MOVE 003               TO WORK-KDCALL                                
020857     MOVE 11                TO WORK-KVWORKD                               
020858     CALL WORKDAY USING WORK-KDCALL WORK-DATE-AREA WORK-KDSVAR            
020862     IF WORK-KDSVAR-FEL                                                   
020863        MOVE ' FEL FRÅN WORKDAY  I B-KOLLA--' TO FELTEXT                  
020864        CALL FELLOG                                                       
020865        MOVE NEJ            TO NYCKLAR-SW                                 
020866     END-IF                                                               
020867     MOVE WORK-TIAAMMDD-FOM TO W-TRANSDATUM-10                            
020868                                                                          
020869**** OM DATUM-ÅR FÖR 11 ARBETSDAGAR SEDAN ÄR LIKA MED AKTULLT ÅR          
020870**** HAR ÅRSKÖRNING KÖRTS OCH SPARADE SOLTRANSAR FINNS FÖR AKT. ÅR        
020871     IF NYCKLAR-OK                                                        
020872       IF WS-ARTAL-NUM(3:2) NOT = W-TRANSDATUM-10(1:2)                    
020874           COMPUTE WS-ARTAL-NUM = WS-ARTAL-NUM - 1                        
020875       END-IF                                                             
020880       MOVE WS-ARTAL-NUM    TO MOD-DATHISY                                
020890       COMPUTE WS-ARTAL-NUM = WS-ARTAL-NUM - 1                            
020892       MOVE WS-ARTAL-NUM    TO MOD-DALASTY                                
021000     END-IF                                                               
022100     .                                                                    
022300     EJECT                                                                
024702 G-KOLLA-INPUT SECTION.                                                   
024703                                                                          
024705     IF MID-SOL-URVALSPARAMETRAR-GRP = ALL '+'                            
024706       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024707       CALL WMEDKONV USING MED-WMEDAREA                                   
024708       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024709       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024710       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024711       MOVE NEJ TO INDATA-SW                                              
024712     ELSE                                                                 
024713*******************************************************                   
024724*** FÖR INMATN.FÄLT : MID-IDARTNR  ********************                   
024725                                                                          
024726       MOVE JA  TO INDATA-SW                                              
024727       IF MID-IDARTNR NOT = ALL '+'                                       
024728         IF MID-IDARTNR NUMERIC  AND MID-IDARTNR > 0                      
024729           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDARTNR-ATTR                   
024730         ELSE                                                             
024731           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR                     
024732           MOVE NEJ TO INDATA-SW                                          
024734         END-IF                                                           
024735       ELSE                                                               
024738           MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR                     
024739           MOVE NEJ TO INDATA-SW                                          
024740       END-IF                                                             
024741                                                                          
024742***************************************************************           
024743******* FÖR INMATN.FÄLT : MID-FLTHISYEAR **********************           
024744                                                                          
024745       IF MID-FLTHISYEAR NOT = ALL '+'                                    
024746         IF MID-FLTHISYEAR = 'Y' OR 'N'                                   
024747           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLTHISYEAR-ATTR               
024748         ELSE                                                             
024749           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTHISYEAR-ATTR                 
024750           MOVE NEJ TO INDATA-SW                                          
024751         END-IF                                                           
024752       ELSE                                                               
024753         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTHISYEAR-ATTR                   
024754         MOVE NEJ TO INDATA-SW                                            
024755       END-IF                                                             
024756                                                                          
024757***************************************************************           
024758******* FÖR INMATN.FÄLT : MID-FLLASTYEAR **********************           
024759                                                                          
024760       IF MID-FLLASTYEAR NOT = ALL '+'                                    
024761         IF MID-FLLASTYEAR = 'Y' OR 'N'                                   
024762           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLLASTYEAR-ATTR               
024764         ELSE                                                             
024765           MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLASTYEAR-ATTR                 
024766           MOVE NEJ TO INDATA-SW                                          
024767         END-IF                                                           
024768       ELSE                                                               
024769         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLASTYEAR-ATTR                   
024770         MOVE NEJ TO INDATA-SW                                            
024771       END-IF                                                             
024772                                                                          
024789***************************************************************           
024790******* BÅDA DESSA FÄLT FÅR INTE HA VÄRDET N SAMTIDIGT. *******           
024791                                                                          
024792       IF MID-FLTHISYEAR = 'N' AND MID-FLLASTYEAR = 'N'                   
024793         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
024794         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLTHISYEAR-ATTR                   
024795         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLLASTYEAR-ATTR                   
024796         MOVE NEJ TO INDATA-SW                                            
024797       END-IF                                                             
024798                                                                          
024799       IF INDATA-FEL                                                      
024800         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
024801         CALL WMEDKONV USING MED-WMEDAREA                                 
024802         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024803         PERFORM MFS-ROER-EJ-FAELT-UT                                     
024804         PERFORM MFS-ROER-EJ-FAELT-IN                                     
024819       END-IF                                                             
024820     END-IF                                                               
024821     .                                                                    
024822     EJECT                                                                
024823 H-UPPDATERA SECTION.                                                     
024838                                                                          
024839*                                                                         
024840*  FLYTTA DATA TILL WMSGSOP / STARTA BMP W555B1                           
024841*                                                                         
024842     MOVE MID-IDARTNR       TO SKICKA-IDARTNR                             
024843     MOVE MID-FLTHISYEAR    TO SKICKA-FLTYEAR                             
024844     MOVE MID-FLLASTYEAR    TO SKICKA-FLLYEAR                             
024846     MOVE MSG-SIGNON-USERID TO SKICKA-IDUSER                              
024847                                                                          
024848     MOVE '5161'        TO MSGSOP-IDTRANS                                 
024849     MOVE MFS-KDMFSFOR  TO MSGSOP-KDMFSFOR                                
024850     MOVE 'W555B1'      TO MSGSOP-IDPROCESS                               
024851     MOVE 'O'           TO MSGSOP-KDSOPFUNK                               
024852                                                                          
024853     STRING 'IDARTNR(' SKICKA-IDARTNR ')                                  
024854-           'FLTYEAR(' SKICKA-FLTYEAR ')                                  
024855-           'FLLYEAR(' SKICKA-FLLYEAR ')                                  
024858-           'IDUSER(' SKICKA-IDUSER ')'                                   
024860          DELIMITED BY SIZE INTO MSGSOP-TESYMBV                           
024861     PERFORM IMS-INSERT-ALTMSG                                            
024868                                                                          
024869     MOVE 'BMP STARTED ' TO MOD-TEMFSINF                                  
024870     .                                                                    
024880     EJECT                                                                
024890 S01-MID-INDATA-TILL-MOD SECTION.                                         
024891                                                                          
024892* * * * * FÖR VARJE MID-FÄLT                                              
024893* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
024894* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
024895* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
024896                                                                          
024897     IF MID-IDARTNR NOT = ALL '+'                                         
024898       MOVE MID-IDARTNR TO MOD-IDARTNR                                    
024899       INSPECT MOD-IDARTNR REPLACING LEADING ZERO BY SPACE                
024900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-ATTR                     
024901       MOVE MFS-ROER-EJ-FAELT     TO MOD-IDARTNR                          
024902     ELSE                                                                 
024903       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR                                
024904     END-IF                                                               
024905                                                                          
024906     IF MID-FLTHISYEAR NOT = ALL '+'                                      
024907       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLTHISYEAR-ATTR                  
024908       MOVE MFS-ROER-EJ-FAELT TO MOD-FLTHISYEAR                           
024909     ELSE                                                                 
024910       MOVE MFS-RENSA-FAELT TO MOD-FLTHISYEAR                             
024911     END-IF                                                               
024912                                                                          
024913     IF MID-FLLASTYEAR NOT = ALL '+'                                      
024914       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLLASTYEAR-ATTR                  
024915       MOVE MFS-ROER-EJ-FAELT TO MOD-FLLASTYEAR                           
024916     ELSE                                                                 
024917       MOVE MFS-RENSA-FAELT TO MOD-FLLASTYEAR                             
024918     END-IF                                                               
024919                                                                          
024927     .                                                                    
024928     EJECT                                                                
025800 MFS-RENSA-FAELT-IN SECTION.                                              
025900                                                                          
026000*    --- ALLA INDATA-FÄLT                                                 
026100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR                                  
026200                             MOD-FLTHISYEAR                               
026210                             MOD-FLLASTYEAR                               
026300     .                                                                    
026400     SKIP3                                                                
026500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026600                                                                          
026700*    --- ALLA UTDATA-FÄLT                                                 
026900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDUSER-UT                              
027200     .                                                                    
027300     SKIP3                                                                
027400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027500                                                                          
027600*    --- ALLA INDATA-FÄLT                                                 
027700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR                                
027800                               MOD-FLTHISYEAR                             
027810                               MOD-FLLASTYEAR                             
027900     .                                                                    
028000     EJECT                                                                
028100*MFS-FORM-ATTR SECTION.                                                   
028200*                                                                         
028300*    --- ALLA INDATA-FÄLT                                                 
028400*    MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-ATTR                          
028500*                               MOD-FLTHISYEAR-ATTR                       
028510*                               MOD-FLLASTYEAR-ATTR                       
028600*    .                                                                    
028700     SKIP2                                                                
029400     EJECT                                                                
029500* --- IMS SEKTIONER ---                                                   
029600                                                                          
029700 IMS-GET-MSG SECTION.                                                     
029800                                                                          
029900     MOVE '  QC' TO GODK-STATUSKODER                                      
030000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030200     PERFORM IMS-STATUSKONTROLL                                           
030300     .                                                                    
030400     SKIP3                                                                
030500 IMS-INSERT-MSG SECTION.                                                  
030600                                                                          
030700     IF ENGLISH-TEXT                                                      
030800       MOVE 'N' TO MFS-KDHUVOMR                                           
030900     END-IF                                                               
031000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031100     MOVE SPACE TO GODK-STATUSKODER                                       
031200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031400     PERFORM IMS-STATUSKONTROLL                                           
031500     .                                                                    
031601     SKIP3                                                                
031630 IMS-INSERT-ALTMSG SECTION.                                               
031640     MOVE '  ' TO GODK-STATUSKODER                                        
031650     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
031660     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
031670     PERFORM IMS-STATUSKONTROLL                                           
031680     .                                                                    
031700     EJECT                                                                
031800 IMS-STATUSKONTROLL SECTION.                                              
031900                                                                          
032000     SET STATUS-IX TO 1                                                   
032100     SEARCH GODK-STATUS                                                   
032200       AT END                                                             
032300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032400         DELIMITED BY SIZE INTO FELTEXT                                   
032500         CALL FELLOG                                                      
032600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032700         CONTINUE                                                         
032800     END-SEARCH                                                           
032900     .                                                                    

001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4269600.                                                
001300*AUTHOR.         INGER NILSSON.                                           
001400*DATE-WRITTEN.   92/10/15.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        FIL FÖR ATT LADDA W6D2                                           
002000*                                                                         
002110*        PROGRAMMET SKRIVER UTFIL FÖR UPPDATERAR W6KVAH (W6D2)            
002120*        PROGRAMMET LÄSER      W6PROA (W6G1)                              
002130*                              W6INLE (WDL2)                              
002200*                                                                         
002300*    ABENDKODER:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- ARTIKLAR FÖR UPPDATERING PÅ W6D2                           
003410     SELECT W42601                     ASSIGN TO W42696D1.                
003420     SELECT W42602                     ASSIGN TO W42696D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W42601                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005     SKIP2                                                                
004010 01  INPOST.                                                              
004020*   03 -COPY W4269702 -PRE IN-                                            
004110     EJECT                                                                
004120 FD  W42602                                                               
004130     RECORDING       F                                                    
004140     BLOCK CONTAINS  0.                                                   
004150     SKIP2                                                                
004160 01  UTPOST.                                                              
004170*   03 -COPY W4269702 -PRE UT-                                            
004180     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004302*    -- CHECKED BY WY2000                                                 
004303 77  IDPGM                       PIC X(8)    VALUE 'W4269600'.            
004304*    ---- ARBETSVARIABLER                                                 
004305*                                                                         
004306 01  MAX-TAL                     PIC S9(09) COMP.                         
004307 01  SLUMP-TAL                   PIC S9(09) COMP.                         
004310                                                                          
004320*77  W-IDPTYP                    PIC X(3)    VALUE 'R32'.                 
004330*77  W-IDLEVNR                   PIC X(5)    VALUE SPACE.                 
004340 77  W-IDPROVPL-PRI              PIC X       VALUE SPACE.                 
004350 77  W-IDPROVPL-SEK              PIC X       VALUE SPACE.                 
004360 77  W-KVSKPLOT                  PIC S9(03) COMP-3 VALUE +0.              
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004801                                                                          
004802 77  TIUPPDAT-SW                 PIC X       VALUE 'N'.                   
004810     88  TIUPPDAT-FOUND                      VALUE 'J'.                   
004820                                                                          
004830 77  W42601-EOF-SW               PIC X       VALUE 'N'.                   
004840     88  END-OF-W42601                       VALUE 'J'.                   
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006120     03  WRANDOM                 PIC X(8)    VALUE 'WRANDOM'.             
006200     SKIP2                                                                
006300*    --- PARAMETRAR TILL ABEND                                            
006400                                                                          
006500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  FELTEXT.                                                             
006900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008101     03  W-IDARTNR-X.                                                     
008102         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008104     03  W-IDPTYP                PIC X(3)    VALUE 'R32'.                 
008105     03  W-IDLEVNR-X.                                                     
008110         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
008114     03  W-W6GX-6101-KEY-X.                                               
008115         05 W-IDHTYP-6101        PIC X(04)  VALUE '6101'.                 
008116         05 FILLER               PIC X(26)  VALUE LOW-VALUE.              
008117     03  W-W6GX-6102-KEY-X.                                               
008119         05 W-IDPROVPL           PIC 9(1)   VALUE ZERO.                   
008130         05 W-KDPROVPL           PIC X(1)   VALUE 'R'.                    
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009310 01  SSA3                        PIC X(96).                               
009400     EJECT                                                                
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009701 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
009723 01  DLI-IO-AREA-KVAH01.                                                  
009724     03  W6KVAH01.                                                        
009725*        05  -COPY W6D201                                                 
009726     SKIP3                                                                
009727 01  DLI-IO-AREA-KVAH12.                                                  
009728     03  W6KVAH12.                                                        
009729*        05  -COPY W6D212                                                 
009730     SKIP3                                                                
009735 01  DLI-IO-AREA-PROA11.                                                  
009736     03  W6PROA11.                                                        
009737*        05  -COPY W6GX6102                                               
009738     SKIP3                                                                
009739 01  DLI-IO-AREA-INLE21.                                                  
009740     03  WLINLE21.                                                        
009741*        05  -COPY WDL221                                                 
009742     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011001     EJECT                                                                
011200*01  -COPY W0008  -PRE PROA-                                              
011201     05  FILLER                  PIC X.                                   
011202     EJECT                                                                
011203*01  -COPY W0008  -PRE INLE-                                              
011204     05  FILLER                  PIC X.                                   
011205     EJECT                                                                
011206 PROCEDURE DIVISION  USING PROA-PCB INLE-PCB.                             
011210     ENTRY 'DLITCBL' USING PROA-PCB INLE-PCB.                             
011300                                                                          
011500     SKIP2                                                                
011600     PERFORM A-INIT                                                       
011710     PERFORM S01-LAES-W42601                                              
011800     PERFORM UNTIL END-OF-W42601                                          
011810                                                                          
011820       MOVE IN-W4269702     TO UT-W4269702                                
011900       MOVE IN-IDARTNR      TO W-IDARTNR                                  
012310       MOVE IN-IDLEVNR      TO W-IDLEVNR                                  
012312                                                                          
012380       IF IN-IDPROVPL-PRI NOT = '0'                                       
012382          MOVE IN-IDPROVPL-PRI    TO W-IDPROVPL                           
012390          PERFORM IMS-GU-W6PROA11                                         
012391          IF SEGMENT-FINNS                                                
012392            MOVE 6102-KVSKPLOT    TO MAX-TAL                              
012393            MOVE 6102-KVSKPLOT    TO UT-KVSKPLOT-PRI-INIT                 
012394            CALL WRANDOM USING MAX-TAL SLUMP-TAL                          
012395            IF SLUMP-TAL = +0                                             
012396               MOVE 6102-KVSKPLOT TO UT-KVSKPLOT-PRI                      
012397            ELSE                                                          
012398               MOVE SLUMP-TAL     TO UT-KVSKPLOT-PRI                      
012399            END-IF                                                        
012400          ELSE                                                            
012401            DISPLAY 'ARTNR KKOD-PRI SAKNAS ' IN-IDARTNR                   
012402                    ' KKOD '                 IN-KDKVAKTL                  
012403            CALL ABEND USING RKOD-ABEND-UTAN-DUMP                         
012404          END-IF                                                          
012405       ELSE                                                               
012406          MOVE +0              TO UT-KVSKPLOT-PRI-INIT                    
012407          MOVE +0              TO UT-KVSKPLOT-PRI                         
012408       END-IF                                                             
012412                                                                          
012413       IF IN-IDPROVPL-SEK NOT = '0'                                       
012414          MOVE IN-IDPROVPL-SEK    TO W-IDPROVPL                           
012415          PERFORM IMS-GU-W6PROA11                                         
012416          IF SEGMENT-FINNS                                                
012417            MOVE 6102-KVSKPLOT    TO MAX-TAL                              
012418            MOVE 6102-KVSKPLOT    TO UT-KVSKPLOT-SEK-INIT                 
012419            CALL WRANDOM USING MAX-TAL SLUMP-TAL                          
012420            IF SLUMP-TAL = +0                                             
012421               MOVE 6102-KVSKPLOT TO UT-KVSKPLOT-SEK                      
012422            ELSE                                                          
012423               MOVE SLUMP-TAL     TO UT-KVSKPLOT-SEK                      
012424            END-IF                                                        
012425          ELSE                                                            
012426            DISPLAY 'ARTNR KKOD-SEK SAKNAS ' IN-IDARTNR                   
012427                    ' KKOD '                 IN-KDKVAKTL                  
012428            CALL ABEND USING RKOD-ABEND-UTAN-DUMP                         
012429          END-IF                                                          
012430       ELSE                                                               
012431          MOVE +0              TO UT-KVSKPLOT-SEK-INIT                    
012432          MOVE +0              TO UT-KVSKPLOT-SEK                         
012433       END-IF                                                             
012435                                                                          
012440       IF  UT-KVSKPLOT-PRI > +0                                           
012441       AND UT-KVSKPLOT-SEK > +0                                           
012443        IF UT-KVSKPLOT-PRI = UT-KVSKPLOT-SEK                              
012445          CONTINUE                                                        
012446        ELSE                                                              
012447          IF UT-KVSKPLOT-PRI-INIT = UT-KVSKPLOT-SEK-INIT                  
012449            IF UT-KVSKPLOT-PRI > UT-KVSKPLOT-SEK                          
012451               COMPUTE W-KVSKPLOT ROUNDED = UT-KVSKPLOT-SEK +             
012452                     ((UT-KVSKPLOT-PRI - UT-KVSKPLOT-SEK) / 2)            
012453            ELSE                                                          
012455               COMPUTE W-KVSKPLOT ROUNDED = UT-KVSKPLOT-PRI +             
012456                     ((UT-KVSKPLOT-SEK - UT-KVSKPLOT-PRI) / 2)            
012457            END-IF                                                        
012458            IF W-KVSKPLOT = +0                                            
012460               MOVE UT-KVSKPLOT-PRI TO UT-KVSKPLOT-SEK                    
012461            ELSE                                                          
012463               MOVE W-KVSKPLOT      TO UT-KVSKPLOT-SEK                    
012464               MOVE W-KVSKPLOT      TO UT-KVSKPLOT-PRI                    
012465            END-IF                                                        
012466          ELSE                                                            
012467           IF UT-KVSKPLOT-PRI-INIT > UT-KVSKPLOT-SEK-INIT                 
012469             IF UT-KVSKPLOT-PRI < UT-KVSKPLOT-SEK                         
012471               MOVE UT-KVSKPLOT-PRI TO UT-KVSKPLOT-SEK                    
012472             ELSE                                                         
012474               MOVE UT-KVSKPLOT-PRI TO W-KVSKPLOT                         
012475               PERFORM UNTIL W-KVSKPLOT NOT > UT-KVSKPLOT-SEK-INIT        
012477                 COMPUTE W-KVSKPLOT =                                     
012478                         W-KVSKPLOT - UT-KVSKPLOT-SEK-INIT                
012479                 IF W-KVSKPLOT < +1                                       
012481                    MOVE UT-KVSKPLOT-PRI TO UT-KVSKPLOT-SEK               
012482                 END-IF                                                   
012483               END-PERFORM                                                
012484               MOVE W-KVSKPLOT      TO UT-KVSKPLOT-SEK                    
012485             END-IF                                                       
012486           ELSE                                                           
012487             IF UT-KVSKPLOT-SEK < UT-KVSKPLOT-PRI                         
012489               MOVE UT-KVSKPLOT-SEK TO UT-KVSKPLOT-PRI                    
012490             ELSE                                                         
012491               MOVE UT-KVSKPLOT-SEK TO W-KVSKPLOT                         
012492               PERFORM UNTIL W-KVSKPLOT NOT > UT-KVSKPLOT-PRI-INIT        
012494                 COMPUTE W-KVSKPLOT =                                     
012495                         W-KVSKPLOT - UT-KVSKPLOT-PRI-INIT                
012496                 IF W-KVSKPLOT < +1                                       
012498                    MOVE UT-KVSKPLOT-SEK TO UT-KVSKPLOT-PRI               
012499                 END-IF                                                   
012500               END-PERFORM                                                
012501               MOVE W-KVSKPLOT TO UT-KVSKPLOT-PRI                         
012502             END-IF                                                       
012503           END-IF                                                         
012504          END-IF                                                          
012505         END-IF                                                           
012506       END-IF                                                             
012507                                                                          
012508       MOVE NEJ TO TIUPPDAT-SW                                            
012509       PERFORM IMS-GU-WLINLE01                                            
012510       IF SEGMENT-FINNS                                                   
012512         PERFORM IMS-GNP-WLINLE21                                         
012513         PERFORM UNTIL SEGMENT-SAKNAS OR TIUPPDAT-FOUND                   
012515           IF MOT-KDRT = +0 OR +1 OR +2 OR +4 OR +5 OR +9 OR +10          
012517             MOVE JA        TO TIUPPDAT-SW                                
012518             MOVE MOT-TIUPPDAT TO UT-TIUPPDAT                             
012519           END-IF                                                         
012520           PERFORM IMS-GNP-WLINLE21                                       
012521         END-PERFORM                                                      
012522       END-IF                                                             
012523                                                                          
012524       IF NOT TIUPPDAT-FOUND                                              
012525         MOVE DAGENS-DATUM  TO UT-TIUPPDAT                                
012526       END-IF                                                             
012527                                                                          
012528       WRITE UTPOST                                                       
012529                                                                          
012530       PERFORM S01-LAES-W42601                                            
012600     END-PERFORM                                                          
012700                                                                          
012900     PERFORM Z-FINIT                                                      
013000                                                                          
013100     MOVE ZERO TO RETURN-CODE                                             
013200     GOBACK                                                               
013300     .                                                                    
013400     EJECT                                                                
013500 A-INIT SECTION.                                                          
013601                                                                          
013610     OPEN INPUT  W42601                                                   
013620          OUTPUT W42602                                                   
013800     SKIP2                                                                
013900     ACCEPT DAGENS-DATUM  FROM DATE                                       
014010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014200     .                                                                    
014300     EJECT                                                                
014400 Z-FINIT SECTION.                                                         
014510     CLOSE W42601 W42602                                                  
014601     SKIP2                                                                
014602     MOVE 'S' TO POSTSUM-OPKOD                                            
014610     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014801     EJECT                                                                
014802 S01-LAES-W42601  SECTION.                                                
014803     SKIP2                                                                
014804     READ W42601                                                          
014805     AT END                                                               
014807        SET END-OF-W42601 TO TRUE                                         
014808                                                                          
014809     NOT AT END                                                           
014810        MOVE 'W42601' TO POSTSUM-FDNAMN                                   
014811        MOVE 'W42601D1' TO POSTSUM-DDNAMN2                                
014812        MOVE 'KVAL'    TO POSTSUM-TRANSTYP                                
014813        CALL POSTSUM USING POSTSUM-PARM                                   
014814     END-READ                                                             
014820     .                                                                    
015100     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900     SKIP3                                                                
016101 IMS-GU-W6PROA11 SECTION.                                                 
016102                                                                          
016103     STRING 'W6PROA01(W6GXKEY  =' W-W6GX-6101-KEY-X ')'                   
016104            DELIMITED BY SIZE INTO SSA1                                   
016106     STRING 'W6PROA11(W6GXKEY  =' W-W6GX-6102-KEY-X ')'                   
016107            DELIMITED BY SIZE INTO SSA2                                   
016108     MOVE '  GE'                  TO GODK-STATUSKODER                     
016109     CALL CBLTDLI USING GU PROA-PCB DLI-IO-AREA-PROA11 SSA1 SSA2          
016110     MOVE PROA-STATUS-CODE TO STATUS-WS                                   
016111     PERFORM IMS-STATUSKONTROLL                                           
016112     .                                                                    
016120     SKIP3                                                                
016130 IMS-GU-WLINLE01 SECTION.                                                 
016140                                                                          
016150     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
016160            DELIMITED BY SIZE INTO SSA1                                   
016190     MOVE '  GE'                  TO GODK-STATUSKODER                     
016191     CALL CBLTDLI USING GU INLE-PCB DLI-IO-AREA-INLE21                    
016192                                   SSA1                                   
016194     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
016195     PERFORM IMS-STATUSKONTROLL                                           
016196     .                                                                    
016197     SKIP3                                                                
016198 IMS-GNP-WLINLE21 SECTION.                                                
016199                                                                          
016202     MOVE   'WLINLE11 '         TO SSA1                                   
016203     STRING 'WLINLE21(IDPTYP   =' W-IDPTYP                                
016204                    '&IDLEVNR  =' W-IDLEVNR-X ')'                         
016205            DELIMITED BY SIZE INTO SSA2                                   
016206     MOVE '  GE'                TO GODK-STATUSKODER                       
016207     CALL CBLTDLI USING GNP INLE-PCB DLI-IO-AREA-INLE21                   
016208                                        SSA1 SSA2                         
016210     MOVE INLE-STATUS-CODE      TO STATUS-WS                              
016211     PERFORM IMS-STATUSKONTROLL                                           
016212     .                                                                    
016213     SKIP3                                                                
016230 IMS-STATUSKONTROLL SECTION.                                              
016300     SKIP2                                                                
016400     SET STATUS-IX TO 1                                                   
016500     SEARCH GODK-STATUS                                                   
016600       AT END                                                             
016700         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
016800         DISPLAY FELTEXT                                                  
016900         CALL FELLOG                                                      
017000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017100         CONTINUE                                                         
017200     END-SEARCH                                                           
017300     .                                                                    

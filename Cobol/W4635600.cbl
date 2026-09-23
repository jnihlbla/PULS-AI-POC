000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4635600.                                                
000300 AUTHOR.         BO SVENSSON, MARKUS ASPFJÄLL                             
000400 DATE-WRITTEN.   99/03/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LEVERANSMEDDELANDEN FRÅN FRÅN DIREKTLEVERANTÖR,                  
000900*        KOMPLETTERAR FRÅN ORDER OCH ARTIKELREG,                          
001000*        SKAPAR TRANS SOM SKALL TILL AUTOMATPACKNING I PULS,              
001100*        OM FEL HITTAS SKAPAS FELPOSTER.                                  
001200*                                                                         
001300*        PROGRAMMET LÄSER      WDE4                                       
001400*        PROGRAMMET LÄSER      WDE6                                       
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- KVANTJUSTERAD FIL FRÅN DIREKTLEVERANTÖR                    
002900     SELECT W4635B                     ASSIGN TO W46356D1.                
003000     SKIP2                                                                
003100*          --- TRANSAR TILL AUTOMATPACKNING                               
003200     SELECT W46357                     ASSIGN TO W46356D2.                
003300     SKIP2                                                                
003400*          --- FELPOSTER                                                  
003500     SELECT W46358                     ASSIGN TO W46356D3.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W4635B                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  -COPY W46356      -L.                                                
004600     SKIP3                                                                
004700 FD  W46357                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  POST -COPY W46357 -PRE  UT-  -L.                                     
005200     SKIP3                                                                
005300 FD  W46358                                                               
005400     RECORDING       V                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W46358 -PRE  FEL-  -L.                                    
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000                                                                          
006100                                                                          
006200*    -- CHECKED BY WY2000                                                 
006300 77  IDPGM                       PIC X(8)    VALUE 'W4635600'.            
006400 77  WS-FELTEXT                  PIC X(20)   VALUE SPACE.                 
006500 77  WS-UTIX                     PIC S9(4)   COMP SYNC VALUE ZERO.        
006600 77  MAX-UTIX                    PIC 9(2)    VALUE 74.                    
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900                                                                          
007000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007100     88 OK                                   VALUE 'J'.                   
007200     88 FEL                                  VALUE 'N'.                   
007300                                                                          
007400 77  FIRST-SW                    PIC X       VALUE 'J'.                   
007500     88 FIRST-TIME                           VALUE 'J'.                   
007600     88 NOT-FIRST                            VALUE 'N'.                   
007700                                                                          
007800 77  FIRST-POST-SW               PIC X       VALUE 'J'.                   
007900     88 FIRST-POST                           VALUE 'J'.                   
008000     88 NOT-FIRST-POST                       VALUE 'N'.                   
008100                                                                          
008200 77  W4635B-EOF-SW               PIC X       VALUE 'N'.                   
008300     88  END-OF-W4635B                       VALUE 'J'.                   
008400                                                                          
008500 01  WS-IDORDNR7                 PIC 9(7)    VALUE ZERO.                  
008600 01  FILLER REDEFINES WS-IDORDNR7.                                        
008700     03  FILLER                  PIC X(2).                                
008800     03  WS-IDORDNR5             PIC X(5).                                
008900                                                                          
009000 01  WS-VIKT                     PIC 9(6)V9(1).                           
009100 01  WS-VIKT-X REDEFINES WS-VIKT PIC X(7).                                
009200 01  WS-VIKT-UT.                                                          
009300     03 WS-VIKT-HEL-UT           PIC X(6).                                
009400     03 WS-VIKT-PKT-UT           PIC X.                                   
009500     03 WS-VIKT-DEC-UT           PIC X.                                   
009600                                                                          
009700 01  WS-VOLYM                    PIC 9(4)V9(3).                           
009800 01  WS-VOLYM-X REDEFINES WS-VOLYM PIC X(7).                              
009900 01  WS-VOLYM-UT.                                                         
010000     03 WS-VOLYM-HEL-UT          PIC X(4).                                
010100     03 WS-VOLYM-PKT-UT          PIC X.                                   
010200     03 WS-VOLYM-DEC-UT          PIC X(3).                                
010300                                                                          
010400 01  WS-IDDISTR-X.                                                        
010500     03  WS-IDDISTR-N            PIC 9(4).                                
010600                                                                          
010700 01  WS-KVLEVART-X.                                                       
010800     03  WS-KVLEVART-N           PIC 9(6).                                
010900                                                                          
011000 01  WS-DADAT.                                                            
011100     03  WS-SEKEL                PIC 9(2)    VALUE ZERO.                  
011200     03  WS-TIDAT                PIC 9(6)    VALUE ZERO.                  
011300 01  WS-DADAT-N REDEFINES WS-DADAT                                        
011400                                 PIC 9(8).                                
011500                                                                          
011600     EJECT                                                                
011700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011800 01  FILLER REDEFINES DAGENS-DATUM.                                       
011900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012200                                                                          
012300     EJECT                                                                
012400 01  DYNAMISKA-SUBPROGRAM.                                                
012500*                                                                         
012600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013000     SKIP2                                                                
013100*    --- PARAMETRAR TILL ABEND                                            
013200                                                                          
013300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013600     SKIP2                                                                
013700 01  FELTEXT.                                                             
013800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014000     EJECT                                                                
014100*    --- PARAMETRAR TILL POSTSUM                                          
014200*                                                                         
014300*01  -COPY W0005   -PRE  POSTSUM-                                         
014400     EJECT                                                                
014500 01  IN-AREA-START               PIC X(24)   VALUE                        
014600                                 'IN-AREA-START  '.                       
014700     SKIP2                                                                
014800                                                                          
014900*01  AREA -COPY W46356     -PRE IN-                                       
015000     EJECT                                                                
015100 01  UT-AREA-START               PIC X(24)   VALUE                        
015200                                 'UT-AREA-START  '.                       
015300     SKIP2                                                                
015400                                                                          
015500*01  AREA -COPY W46357     -PRE UT-                                       
015600     EJECT                                                                
015700 01  FEL-AREA-START              PIC X(24)   VALUE                        
015800                                 'FEL-AREA-START  '.                      
015900     SKIP2                                                                
016000                                                                          
016100*01  AREA -COPY W46358      -PRE FEL-                                     
016200     EJECT                                                                
016300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016400*                                                                         
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016700     SKIP3                                                                
016800 01  NYCKLAR-TILL-DLI.                                                    
016900     03  W-IDPURAD-X.                                                     
017000         05  W-IDPURAD           PIC S9(5)   VALUE ZERO COMP-3.           
017100     03  W-IDPRODNR-X.                                                    
017200         05  W-IDPRODNR          PIC S9(7)   VALUE ZERO COMP-3.           
017300     03  W-IDKOLLI-X.                                                     
017400         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
017500                                                                          
017600     SKIP2                                                                
017700*    --- STATUS-KOD FRÅN IMS                                              
017800 01  STATUS-WS                   PIC XX.                                  
017900     88  SEGMENT-FINNS                       VALUE '  '.                  
018000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018200     SKIP2                                                                
018300 01  GODK-STATUSKODER.                                                    
018400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018500     SKIP3                                                                
018600 01  SSA1                        PIC X(64).                               
018700 01  SSA2                        PIC X(64).                               
018800     EJECT                                                                
018900*    --- IMS FUNKTIONSKODER                                               
019000*01  -COPY W0003                                                          
019100     EJECT                                                                
019200*    ---  DLI INPUT-OUTPUT AREA                                           
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-E401'.                        
019400 01  DLI-IO-E401.                                                         
019500*    03  -COPY WDE401                                                     
019600     EJECT                                                                
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-E411'.                        
019800 01  DLI-IO-E411.                                                         
019900*    03  -COPY WDE411                                                     
020000 01  FILLER         PIC X(16) VALUE 'DLI-IO-E601'.                        
020100 01  DLI-IO-E601.                                                         
020200*    03  -COPY WDE601                                                     
020300     EJECT                                                                
020400 01  FILLER         PIC X(16) VALUE 'DLI-IO-E611'.                        
020500 01  DLI-IO-E611.                                                         
020600*    03  -COPY WDE611                                                     
020700     EJECT                                                                
020800 LINKAGE SECTION.                                                         
020900                                                                          
021000     EJECT                                                                
021100*01  -COPY W0008  -PRE WDE4-                                              
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400*01  -COPY W0008  -PRE WDE6-                                              
021500     05  FILLER                  PIC X.                                   
021600     EJECT                                                                
021700 PROCEDURE DIVISION  USING WDE4-PCB WDE6-PCB.                             
021800 MAIN SECTION.                                                            
021900     ENTRY 'DLITCBL' USING WDE4-PCB WDE6-PCB.                             
022000                                                                          
022100     PERFORM A-INIT                                                       
022200                                                                          
022300     PERFORM S01-LAES-W4635B                                              
022400                                                                          
022500     MOVE IN-IDPRODNR      TO UT-IDPRODNR                                 
022600     MOVE IN-IDKOLLI       TO UT-IDKOLLI                                  
022700     MOVE JA               TO UT-FLSLUT                                   
022800     IF NOT END-OF-W4635B                                                 
022900       PERFORM UNTIL END-OF-W4635B                                        
023000         IF IN-IDPRODNR NOT = UT-IDPRODNR OR                              
023100            IN-IDKOLLI  NOT = UT-IDKOLLI OR                               
023200            UT-FLSLUT   NOT = JA                                          
023300           IF IN-IDPRODNR NOT = UT-IDPRODNR                               
023400             MOVE JA TO UT-FLSLUT-VORD                                    
023500           END-IF                                                         
023600           PERFORM S11-SKRIV-W46357                                       
023700           MOVE JA TO UT-FLSLUT                                           
023800           MOVE +1              TO WS-UTIX                                
023900           PERFORM UNTIL WS-UTIX > MAX-UTIX                               
024000             MOVE ALL '+'       TO UT-IDRADNR (WS-UTIX)                   
024100                                   UT-KVLEVART(WS-UTIX)                   
024200                                   UT-KDARTURS (WS-UTIX)                  
024300             MOVE ZERO          TO UT-IDARTNR (WS-UTIX)                   
024400             ADD +1             TO WS-UTIX                                
024500           END-PERFORM                                                    
024600                                                                          
024700           MOVE ZERO          TO WS-UTIX                                  
024800         END-IF                                                           
024900                                                                          
025000         IF IN-IDPRODNR NOT = UT-IDPRODNR OR FIRST-POST                   
025100           PERFORM B-KONTROLLERA-IDPRODNR                                 
025200         END-IF                                                           
025300         IF IN-IDKOLLI  NOT = UT-IDKOLLI OR FIRST-POST                    
025400           PERFORM C-KONTROLLERA-IDKOLLI                                  
025500         END-IF                                                           
025600         PERFORM D-KONTROLLERA-RADNR                                      
025700                                                                          
025800         IF IN-FLFEL = JA                                                 
025900           MOVE 'Case no already exists' TO FEL-FELTEXT                   
026000           MOVE NEJ                      TO INDATA-SW                     
026100         END-IF                                                           
026200                                                                          
026300         IF FEL AND FIRST-TIME                                            
026400           PERFORM S12-SKRIV-W46358                                       
026500           MOVE NEJ TO FIRST-SW                                           
026600         END-IF                                                           
026700                                                                          
026800         PERFORM S01-LAES-W4635B                                          
026900       END-PERFORM                                                        
027000       MOVE JA TO UT-FLSLUT-VORD                                          
027100       MOVE JA TO UT-FLSLUT                                               
027200       PERFORM S11-SKRIV-W46357                                           
027300     END-IF                                                               
027400                                                                          
027500     PERFORM Z-FINIT                                                      
027600                                                                          
027700     MOVE ZERO TO RETURN-CODE                                             
027800     GOBACK                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 A-INIT SECTION.                                                          
028200                                                                          
028300     OPEN INPUT  W4635B                                                   
028400                                                                          
028500     OPEN OUTPUT W46357                                                   
028600                 W46358                                                   
028700                                                                          
028800     ACCEPT DAGENS-DATUM  FROM DATE                                       
028900     MOVE DAGENS-DATUM   TO WS-TIDAT                                      
029000     IF DAGENS-DATUM-AAR > 50                                             
029100      MOVE 19            TO WS-SEKEL                                      
029200     ELSE                                                                 
029300      MOVE 20            TO WS-SEKEL                                      
029400     END-IF                                                               
029500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029600     .                                                                    
029700     EJECT                                                                
029800 B-KONTROLLERA-IDPRODNR SECTION.                                          
029900                                                                          
030000     MOVE ALL '+'       TO UT-W46357                                      
030100     MOVE IN-IDPRODNR   TO UT-IDPRODNR                                    
030200     MOVE IN-IDPRODNR   TO W-IDPRODNR                                     
030300     MOVE IN-IDKUNDNR   TO UT-IDKUNDNR                                    
030400     MOVE IN-IDORDNR7   TO WS-IDORDNR7                                    
030500     MOVE WS-IDORDNR5   TO UT-IDORDNR                                     
030600     MOVE IN-IDLEVNR    TO UT-IDANSTNR                                    
030700*    MOVE IN-IDDC       TO UT-IDDC                                        
030800     MOVE 'DDGS P '     TO UT-IDSNDNOD                                    
030900     MOVE ZERO          TO WS-UTIX                                        
031000     MOVE JA            TO INDATA-SW                                      
031100     MOVE JA            TO FIRST-SW                                       
031200     MOVE NEJ           TO UT-FLSLUT-VORD                                 
031300     MOVE JA            TO UT-FLSLUT                                      
031400                                                                          
031500     PERFORM IMS-GU-WDE601                                                
031600     IF SEGMENT-FINNS                                                     
031700       MOVE VORD-IDDISTR  TO WS-IDDISTR-N                                 
031800       MOVE WS-IDDISTR-X  TO UT-IDDISTR                                   
031900       MOVE VORD-IDDC     TO UT-IDDC                                      
032000                                                                          
032100       PERFORM IMS-GU-WDE401-ESEQ                                         
032200       IF  SEGMENT-SAKNAS                                                 
032300         MOVE 'Order no is missing' TO FEL-FELTEXT                        
032400         MOVE NEJ                   TO INDATA-SW                          
032500       ELSE                                                               
032600         IF KORD-IDORDNR5 = UT-IDORDNR AND                                
032700            KORD-IDKUNDNR = IN-IDKUNDNR                                   
032800           CONTINUE                                                       
032900         ELSE                                                             
033000           IF KORD-IDORDNR5 NOT = UT-IDORDNR                              
033100             MOVE 'Order no is wrong' TO FEL-FELTEXT                      
033200           END-IF                                                         
033300           IF KORD-IDKUNDNR NOT = IN-IDKUNDNR                             
033400             MOVE 'Cust no is wrong' TO FEL-FELTEXT                       
033500           END-IF                                                         
033600           MOVE NEJ            TO INDATA-SW                               
033700         END-IF                                                           
033800       END-IF                                                             
033900     ELSE                                                                 
034000       MOVE 'Prod no is missing' TO FEL-FELTEXT                           
034100       MOVE NEJ                  TO INDATA-SW                             
034200     END-IF                                                               
034300                                                                          
034400     .                                                                    
034500     EJECT                                                                
034600 C-KONTROLLERA-IDKOLLI SECTION.                                           
034700     IF OK                                                                
034800       MOVE IN-IDKOLLI         TO W-IDKOLLI                               
034900                                                                          
035000       IF W-IDKOLLI = 0                                                   
035100         MOVE 'Case no is wrong' TO FEL-FELTEXT                           
035200         MOVE NEJ                TO INDATA-SW                             
035300       END-IF                                                             
035300       IF OK                                                              
035300         IF IN-DIKOLLIL = 0 OR                                            
035300            IN-DIKOLLIB = 0 OR                                            
035300            IN-DIKOLLIH = 0                                               
035300           MOVE 'Case dimension is zero' TO FEL-FELTEXT                   
035300           MOVE NEJ              TO INDATA-SW                             
035300         END-IF                                                           
035300       END-IF                                                             
035400       PERFORM IMS-GNP-WDE611                                             
035500       IF SEGMENT-FINNS                                                   
035600         MOVE 'Case no already exists' TO FEL-FELTEXT                     
035700         MOVE NEJ                TO INDATA-SW                             
035800       END-IF                                                             
035900     END-IF                                                               
036000                                                                          
036100     MOVE IN-IDKOLLI            TO UT-IDKOLLI                             
036200     MOVE IN-IDSUPREF           TO UT-IDSUPREF                            
036300     MOVE IN-DASUPREF           TO UT-DASUPREF                            
036400     MOVE IN-TISUPTID           TO UT-TISUPTID                            
036500     MOVE IN-VKORDBTO-KOLLI     TO WS-VIKT                                
036600     MOVE WS-VIKT-X (1:6)       TO WS-VIKT-HEL-UT                         
036700     MOVE '.'                   TO WS-VIKT-PKT-UT                         
036800     MOVE WS-VIKT-X (7:1)       TO WS-VIKT-DEC-UT                         
036900     MOVE WS-VIKT-UT            TO UT-VKORDBTO-KOLLI                      
037000     MOVE IN-KDEMBTYP           TO UT-KDEMBTYP                            
037100     MOVE IN-DIKOLLIL           TO UT-DIKOLLIL                            
037200     MOVE IN-DIKOLLIB           TO UT-DIKOLLIB                            
037300     MOVE IN-DIKOLLIH           TO UT-DIKOLLIH                            
037400     MOVE IN-VLORDBTO-KOLLI     TO WS-VOLYM                               
037500     MOVE WS-VOLYM-X (1:4)      TO WS-VOLYM-HEL-UT                        
037600     MOVE '.'                   TO WS-VOLYM-PKT-UT                        
037700     MOVE WS-VOLYM-X (5:3)      TO WS-VOLYM-DEC-UT                        
037800     MOVE WS-VOLYM-UT           TO UT-VLORDBTO-KOLLI                      
037900     MOVE +1                    TO WS-UTIX                                
038000     PERFORM UNTIL WS-UTIX > MAX-UTIX                                     
038100     MOVE ALL '+'               TO UT-IDRADNR (WS-UTIX)                   
038200                                   UT-KVLEVART(WS-UTIX)                   
038300                                   UT-KDARTURS (WS-UTIX)                  
038400     ADD +1 TO WS-UTIX                                                    
038500     END-PERFORM                                                          
038600     MOVE ZERO                  TO WS-UTIX                                
038700                                                                          
038800     .                                                                    
038900     EJECT                                                                
039000 D-KONTROLLERA-RADNR SECTION.                                             
039100     IF OK                                                                
039200       MOVE IN-IDRADNR            TO W-IDPURAD                            
039300       PERFORM IMS-GNP-WDE411                                             
039400                                                                          
039500       IF  SEGMENT-SAKNAS                                                 
039600         MOVE 'Line no is missing' TO FEL-FELTEXT                         
039700         MOVE NEJ                  TO INDATA-SW                           
039800       ELSE                                                               
039900         IF ORAD-IDARTNR NOT = IN-IDARTNR                                 
040000          MOVE 'Partno is wrong'  TO FEL-FELTEXT                          
040100          MOVE NEJ                TO INDATA-SW                            
040200         ELSE                                                             
040300          IF IN-KVLEVART + ORAD-KVLEVART > ORAD-KVAVBART                  
040400           MOVE 'Wrong qty'       TO FEL-FELTEXT                          
040500           MOVE NEJ               TO INDATA-SW                            
040600          END-IF                                                          
040700         END-IF                                                           
040800       END-IF                                                             
040900     END-IF                                                               
041000     ADD  +1                    TO WS-UTIX                                
041100     IF  WS-UTIX = MAX-UTIX                                               
041200         MOVE NEJ               TO UT-FLSLUT                              
041300     END-IF                                                               
041400     MOVE IN-IDRADNR            TO UT-IDRADNR(WS-UTIX)                    
041500     MOVE IN-IDARTNR            TO UT-IDARTNR(WS-UTIX)                    
041600     MOVE IN-KVLEVART           TO WS-KVLEVART-N                          
041700     MOVE WS-KVLEVART-X         TO UT-KVLEVART(WS-UTIX)                   
041800     MOVE IN-KDARTURS           TO UT-KDARTURS(WS-UTIX)                   
041900     MOVE NEJ TO FIRST-POST-SW                                            
042000     .                                                                    
042100     EJECT                                                                
042200 Z-FINIT SECTION.                                                         
042300     CLOSE W4635B                                                         
042400           W46357                                                         
042500           W46358                                                         
042600     SKIP2                                                                
042700     MOVE 'S' TO POSTSUM-OPKOD                                            
042800     CALL POSTSUM USING POSTSUM-PARM                                      
042900     .                                                                    
043000     EJECT                                                                
043100 S01-LAES-W4635B  SECTION.                                                
043200     READ W4635B INTO IN-AREA                                             
043300     AT END                                                               
043400        MOVE HIGH-VALUE TO IN-AREA                                        
043500        SET END-OF-W4635B TO TRUE                                         
043600                                                                          
043700     NOT AT END                                                           
043800        MOVE 'W4635B' TO POSTSUM-FDNAMN                                   
043900        MOVE 'W46356D1' TO POSTSUM-DDNAMN2                                
044000        MOVE SPACE TO POSTSUM-TRANSTYP                                    
044100        CALL POSTSUM USING POSTSUM-PARM                                   
044200     END-READ                                                             
044300     .                                                                    
044400     EJECT                                                                
044500 S11-SKRIV-W46357 SECTION.                                                
044600                                                                          
044700     WRITE UT-POST FROM UT-AREA                                           
044800                                                                          
044900     MOVE SPACE TO POSTSUM-TRANSTYP                                       
045000     MOVE 'W46357' TO POSTSUM-FDNAMN                                      
045100     MOVE 'W46356D2' TO POSTSUM-DDNAMN2                                   
045200     CALL POSTSUM USING POSTSUM-PARM                                      
045300     .                                                                    
045400     EJECT                                                                
045500 S12-SKRIV-W46358 SECTION.                                                
045600                                                                          
045700     MOVE WS-DADAT           TO FEL-DAREGDAT                              
045800     MOVE IN-IDPRODNR        TO FEL-IDPRODNR                              
045900     MOVE IN-IDSUPREF        TO FEL-IDSUPREF                              
046000     WRITE FEL-POST FROM FEL-AREA                                         
046100                                                                          
046200     MOVE SPACE TO POSTSUM-TRANSTYP                                       
046300     MOVE 'W46358' TO POSTSUM-FDNAMN                                      
046400     MOVE 'W46356D3' TO POSTSUM-DDNAMN2                                   
046500     CALL POSTSUM USING POSTSUM-PARM                                      
046600     .                                                                    
046700     EJECT                                                                
046800* --- IMS SEKTIONER ---                                                   
046900     SKIP3                                                                
047000 IMS-GU-WDE401-ESEQ SECTION.                                              
047100                                                                          
047200     STRING 'WDE401  (WDE4ESEQ =' W-IDPRODNR-X ')'                        
047300          DELIMITED BY SIZE INTO SSA1                                     
047400     MOVE '  GE' TO GODK-STATUSKODER                                      
047500     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-E401 SSA1                      
047600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
047700     PERFORM IMS-STATUSKONTROLL                                           
047800     .                                                                    
047900     EJECT                                                                
048000 IMS-GNP-WDE411 SECTION.                                                  
048100                                                                          
048200     STRING 'WDE411  *F(IDPURAD  =' W-IDPURAD-X ')'                       
048300          DELIMITED BY SIZE INTO SSA1                                     
048400     MOVE '  GE' TO GODK-STATUSKODER                                      
048500     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-E411 SSA1                     
048600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
048700     PERFORM IMS-STATUSKONTROLL                                           
048800     .                                                                    
048900     EJECT                                                                
049000 IMS-GU-WDE601 SECTION.                                                   
049100                                                                          
049200     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
049300          DELIMITED BY SIZE INTO SSA1                                     
049400     MOVE '  GE' TO GODK-STATUSKODER                                      
049500     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-E601 SSA1                      
049600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
049700     PERFORM IMS-STATUSKONTROLL                                           
049800     .                                                                    
049900     EJECT                                                                
050000 IMS-GNP-WDE611 SECTION.                                                  
050100                                                                          
050200     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
050300          DELIMITED BY SIZE INTO SSA1                                     
050400     MOVE '  GE' TO GODK-STATUSKODER                                      
050500     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-E611 SSA1                     
050600     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
050700     PERFORM IMS-STATUSKONTROLL                                           
050800     .                                                                    
050900     EJECT                                                                
051000 IMS-STATUSKONTROLL SECTION.                                              
051100                                                                          
051200     SET STATUS-IX TO 1                                                   
051300     SEARCH GODK-STATUS                                                   
051400       AT END                                                             
051500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
051600           DELIMITED BY SIZE INTO FELTEXT                                 
051700         DISPLAY FELTEXT                                                  
051800         CALL FELLOG                                                      
051900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
052000         CONTINUE                                                         
052100     END-SEARCH                                                           
052200     .                                                                    

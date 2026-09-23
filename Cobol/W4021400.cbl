000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4021400.                                                
000400 AUTHOR.         GÖRAN KJELLSON   GUIDE DATAKONSULT AB                    
000500 DATE-WRITTEN.   APRIL -90.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR TILLÄGG AV UPPGIFTER I ORDERHUVUD            
001100*        PÅ ORDERKÖN.                                                     
001200*        TILLÄGG KAN NEDAST GÖRAS PÅ ORDER SOM FINNS PÅ                   
001300*        ORDERKÖN OCH INTE ÄR AVSLUTADE.                                  
001400*        KONTROLL GÖRS SÅ ATT TILLÄGG INTE KAN GÖRAS                      
001500*        PÅ ORDER DÄR MAN INTE ÄR BEHÖRIG.                                
001600*        EFTER TILLÄGG AV UPPGIFTER SKER UTHOPP TILL                      
001700*        REGISTRERING AV ORDERRADER.                                      
001800*                                                                         
001900*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002000*        PROGRAMMET UPPDATERAR WLORQI (WDQ2)  ORDERHUVUD                  
002100*        PROGRAMMET UPPDATERAR WLXXKP (WDR1)  ORDERNUMMERREGISTER         
002200*                                                                         
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T214                                              
002600*        MID:         W4I21401                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O21401                                            
003000     SKIP2                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     EJECT                                                                
003300                                                                          
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)  VALUE 'W4021400'.             
003900 77  JA                          PIC X      VALUE 'J'.                    
004000 77  NEJ                         PIC X      VALUE 'N'.                    
004100 77  SPRAK-IX                    PIC S9     VALUE +0    COMP-3.           
004200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +0    COMP SYNC.        
004300 77  4212-MOD-LAENGD             PIC S9(4)  VALUE +107  COMP SYNC.        
004400 77  WS-INDEX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004500 77  HOPP                        PIC X(1)   VALUE 'N'.                    
004600                                                                          
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004800                                                                          
004900 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
005000 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
005100 77  WS-IDORDNR                  PIC X(5)    VALUE SPACE.                 
005200 77  WS-TEDDI                    PIC X(11)   VALUE SPACE.                 
005300 77  WS-KDTULLVE                 PIC 9(1).                                
005400 77  WS-KDVRINFO                 PIC 9(1).                                
005500                                                                          
005600 77  INMATNING-SW                PIC X       VALUE 'J'.                   
005700     88  INMATNING-FINNS                     VALUE 'J'.                   
005800                                                                          
005900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006000     88  ALLT-OK                             VALUE 'J'.                   
006110                                                                          
006200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006300     88  EGEN-MID                            VALUE '4214'.                
006400     88  GODK-MID                            VALUE '4211' '4212'          
006500                                                   '4213' '4214'.         
006600     SKIP2                                                                
006700*                                                                         
006800*    ----DISTR-DEALER-PRICE--------                                       
006900*01  -COPY WWDIST79                                                       
007000                                                                          
007100 01  WS-ALFA-1.                                                           
007200     03  WS-NUM-1                PIC 9(1).                                
007300 01  WS-ALFA-7-4.                                                         
007400     03 WS-NUM-7-4               PIC 9(11).                               
007500     03 FILLER REDEFINES WS-NUM-7-4.                                      
007600        05  FILLER               PIC X.                                   
007700        05 WS-ALFA-6.                                                     
007800            07 WS-NUM-6          PIC 9(6).                                
007900        05 WS-ALFA-4.                                                     
008000            07 WS-NUM-4          PIC 9(4).                                
008100                                                                          
008200     EJECT                                                                
008300                                                                          
008400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008500 01  GENERELLA-SUBPROGRAM.                                                
008600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000*                                                                         
009100*                                                                         
009200 01  GEMENSAMMA-SUBPROGRAM.                                               
009300     03  W411OHFK                PIC X(8)    VALUE 'W411OHFK'.            
009400*        FORMELLA KONTROLLER                                              
009500     EJECT                                                                
010000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010100*   -COPY WMEDAREA                                                        
010200     EJECT                                                                
010300 01  MESSAGE-CODES.                                                       
010400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010500     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
010600     03  ERR-ORDER-KLAR          PIC X(3)    VALUE '057'.                 
010700     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
010800     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '001'.                 
010900     EJECT                                                                
011000                                                                          
011100*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
011200*   -COPY W411OHFK                                                        
011300     EJECT                                                                
011400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011600     SKIP3                                                                
011700*01  MID -COPY W4I21401                                                   
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012000     SKIP3                                                                
012100*01  -COPY WMSGAREA                                                       
012200     EJECT                                                                
012300*    03  MOD -COPY W4O21401   -RED MSG-AREA.                              
012400     EJECT                                                                
012500*    03 -COPY W4O21201 -PRE 4212-  -RED MSG-AREA.                         
012600     EJECT                                                                
012700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012800     SKIP3                                                                
012900*01  -COPY WMFSAREA                                                       
013000     EJECT                                                                
013100                                                                          
013200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013300*                                                                         
013400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013500     SKIP3                                                                
013600 01  NYCKLAR-TILL-DLI.                                                    
013700     03  W-WDQ2CSEQ-X.                                                    
013800         05 W-IDDISTR            PIC S9(5)   VALUE ZERO COMP-3.           
013900         05 W-IDKUNDNR           PIC S9(7)   VALUE ZERO COMP-3.           
014000         05 W-IDKUNDRF.                                                   
014100            07 W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
014200            07 FILLER            PIC X(3)    VALUE SPACE.                 
014300*                                                                         
014400     03  W-IDDC-X.                                                        
014500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016200*    --- STATUS-KOD FRÅN IMS                                              
016300 01  STATUS-WS                   PIC XX.                                  
016400     88  SEGMENT-FINNS                       VALUE '  '.                  
016500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016700     SKIP2                                                                
016800 01  GODK-STATUSKODER.                                                    
016900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017000     SKIP3                                                                
017100 01  SSA1                        PIC X(64).                               
017200     EJECT                                                                
017300                                                                          
017400*    --- IMS FUNKTIONSKODER                                               
017500*01  -COPY W0003                                                          
017600     EJECT                                                                
017700*    ---  DLI INPUT-OUTPUT AREA                                           
017800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017900     SKIP3                                                                
018000 01  DLI-IO-OHUV.                                                         
018100*    05  -COPY WDQ201                                                     
018200     EJECT                                                                
018300 01  DLI-IO-ARB.                                                          
018400*    03  -COPY WDQ212                                                     
019500                                                                          
019600 01  FILLER                  PIC X(16)  VALUE 'P-TO-P-SW'.                
019700 01  4297-MSG-IO-AREA.                                                    
019800     03  4297-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
019900     03  4297-Z1               PIC X.                                     
020000     03  4297-Z2               PIC X.                                     
020100     03  4297-TRANSKOD         PIC X(8)   VALUE 'W4T297X '.               
020200     03  4297-IDTRANS          PIC X(4)   VALUE '4214'.                   
020300     03  4297-SPRAK            PIC X.                                     
020400*    03  -COPY W4I29701  -PRE 4297-                                       
020500     EJECT                                                                
020600                                                                          
020700 LINKAGE SECTION.                                                         
020800*01  -COPY W0009   -PRE MSG-                                              
020900                                                                          
021000*01  -COPY W0009   -PRE 4297-                                             
021100     EJECT                                                                
021400*01  -COPY W0008   -PRE ORQI-                                             
021500     05  FILLER                  PIC X.                                   
022200     SKIP2                                                                
022300                                                                          
022400 01  ORDN-XXKP-PCB               PIC X.                                   
022500                                                                          
022600     EJECT                                                                
022700                                                                          
022800 PROCEDURE DIVISION  USING MSG-PCB 4297-PCB                               
022900                           ORQI-PCB                                       
023000                           ORDN-XXKP-PCB.                                 
023100     ENTRY 'DLITCBL' USING MSG-PCB 4297-PCB                               
023200                           ORQI-PCB                                       
023300                           ORDN-XXKP-PCB.                                 
023400     PERFORM IMS-GET-MSG                                                  
023500     IF SEGMENT-FINNS                                                     
023610        PERFORM A-INIT                                                    
023700        PERFORM B-KONTROLLERA-NYCKLAR                                     
023800        IF ALLT-OK                                                        
023900           PERFORM C-KOLLA-ORDERN                                         
024000           IF ALLT-OK                                                     
024100              IF MFS-FIRST                                                
024200                 PERFORM D-REDIGERA-BILD                                  
024300              ELSE                                                        
024400                 IF INMATNING-FINNS                                       
024500                    PERFORM E-KONTROLLERA-FORMELLA-FEL                    
024600                 END-IF                                                   
024700                 IF ALLT-OK                                               
024800                    PERFORM H-UPPDATERA-ORDERHUVUD                        
024900                    IF ARB-KDROPACK = 'L'                                 
025000                       PERFORM J-STARTA-BIPACKNINGEN                      
025100                    ELSE                                                  
025200                       PERFORM K-HOPPA-TILL-RADREGISTRERING               
025300                    END-IF                                                
025400                 END-IF                                                   
025500              END-IF                                                      
025600           END-IF                                                         
025700        END-IF                                                            
025800        IF HOPP = NEJ                                                     
025900           PERFORM Z-AVSLUTA                                              
026000        END-IF                                                            
026100     END-IF                                                               
026200     MOVE +0 TO RETURN-CODE                                               
026300     GOBACK                                                               
026400     .                                                                    
026500     EJECT                                                                
026600                                                                          
026700 A-INIT SECTION.                                                          
026800                                                                          
026900     MOVE SPACE                TO MED-IDMFSFEL                            
027000     MOVE JA                   TO ALLT-SW                                 
027100                                                                          
027200     IF MSG-DUBBLA-TRANSKODER                                             
027300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I21401                 
027400       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
027500       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
027600     ELSE                                                                 
027700       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I21401                  
027800       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
027900       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
028000     END-IF                                                               
028100                                                                          
028200     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
028300     MOVE MSG-IDPFK            TO MFS-IDPFK                               
028400     MOVE MFS-IDTRANS          TO W-IDTRANS                               
028500     MOVE LOW-VALUE            TO MSG-AREA                                
028600     MOVE 'W4O214N1'           TO MFS-IDMOD                               
028700     MOVE '4214'               TO MOD-IDTRANS                             
028800     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL                            
028900                                  MOD-TEMFSINF                            
029000     COMPUTE MAX-MOD-LAENGD = LENGTH OF MOD-W4O21401 + 4                  
029100     IF NOT EGEN-MID                                                      
029200       MOVE SPACE              TO MFS-KDTRTYP                             
029300       MOVE '7'                TO MFS-IDPFK                               
029400     END-IF                                                               
029500                                                                          
029600     IF ENGLISH-TEXT                                                      
029700       MOVE +2                 TO SPRAK-IX                                
029800       MOVE 'GB '              TO MED-IDSKYLT                             
029900     ELSE                                                                 
030000       MOVE +1                 TO SPRAK-IX                                
030100       MOVE 'S  '              TO MED-IDSKYLT                             
030200     END-IF                                                               
030300                                                                          
030400     .                                                                    
030500     EJECT                                                                
030600 B-KONTROLLERA-NYCKLAR SECTION.                                           
030700                                                                          
030800     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
030900                                  MOD-IDKUNDNR-IN                         
031000                                  MOD-IDORDNR-IN                          
031110                                                                          
031200     IF MID-IDDISTR-IN NOT = ALL '+'                                      
031300       MOVE MID-IDDISTR-IN     TO WS-IDDISTR                              
031310       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
031400       MOVE '7'                TO MFS-IDPFK                               
031500     ELSE                                                                 
031600       MOVE MID-IDDISTR-UT     TO WS-IDDISTR                              
031700       INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                 
031800     END-IF                                                               
031900                                                                          
032000     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
032100        MOVE MID-IDKUNDNR-IN   TO WS-IDKUNDNR                             
032200        MOVE '7'               TO MFS-IDPFK                               
032300     ELSE                                                                 
032400        MOVE MID-IDKUNDNR-UT   TO WS-IDKUNDNR                             
032500        INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO               
032600     END-IF                                                               
032700                                                                          
032800     IF MID-IDORDNR-IN NOT = ALL '+'                                      
032900        MOVE MID-IDORDNR-IN    TO WS-IDORDNR                              
033000        MOVE '7'               TO MFS-IDPFK                               
033100     ELSE                                                                 
033200        MOVE MID-IDORDNR-UT    TO WS-IDORDNR                              
033300        INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                
033400     END-IF                                                               
033500                                                                          
033600     IF WS-IDDISTR NUMERIC AND WS-IDDISTR > ZERO                          
033700        MOVE WS-IDDISTR         TO W-IDDISTR                              
033710        MOVE WS-IDDISTR         TO DIST79-IDDISTR                         
033720        IF DIST79-DEALER-PRICE                                            
033730          IF ENGLISH-TEXT                                                 
033740             MOVE 'DEALERPRICE' TO MOD-TEDDI                              
033750                                        WS-TEDDI                          
033760          ELSE                                                            
033770             MOVE 'ÅFP'         TO MOD-TEDDI                              
033780                                        WS-TEDDI                          
033790          END-IF                                                          
033791        ELSE                                                              
033792          MOVE ' '              TO MOD-TEDDI                              
033793                                        WS-TEDDI                          
033794        END-IF                                                            
033800     ELSE                                                                 
033900        MOVE NEJ                TO ALLT-SW                                
034000     END-IF                                                               
034100                                                                          
035500                                                                          
035600     EJECT                                                                
035700     IF WS-IDKUNDNR NUMERIC                                               
035800        MOVE WS-IDKUNDNR     TO W-IDKUNDNR                                
035900     ELSE                                                                 
036000        MOVE NEJ             TO ALLT-SW                                   
036100     END-IF                                                               
036200                                                                          
036300     IF WS-IDORDNR NUMERIC AND WS-IDORDNR > ZERO                          
036400        MOVE WS-IDORDNR      TO W-IDORDNR                                 
036500     ELSE                                                                 
036600        MOVE NEJ             TO ALLT-SW                                   
036700     END-IF                                                               
036800                                                                          
036900     IF GODK-MID OR ALLT-OK                                               
037000       MOVE WS-IDDISTR         TO MOD-IDDISTR-UT                          
037100       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
037200                                                                          
037300       MOVE WS-IDKUNDNR        TO MOD-IDKUNDNR-UT                         
037400       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
037500       IF WS-IDKUNDNR = ZERO                                              
037600          MOVE '     0'        TO MOD-IDKUNDNR-UT                         
037700       END-IF                                                             
037800                                                                          
037900       MOVE WS-IDORDNR         TO MOD-IDORDNR-UT                          
038000       INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE             
038100     ELSE                                                                 
038200       MOVE MFS-RENSA-FAELT     TO MOD-IDDISTR-UT                         
038300                                   MOD-IDKUNDNR-UT                        
038400                                   MOD-IDORDNR-UT                         
038500     END-IF                                                               
038600                                                                          
038700     IF NOT ALLT-OK                                                       
038800        MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                            
038900        PERFORM MFS-RENSA-MOD-FAELT                                       
039000     END-IF                                                               
039100     .                                                                    
039200     EJECT                                                                
039300                                                                          
039400 C-KOLLA-ORDERN SECTION.                                                  
039500                                                                          
039600     PERFORM IMS-01-GET-ORQI-WDQ201                                       
039700                                                                          
039800     IF SEGMENT-FINNS                                                     
039900       IF OHUV-FLKLAR = NEJ                                               
040000         IF OHUV-IDSYSTEM = '4211'                                        
040100           IF OHUV-IDUSER = MSG-SIGNON-USERID                             
040200                                                                          
040300              MOVE OHUV-IDDC-PRIM      TO W-IDDC                          
040400              PERFORM IMS-02-GNP-ORQI-WDQ212                              
040500                                                                          
040600              PERFORM CA-KOLLA-OM-INMATNING                               
040800           ELSE                                                           
040900              MOVE ERR-OBEHORIG    TO MED-IDMFSFEL                        
041000              MOVE NEJ             TO ALLT-SW                             
041100              PERFORM MFS-RENSA-MOD-FAELT                                 
041200           END-IF                                                         
041300         ELSE                                                             
041400           MOVE 'FEL BILD.....'    TO MOD-TEMFSFEL                        
041500           MOVE NEJ                TO ALLT-SW                             
041600           PERFORM MFS-RENSA-MOD-FAELT                                    
041700         END-IF                                                           
041800       ELSE                                                               
041900         MOVE ERR-ORDER-KLAR       TO MED-IDMFSFEL                        
042000         MOVE NEJ                  TO ALLT-SW                             
042100         PERFORM MFS-RENSA-MOD-FAELT                                      
042200       END-IF                                                             
042300     ELSE                                                                 
042400        MOVE ERR-ORDER-SAKNAS      TO MED-IDMFSFEL                        
042500        MOVE NEJ                   TO ALLT-SW                             
042600        PERFORM MFS-RENSA-MOD-FAELT                                       
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000                                                                          
043100 CA-KOLLA-OM-INMATNING SECTION.                                           
043200                                                                          
043300     MOVE JA               TO INMATNING-SW                                
043400                                                                          
043500     MOVE OHUV-KDTULLVE    TO WS-KDTULLVE                                 
043600     MOVE OHUV-KDVRINFO    TO WS-KDVRINFO                                 
043700                                                                          
043800     IF  MID-BEBET    = OHUV-BEBET   AND                                  
043900         MID-ADBET    = OHUV-ADBET   AND                                  
044000         MID-IDSKYLT  = OHUV-IDSKYLT AND                                  
044100         MID-KDTULLVE = WS-KDTULLVE  AND                                  
044200         MID-KDVRINFO = WS-KDVRINFO                                       
044300                                                                          
044400         MOVE NEJ              TO INMATNING-SW                            
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800                                                                          
044900                                                                          
046300 D-REDIGERA-BILD SECTION.                                                 
046400                                                                          
046500     MOVE W-IDDC               TO MOD-IDDC-UT                             
046600     MOVE OHUV-BEBET           TO MOD-BEBET                               
046700     MOVE OHUV-ADBET           TO MOD-ADBET                               
046800     MOVE OHUV-IDSKYLT         TO MOD-IDSKYLT                             
046900     MOVE ARB-TIRFS            TO WS-NUM-7-4                              
047000     MOVE WS-ALFA-6            TO MOD-TIRFS-DAT                           
047100     MOVE WS-ALFA-4            TO MOD-TIRFS-TID                           
047200     MOVE OHUV-TITPO           TO MOD-TITPO                               
047300     MOVE OHUV-KDTULLVE        TO MOD-KDTULLVE                            
047400     MOVE OHUV-KDVRINFO        TO MOD-KDVRINFO                            
047500     .                                                                    
047600     EJECT                                                                
047700                                                                          
047800 E-KONTROLLERA-FORMELLA-FEL SECTION.                                      
047900                                                                          
048000     MOVE 'IMS '               TO OHFK-IDSYSTEM                           
048100     MOVE WS-IDDISTR           TO OHFK-IDDISTR                            
048200     MOVE WS-IDKUNDNR          TO OHFK-IDKUNDNR                           
048300     MOVE WS-IDORDNR           TO OHFK-IDORDNR                            
048400     MOVE MID-IDSKYLT          TO OHFK-IDSKYLT                            
048500     MOVE MID-KDTULLVE         TO OHFK-KDTULLVE                           
048600     MOVE MID-KDVRINFO         TO OHFK-KDVRINFO                           
048700     MOVE ALL '+'              TO OHFK-TIREGDAT                           
048800                                  OHFK-TIHHMM                             
048900                                  OHFK-IDDC                               
049000                                  OHFK-KDORDKL                            
049100                                  OHFK-KDFRAKT                            
049200                                  OHFK-FLAUTFAK                           
049300                                  OHFK-FLFORBI                            
049400                                  OHFK-FLAUTPAC                           
049500                                  OHFK-FLORDSPE                           
049600                                  OHFK-FLLSBOK                            
049700                                  OHFK-FLVORKO                            
049800                                  OHFK-FLRESTN                            
049900                                  OHFK-IDBIPREF                           
050000                                  OHFK-IDFTG                              
050100                                  OHFK-IDKONTO                            
050200                                  OHFK-IDKST                              
050300                                  OHFK-IDKAMPRF                           
050400                                  OHFK-KDFAKTYP                           
050500                                  OHFK-KDROPACK                           
050600                                  OHFK-KDTPOTYP                           
050700                                  OHFK-TIFORDAT                           
050800                                  OHFK-TITPO                              
050900                                  OHFK-TIRFSDAT                           
051000                                  OHFK-TIRFSTID                           
051100                                  OHFK-KDPROTYP                           
051200                                                                          
051300     CALL W411OHFK USING OHFK-W411OHFK                                    
051400     PERFORM EA-KOLLA-FEL-FK                                              
051500     .                                                                    
051600     EJECT                                                                
051700                                                                          
051800 EA-KOLLA-FEL-FK SECTION.                                                 
051900                                                                          
052000     IF OHFK-IDSKYLT-OK = NEJ                                             
052100        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
052200        MOVE MFS-ALFA-FAELT-FEL  TO MOD-IDSKYLT-ATTR                      
052300        MOVE NEJ                 TO ALLT-SW                               
052400     END-IF                                                               
052500                                                                          
052600     IF OHFK-KDTULLVE-OK = NEJ                                            
052700        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
052800        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDTULLVE-ATTR                     
052900        MOVE NEJ                 TO ALLT-SW                               
053000     END-IF                                                               
053100                                                                          
053200     IF OHFK-KDVRINFO-OK = NEJ                                            
053300        MOVE ERR-UPPLYSTA-FEL TO MED-IDMFSFEL                             
053400        MOVE MFS-NUM-FAELT-FEL   TO MOD-KDVRINFO-ATTR                     
053500        MOVE NEJ                 TO ALLT-SW                               
053600     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900                                                                          
054000 H-UPPDATERA-ORDERHUVUD SECTION.                                          
054110                                                                          
054200     IF INMATNING-FINNS                                                   
054210        PERFORM IMS-GHU-WDQ201                                            
054300                                                                          
054400        MOVE W-IDORDNR             TO OHUV-IDKUNDRF                       
054500        MOVE NEJ                   TO OHUV-FLBORT                         
054600                                                                          
054700        MOVE MID-ADBET             TO OHUV-ADBET                          
054800        INSPECT OHUV-ADBET REPLACING ALL '+' BY SPACE                     
054900                                                                          
055000        MOVE MID-BEBET             TO OHUV-BEBET                          
055100        INSPECT OHUV-BEBET  REPLACING ALL '+' BY SPACE                    
055200                                                                          
055300        MOVE MID-IDSKYLT           TO OHUV-IDSKYLT                        
055400        INSPECT OHUV-IDSKYLT REPLACING ALL '+' BY SPACE                   
055500                                                                          
055600        MOVE MID-KDTULLVE          TO WS-ALFA-1                           
055700        INSPECT WS-ALFA-1     REPLACING ALL '+' BY ZERO                   
055800        MOVE WS-NUM-1              TO OHUV-KDTULLVE                       
055900                                                                          
056000        MOVE MID-KDVRINFO          TO WS-ALFA-1                           
056100        INSPECT WS-ALFA-1     REPLACING ALL '+' BY ZERO                   
056200        MOVE WS-NUM-1              TO OHUV-KDVRINFO                       
056300                                                                          
056500        PERFORM IMS-REPL-WDQ201                                           
056610        PERFORM IMS-01-GET-ORQI-WDQ201                                    
056620     END-IF                                                               
056700     .                                                                    
056800     EJECT                                                                
056900                                                                          
057000 J-STARTA-BIPACKNINGEN SECTION.                                           
057100                                                                          
057200     COMPUTE 4297-LL = LENGTH OF 4297-MID-W4I29701 + 17                   
057300     MOVE MFS-KDMFSFOR           TO 4297-SPRAK                            
057400                                                                          
057500     MOVE W-IDDISTR              TO 4297-MID-IDDISTR                      
057600     MOVE W-IDKUNDNR             TO 4297-MID-IDKUNDNR                     
057700     MOVE W-IDKUNDRF             TO 4297-MID-IDKUNDRF                     
057800     MOVE OHUV-KDTPOTYP          TO 4297-MID-KDTPOTYP                     
057900     MOVE OHUV-KDORDKL           TO 4297-MID-KDORDKL                      
058000     MOVE OHUV-KDFAKTYP          TO 4297-MID-KDFAKTYP                     
058100     MOVE OHUV-IDKAMPRF          TO 4297-MID-IDKAMPRF                     
058200     MOVE OHUV-IDKONTO           TO 4297-MID-IDKONTO                      
058300     MOVE OHUV-IDKST             TO 4297-MID-IDKST                        
058400     MOVE OHUV-IDANALYS          TO 4297-MID-IDANALYS                     
058500     MOVE OHUV-FLFORBI           TO 4297-MID-FLFORBI                      
058600     MOVE OHUV-IDORDER           TO 4297-MID-IDORDER                      
058700     MOVE OHUV-BEKUNDRF          TO 4297-MID-BEKUNDRF                     
058800     MOVE OHUV-TIREGDAT          TO 4297-MID-TIREGDAT                     
058900     MOVE OHUV-BEVARREF          TO 4297-MID-BEVARREF                     
059000     MOVE OHUV-IDFTG             TO 4297-MID-IDFTG                        
059100     MOVE OHUV-IDBIPREF          TO 4297-MID-IDBIPREF                     
059220     MOVE ARB-KDROPACK           TO 4297-MID-KDROPACK                     
059300     MOVE ARB-KDFRAKT            TO 4297-MID-KDFRAKT                      
059400     IF OHUV-IDDC-TVS NOT = SPACE                                         
059500       MOVE OHUV-IDDC-TVS        TO 4297-MID-IDDC                         
059600     ELSE                                                                 
059700       MOVE SPACE                TO 4297-MID-IDDC                         
059800     END-IF                                                               
059900                                                                          
060000     PERFORM IMS-INSERT-4297-MSG                                          
060100     MOVE JA                     TO HOPP                                  
060200     .                                                                    
060300     EJECT                                                                
060400                                                                          
060510 K-HOPPA-TILL-RADREGISTRERING SECTION.                                    
060600                                                                          
060700     MOVE 'W4O212N1'            TO MFS-IDMOD                              
060800                                                                          
061100     MOVE '4212'                TO 4212-MOD-IDTRANS                       
061200     MOVE MFS-RENSA-FAELT       TO 4212-MOD-TEMFSFEL                      
061300     MOVE WS-IDDISTR            TO 4212-MOD-IDDISTR                       
061301     INSPECT 4212-MOD-IDDISTR REPLACING LEADING ZERO BY SPACE             
061302**   MOVE W-IDKUNDNR            TO WS-IDKUNDNR                            
061303     MOVE WS-IDKUNDNR            TO 4212-MOD-IDKUNDNR                     
061304     INSPECT 4212-MOD-IDKUNDNR REPLACING LEADING ZERO BY SPACE            
061305     MOVE W-IDORDNR(3:5)        TO 4212-MOD-IDORDNR5                      
061306     INSPECT 4212-MOD-IDORDNR5 REPLACING LEADING ZERO BY SPACE            
061307     MOVE OHUV-KDORDKL          TO 4212-MOD-KDORDKL                       
061308     MOVE ARB-KDFRAKT           TO 4212-MOD-KDFRAKT                       
061309     MOVE WS-TEDDI              TO 4212-MOD-TEDDI                         
061310     MOVE OHUV-BEKUNDRF         TO 4212-MOD-BEVOLREF                      
061410     MOVE SPACE                 TO 4212-MOD-KDVALISO                      
061500     MOVE MFS-ADD-SAETT-CURSOR  TO 4212-MOD-IDARTNR-ATTR(1)               
061600                                                                          
061700     MOVE 4212-MOD-LAENGD      TO MSG-KVLL                                
061800     PERFORM IMS-INSERT-MSG                                               
061900                                                                          
062000     MOVE JA                    TO HOPP                                   
062100     .                                                                    
062200     EJECT                                                                
062300                                                                          
064500 Z-AVSLUTA SECTION.                                                       
064600                                                                          
064700     IF MED-IDMFSFEL NOT = SPACE                                          
064800        CALL WMEDKONV USING MED-WMEDAREA                                  
064900        MOVE MED-MFSFEL      TO MOD-TEMFSFEL                              
065000     END-IF                                                               
065100                                                                          
065200     IF NOT ALLT-OK AND NOT MFS-FIRST                                     
065300        PERFORM MFS-ROER-EJ-BILD                                          
065400     END-IF                                                               
065500                                                                          
065600     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
065700     PERFORM IMS-INSERT-MSG                                               
065800     .                                                                    
065900     EJECT                                                                
066000                                                                          
066100 MFS-RENSA-MOD-FAELT SECTION.                                             
066200                                                                          
066300     MOVE MFS-RENSA-FAELT      TO MOD-BEBETRAD-1                          
066400                                  MOD-BEBETRAD-2                          
066500                                  MOD-ADBETRAD-1                          
066600                                  MOD-ADBETRAD-2                          
066700                                  MOD-IDSKYLT                             
066800                                  MOD-KDTULLVE                            
066900                                  MOD-KDVRINFO                            
067000                                  MOD-TITPO                               
067100                                  MOD-TIRFS-DAT                           
067200                                  MOD-TIRFS-TID                           
067300                                  MOD-TEMFSFEL                            
067400                                  MOD-TEMFSINF                            
067500     .                                                                    
067600     SKIP2                                                                
067700                                                                          
067800 MFS-ROER-EJ-BILD SECTION.                                                
067900                                                                          
068000     MOVE MFS-ROER-EJ-FAELT    TO MOD-BEBETRAD-1                          
068100                                  MOD-BEBETRAD-2                          
068200                                  MOD-ADBETRAD-1                          
068300                                  MOD-ADBETRAD-2                          
068400                                  MOD-IDSKYLT                             
068500                                  MOD-KDTULLVE                            
068600                                  MOD-KDVRINFO                            
068700                                  MOD-TITPO                               
068800                                  MOD-TIRFS-DAT                           
068900                                  MOD-TIRFS-TID                           
069000     .                                                                    
069100     EJECT                                                                
069200                                                                          
069300* --- IMS SEKTIONER ---                                                   
069400     SKIP2                                                                
069500 IMS-GET-MSG SECTION.                                                     
069600                                                                          
069700     MOVE '  QC' TO GODK-STATUSKODER                                      
069800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
069900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070000     PERFORM IMS-STATUSKONTROLL                                           
070100     .                                                                    
070200     SKIP2                                                                
070300 IMS-INSERT-MSG SECTION.                                                  
070400                                                                          
070500     IF NOT ENGLISH-TEXT                                                  
070600       MOVE '0' TO MFS-KDHUVOMR                                           
070700     END-IF                                                               
070800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
070900     MOVE SPACE TO GODK-STATUSKODER                                       
071000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
071100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071200     PERFORM IMS-STATUSKONTROLL                                           
071300     .                                                                    
071400     SKIP2                                                                
071500 IMS-INSERT-4297-MSG SECTION.                                             
071600                                                                          
071700     MOVE LOW-VALUE TO 4297-Z1 4297-Z2                                    
071800     MOVE SPACE TO GODK-STATUSKODER                                       
071900     CALL CBLTDLI USING ISRT 4297-PCB 4297-MSG-IO-AREA                    
072000     MOVE 4297-STATUS-CODE TO STATUS-WS                                   
072100     PERFORM IMS-STATUSKONTROLL                                           
072200     .                                                                    
072300     EJECT                                                                
072400 IMS-01-GET-ORQI-WDQ201 SECTION.                                          
072500                                                                          
072600     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
072700          DELIMITED BY SIZE INTO SSA1                                     
072800     MOVE '  GE' TO GODK-STATUSKODER                                      
072900     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-OHUV SSA1                      
073000     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
073100     PERFORM IMS-STATUSKONTROLL                                           
073200     .                                                                    
073300     SKIP3                                                                
073400 IMS-GHU-WDQ201 SECTION.                                                  
073500                                                                          
073600     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
073700          DELIMITED BY SIZE INTO SSA1                                     
073800     MOVE '    ' TO GODK-STATUSKODER                                      
073900     CALL CBLTDLI USING GHU ORQI-PCB DLI-IO-OHUV SSA1                     
074000     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
074110     PERFORM IMS-STATUSKONTROLL                                           
074200     .                                                                    
074300     SKIP3                                                                
074400 IMS-REPL-WDQ201 SECTION.                                                 
074500     SKIP2                                                                
074600     MOVE '  ' TO GODK-STATUSKODER                                        
074700     CALL CBLTDLI USING REPL ORQI-PCB DLI-IO-OHUV                         
074800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
074900     PERFORM IMS-STATUSKONTROLL                                           
075000     .                                                                    
075100     EJECT                                                                
075200 IMS-02-GNP-ORQI-WDQ212 SECTION.                                          
075300                                                                          
075400     STRING 'WLORQI12(IDDC     =' W-IDDC-X ')'                            
075500          DELIMITED BY SIZE INTO SSA1                                     
075600     MOVE '  ' TO GODK-STATUSKODER                                        
075700     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-ARB SSA1                      
075800     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
075900     PERFORM IMS-STATUSKONTROLL                                           
076000     .                                                                    
076100     SKIP3                                                                
079300 IMS-STATUSKONTROLL SECTION.                                              
079400                                                                          
079500     SET STATUS-IX TO 1                                                   
079600     SEARCH GODK-STATUS                                                   
079700       AT END CALL FELLOG                                                 
079800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
079900     END-SEARCH                                                           
080000     .                                                                    

000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4023300.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   JAN  1991.                                               
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET HANTERAR SVARSBILD TILL                               
001100*        SPECIALORDERREGISTRERING                                         
001200*        VISAR AVVIKELSER (KOD 53, 58 & 59)                               
001300*        UPPDATERING AV GODKÄNDA RADER. FLOBOK BLIR = 'J'.                
001400*        ANNULLATION AV HEL ORDER MÖJLIG.                                 
001500*                                                                         
001600*        EFTER AVSLUTAD BEHANDLING SKER UTHOPP TILL ORDERHUVUD            
001700*        4231.                                                            
001800*                                                                         
001900*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002000*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKR.BAS               
002100*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD SEK-IX           
002200*        PROGRAMMET LÄSER      WLBENA (WDD3)  BENÄMNINGSREGISTER          
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T233                                              
002600*                     W4T233U                                             
002700*                     W4T233V                                             
002800*        MID:         W4I23301                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W4O23301                                            
003200*                                                                         
003300*    E'TRACKER: 10263222 2015      FORCE TO END ORDER REG                 
003400     EJECT                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600                                                                          
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(08)   VALUE 'W4023300'.            
004200                                                                          
004300 77  JA                          PIC X(1)   VALUE 'J'.                    
004400 77  NEJ                         PIC X(1)   VALUE 'N'.                    
004500 77  HOPP                        PIC X(1)   VALUE 'N'.                    
004600 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004700 77  4231-MOD-LAENGD             PIC S9(4)  VALUE +44   COMP SYNC.        
004800 77  WS-INDEX                    PIC S9(9)   COMP SYNC VALUE ZERO.        
004900 77  WS-INDEX-MID                PIC S9(9)   COMP SYNC VALUE ZERO.        
005000 77  WS-INDEX-MID-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
005100 77  WS-INDEX-MOD                PIC S9(9)   COMP SYNC VALUE ZERO.        
005200 77  WS-INDEX-MOD-MAX            PIC S9(9)   COMP SYNC VALUE +13.         
005300 77  WS-IDDISTR                  PIC X(4).                                
005400 77  WS-IDKUNDNR                 PIC X(6).                                
005500 77  WS-IDORDNR                  PIC X(5).                                
005600 01  ORDERNR-TILL-4231.                                                   
005700    03  FILLER                   PIC X(15).                               
005800    03  WS-4231-ORDERNR-TEXT     PIC X(17)  VALUE SPACE.                  
005900    03  WS-IDKUNDRF              PIC X(5).                                
006000     EJECT                                                                
006100                                                                          
006200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006300     88  ALLT-OK                             VALUE 'J'.                   
006400 77  NYCKEL-SW                   PIC X       VALUE 'J'.                   
006500     88  NYCKEL-OK                           VALUE 'J'.                   
006600 77  AVSLUTA-SW                  PIC X       VALUE 'N'.                   
006700     88  AVSLUTA                             VALUE 'J'.                   
006800                                                                          
006900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007000     88  EGEN-MID                            VALUE '4233'.                
007100     88  GODK-MID                            VALUE '4231' '4232'          
007200                                                   '4233' '0813'.         
007300                                                                          
007400 01  WS-AKTUELL-MID-RAD.                                                  
007500     03  WS-AKT-KDORDBEK         PIC 9(2).                                
007600     03  WS-AKT-IDARTNR          PIC 9(9).                                
007700     03  WS-AKT-FILLER           PIC X(1).                                
007800     03  WS-AKT-REKSIFFR         PIC 9(1).                                
007900     03  WS-AKT-IDDC             PIC X(2).                                
008000     03  WS-AKT-KEYS.                                                     
008100         05 WS-AKT-IDLOPNR       PIC 9(3).                                
008200         05 WS-AKT-IDSEKVNR      PIC 9(3).                                
008300                                                                          
008400 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
008500 01  FILLER  REDEFINES  TEST-IDDISTR.                                     
008600*    ----DIST79-DEALER-PRICE-----                                         
008700*    03  -COPY WWDIST79                                                   
008800     EJECT                                                                
008900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009000 01  GENERELLA-SUBPROGRAM.                                                
009100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009110     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400 01  GEMENSAMMA-PROGRAM.                                                  
009500     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
009600*        PRISFRÅGA                                                        
009700 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
009800*   -COPY W335PRQU                                                        
009900     EJECT                                                                
009910*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009920*   -COPY WMSGINIT                                                        
010000     SKIP3                                                                
010100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010200 01  MESSAGE-CODES.                                                       
010300     03  MED-FLER-SIDOR          PIC X(3)    VALUE '105'.                 
010400     03  MED-EJ-FLER-RADER       PIC X(3)    VALUE '056'.                 
010500     03  MED-ORDER-ANNULLERAD    PIC X(3)    VALUE '052'.                 
010600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010700     03  ERR-OBEHORIG            PIC X(3)    VALUE '405'.                 
010800     03  ERR-ORDER-SAKNAS        PIC X(3)    VALUE '701'.                 
010900     03  ERR-ORDER-AVSLUTAD      PIC X(3)    VALUE '057'.                 
011000     03  ERR-FEL-BILDSERIE       PIC X(3)    VALUE '093'.                 
011100     03  ERR-UPPLYSTA-FEL        PIC X(3)    VALUE '409'.                 
011200     EJECT                                                                
011300*   -COPY WMEDAREA                                                        
011400     EJECT                                                                
011500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011700     SKIP3                                                                
011800*01  MID -COPY W4I23301                                                   
011900     EJECT                                                                
012000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012100     SKIP3                                                                
012200*01  -COPY WMSGAREA                                                       
012300     EJECT                                                                
012400*    03  MOD -COPY W4O23101   -RED MSG-AREA  -PRE 4231-.                  
012500     EJECT                                                                
012600*    03  MOD -COPY W4O23301   -RED MSG-AREA.                              
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012900     SKIP3                                                                
013000*01  -COPY WMFSAREA                                                       
013100     EJECT                                                                
013200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013400                                                                          
013500 01  NYCKLAR-TILL-DLI.                                                    
013600     03  W-WDQ101-KEY-UNIK.                                               
013700         05  W-Q1-IDORDER-UNIK   PIC S9(7)   VALUE ZERO COMP-3.           
013800         05  W-Q1-IDARTNR-UNIK   PIC S9(9)   VALUE ZERO COMP-3.           
013900         05  W-Q1-IDLOPNR-UNIK   PIC S9(3)   VALUE ZERO COMP-3.           
014000         05  W-Q1-IDSEKVNR-UNIK  PIC S9(3)   VALUE ZERO COMP-3.           
014100         05  W-Q1-IDDC-UNIK      PIC  X(2)   VALUE ZERO.                  
014200         05  W-Q1-KDORDBEK-UNIK  PIC 9(2)    VALUE ZERO.                  
014300                                                                          
014400     03  W-WDQ101-KEY-MIN.                                                
014500         05  W-Q1-IDORDER-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
014600         05  W-Q1-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
014700         05  W-Q1-IDLOPNR-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
014800         05  W-Q1-IDSEKVNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
014900         05  W-Q1-IDDC-MIN       PIC  X(2)   VALUE ZERO.                  
015000         05  W-Q1-KDORDBEK-MIN   PIC 9(2)    VALUE ZERO.                  
015100                                                                          
015200     03  W-WDQ101-KEY-MAX.                                                
015300         05  W-Q1-IDORDER-MAX    PIC S9(7) VALUE 9999999 COMP-3.          
015400         05  W-Q1-IDARTNR-MAX    PIC S9(9) VALUE 999999999 COMP-3.        
015500         05  W-Q1-IDLOPNR-MAX    PIC S9(3) VALUE 999  COMP-3.             
015600         05  W-Q1-IDSEKVNR-MAX   PIC S9(3) VALUE 999  COMP-3.             
015700         05  W-Q1-IDDC-MAX       PIC  X(2) VALUE '99'.                    
015800         05  W-Q1-KDORDBEK-MAX   PIC 9(2)  VALUE 99.                      
015900     EJECT                                                                
016000                                                                          
016100     03  W-IDGMTREF-X.                                                    
016200         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
016300         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
016400         05  W-IDKUNDRF.                                                  
016500           07  W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
016600           07  FILLER            PIC X(3)    VALUE SPACE.                 
016700                                                                          
016800     03  W-IDARTNR-X.                                                     
016900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017000     03  W-IDSKYLT-X.                                                     
017100         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
017200     EJECT                                                                
017300*    --- STATUS-KOD FRÅN IMS                                              
017400 01  STATUS-WS                   PIC XX.                                  
017500     88  SEGMENT-FINNS                       VALUE '  '.                  
017600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017700     88  BASEN-SLUT                          VALUE 'GB'.                  
017800     SKIP2                                                                
017900 77  STATUS-OBKR-WS              PIC X(2)    VALUE 'GE'.                  
018000     88  OBKR-SEGMENT-FINNS                  VALUE '  '.                  
018100     SKIP2                                                                
018200 01  GODK-STATUSKODER.                                                    
018300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018400     SKIP3                                                                
018500 01  SSA1                        PIC X(160).                              
018600 01  SSA2                        PIC X(96).                               
018700                                                                          
018800     EJECT                                                                
018900*    --- IMS FUNKTIONSKODER                                               
019000*01  -COPY W0003                                                          
019100     EJECT                                                                
019200*    ---  DLI INPUT-OUTPUT AREA                                           
019300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019400     SKIP3                                                                
019500 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
019600 01  DLI-IO-AREA-ORQM.                                                    
019700     03  WLORQM01.                                                        
019800*        05  -COPY WDQ101                                                 
019900     EJECT                                                                
020000 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
020100 01  DLI-IO-AREA-ORQI01.                                                  
020200     03  WLORQI01.                                                        
020300*        05  -COPY WDQ201                                                 
020400     EJECT                                                                
020500 01  FILLER                      PIC X(16)   VALUE 'WDD311-AREA'.         
020600 01  DLI-IO-AREA-BENA.                                                    
020700     03  WLBENA11.                                                        
020800*        05  -COPY WDD311                                                 
020900     EJECT                                                                
021000                                                                          
021100*--MSG-AREOR FÖR HOPP TILL 4292-ORDERANNULLATION                          
021200*------------------------- 4298-ORDERAVSLUT                               
021300                                                                          
021400 01  FILLER                  PIC X(16)  VALUE '4292-MSG-IO-AREA'.         
021500 01  4292-MSG-IO-AREA.                                                    
021600     03  4292-LL               PIC S9(4)  VALUE +47  COMP SYNC.           
021700     03  4292-Z1               PIC X.                                     
021800     03  4292-Z2               PIC X.                                     
021900     03  4292-TRANSKOD         PIC X(8)   VALUE 'W4T292X '.               
022000     03  4292-IDTRANS          PIC X(4)   VALUE '4233'.                   
022100     03  4292-SPRAK            PIC X.                                     
022200     03  4292-IDORDER          PIC X(7).                                  
022300     03  4292-IDDISTR          PIC X(4).                                  
022400     03  4292-IDKUNDNR         PIC X(6).                                  
022500     03  4292-IDKUNDRF         PIC X(7).                                  
022600     03  FILLER                PIC X(6)   VALUE SPACE.                    
022700     EJECT                                                                
022800                                                                          
022900 01  FILLER                  PIC X(16)  VALUE '4298-MSG-IO-AREA'.         
023000 01  4298-MSG-IO-AREA.                                                    
023100     03  4298-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
023200     03  4298-Z1               PIC X.                                     
023300     03  4298-Z2               PIC X.                                     
023400     03  4298-TRANSKOD         PIC X(8)   VALUE 'W4T298X '.               
023500     03  4298-IDTRANS          PIC X(4)   VALUE '4233'.                   
023600     03  4298-SPRAK            PIC X.                                     
023700     03  -COPY W4I29801  -PRE 4298-                                       
023800     EJECT                                                                
023900 01  FILLER                  PIC X(16)  VALUE '4224-MSG-IO-AREA'.         
024000 01  4224-MSG-IO-AREA.                                                    
024100     03  4224-LL               PIC S9(4)  VALUE +262 COMP SYNC.           
024200     03  4224-Z1               PIC X      VALUE LOW-VALUE.                
024300     03  4224-Z2               PIC X      VALUE LOW-VALUE.                
024400     03  4224-TRANSKOD         PIC X(8)   VALUE 'W4T224  '.               
024500     03  4224-IDTRANS          PIC X(4)   VALUE '4223'.                   
024600     03  4224-SPRAK            PIC X.                                     
024700     03  MID -COPY W4I22401  -PRE 4224-                                   
024800     EJECT                                                                
024900 01  FILLER                  PIC X(16)  VALUE '4225-MSG-IO-AREA'.         
025000 01  4225-MSG-IO-AREA.                                                    
025100     03  4225-LL               PIC S9(4)  VALUE +90  COMP SYNC.           
025200     03  4225-Z1               PIC X      VALUE LOW-VALUE.                
025300     03  4225-Z2               PIC X      VALUE LOW-VALUE.                
025400     03  4225-TRANSKOD         PIC X(8)   VALUE 'W4T225  '.               
025500     03  4225-IDTRANS          PIC X(4)   VALUE '4223'.                   
025600     03  4225-SPRAK            PIC X.                                     
025700     03  MID -COPY W4I22501  -PRE 4225-                                   
025800     EJECT                                                                
025900 LINKAGE SECTION.                                                         
026000*01  -COPY W0009   -PRE MSG-                                              
026100*01  -COPY W0009   -PRE 4292-                                             
026200     EJECT                                                                
026300*01  -COPY W0009   -PRE 4298-                                             
026400     -COPY W0009   -PRE 4224-                                             
026500     -COPY W0009   -PRE 4225-                                             
026510*01  -COPY W0008   -PRE USEA-                                             
026520     05  FILLER                  PIC X.                                   
026530     EJECT                                                                
026600*01  -COPY W0008   -PRE BENA-                                             
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900*01  -COPY W0008   -PRE ORQI-                                             
027000     05  FILLER                  PIC X.                                   
027100*01  -COPY W0008   -PRE ORQM-                                             
027200     05  FILLER                  PIC X.                                   
027300     EJECT                                                                
027400 01  PRQU-WDG2-PCB               PIC X.                                   
027500 01  PRQU-WDC7-PCB               PIC X.                                   
027600 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
027700                                                                          
027800 PROCEDURE DIVISION  USING MSG-PCB 4292-PCB 4298-PCB 4224-PCB             
027900                          4225-PCB USEA-PCB                               
028000                          BENA-PCB ORQI-PCB ORQM-PCB                      
028100                          PRQU-WDG2-PCB                                   
028200                          PRQU-WDC7-PCB                                   
028300                          PRQU-SJKO-WDK6-PCB.                             
028400                                                                          
028500     ENTRY 'DLITCBL' USING MSG-PCB 4292-PCB 4298-PCB 4224-PCB             
028600                          4225-PCB USEA-PCB                               
028700                          BENA-PCB ORQI-PCB ORQM-PCB                      
028800                          PRQU-WDG2-PCB                                   
028900                          PRQU-WDC7-PCB                                   
029000                          PRQU-SJKO-WDK6-PCB.                             
029100     EJECT                                                                
029200     PERFORM IMS-GET-MSG                                                  
029300     IF SEGMENT-FINNS                                                     
029400        PERFORM A-INIT                                                    
029500        PERFORM B-KOLLA-NYCKLAR                                           
029600        IF NYCKEL-OK                                                      
029700           IF MFS-NEXT                                                    
029800              PERFORM C-NAESTA-SIDA                                       
029900           ELSE                                                           
030000              PERFORM D-FOERSTA-SIDA                                      
030100           END-IF                                                         
030200           IF ALLT-OK                                                     
030300              PERFORM F-KOLLA-ATT-ORDER-FINNS                             
030400              IF ALLT-OK                                                  
030500                 PERFORM E-SKRIVSKYDDA-NYCKLAR                            
030600                 IF (MFS-KDTRTYP = 'U' OR 'V') OR MFS-ENTER               
030700                    PERFORM G-KONTROLLERA-BILDEN                          
030800                 END-IF                                                   
030900                 IF ALLT-OK                                               
031000                    PERFORM H-BEHANDLA-RADER                              
031100                    IF AVSLUTA                                            
031200                       PERFORM I-STARTA-ORDERAVSLUT                       
031300                       PERFORM M-HOPPA-TILL-NY-BILD                       
031400                    END-IF                                                
031500                 END-IF                                                   
031600              END-IF                                                      
031700           END-IF                                                         
031800        END-IF                                                            
031900        IF HOPP = NEJ                                                     
032000           PERFORM Z-FINIT                                                
032100        END-IF                                                            
032200     END-IF                                                               
032300     MOVE +0 TO RETURN-CODE                                               
032400     GOBACK                                                               
032500     .                                                                    
032600     EJECT                                                                
032700 A-INIT SECTION.                                                          
032800                                                                          
032900     MOVE SPACE                TO MED-IDMFSFEL                            
033000                                  MED-IDMFSINF                            
033100     MOVE JA                   TO ALLT-SW                                 
033200                                  NYCKEL-SW                               
033300                                                                          
033400     IF MSG-DUBBLA-TRANSKODER                                             
033500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I23301                 
033600       MOVE MSG-IDTRANS-2      TO MFS-IDTRANS                             
033700       MOVE MSG-KDMFSFOR-2     TO MFS-KDMFSFOR                            
033800     ELSE                                                                 
033900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I23301                  
034000       MOVE MSG-IDTRANS-1      TO MFS-IDTRANS                             
034100       MOVE MSG-KDMFSFOR-1     TO MFS-KDMFSFOR                            
034200     END-IF                                                               
034300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
034400     MOVE MSG-IDPFK            TO MFS-IDPFK                               
034500     MOVE MFS-IDTRANS          TO W-IDTRANS                               
034600     MOVE LOW-VALUE            TO MSG-AREA                                
034700     MOVE 'W4O23301'           TO MFS-IDMOD                               
034800     MOVE '4'                  TO MOD-IDTRANS1                            
034810     MOVE '2'                  TO MOD-IDTRANS2                            
034820     MOVE '3'                  TO MOD-IDTRANS3                            
034830     MOVE '3'                  TO MOD-IDTRANS4                            
034900     MOVE MFS-RENSA-FAELT      TO MOD-TEMFSFEL MOD-TEMFSINF               
035000                                                                          
035100     IF NOT EGEN-MID                                                      
035200       MOVE SPACE              TO MFS-KDTRTYP                             
035300       MOVE '7'                TO MFS-IDPFK                               
035310       IF W-IDTRANS = '0813'                                              
035320          MOVE ALL '+'           TO MSGI-WMSGINIT                         
035330          MOVE '013'             TO MSGI-KDCALL                           
035340          MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                           
035350          MOVE '4223'            TO MSGI-IDTRANS                          
035360          MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                     
035370                                                                          
035380          CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                      
035390       END-IF                                                             
035400     END-IF                                                               
035500                                                                          
035600     IF ENGLISH-TEXT                                                      
035700       MOVE +2                 TO SPRAK-IX                                
035800       MOVE 'GB '              TO MED-IDSKYLT                             
035900     ELSE                                                                 
036000       MOVE +1                 TO SPRAK-IX                                
036100       MOVE 'S  '              TO MED-IDSKYLT                             
036200     END-IF                                                               
036300     .                                                                    
036400     EJECT                                                                
036500 B-KOLLA-NYCKLAR SECTION.                                                 
036600                                                                          
036700     MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR-IN                          
036800                                  MOD-IDKUNDNR-IN                         
036900                                  MOD-IDORDNR-IN                          
036910                                                                          
036916     IF W-IDTRANS = '0813'                                                
036917        MOVE MSGI-IDDISTR       TO MID-IDDISTR-IN                         
036918        MOVE MSGI-IDKUNDNR      TO MID-IDKUNDNR-IN                        
036919        MOVE MSGI-IDKUNDRF(3:5) TO MID-IDORDNR-IN                         
036920     END-IF                                                               
036930                                                                          
037000     IF MID-IDDISTR-IN = ALL '+'                                          
037100        MOVE MID-IDDISTR-UT    TO WS-IDDISTR                              
037200        INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                
037300     ELSE                                                                 
037400        MOVE MID-IDDISTR-IN    TO WS-IDDISTR                              
037500        MOVE '7'               TO MFS-IDPFK                               
037600        MOVE SPACE             TO MFS-KDTRTYP                             
037700     END-IF                                                               
037800                                                                          
037900     IF WS-IDDISTR NUMERIC  AND  WS-IDDISTR > ZERO                        
038000        MOVE WS-IDDISTR        TO W-IDDISTR                               
038100     ELSE                                                                 
038200        MOVE NEJ               TO NYCKEL-SW                               
038210        MOVE ZERO              TO W-IDDISTR                               
038300     END-IF                                                               
038400                                                                          
038500     MOVE W-IDDISTR            TO TEST-IDDISTR                            
038600     IF DIST79-DEALER-PRICE                                               
038700        IF ENGLISH-TEXT                                                   
038800           MOVE 'DEALERPRICE'  TO MOD-TEDDI                               
038900        ELSE                                                              
039000           MOVE '    ÅF PRIS'  TO MOD-TEDDI                               
039100        END-IF                                                            
039200     ELSE                                                                 
039300        MOVE SPACES            TO MOD-TEDDI                               
039400     END-IF                                                               
039500                                                                          
039600     IF MID-IDKUNDNR-IN = ALL '+'                                         
039700        MOVE MID-IDKUNDNR-UT   TO WS-IDKUNDNR                             
039800        INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO               
039900     ELSE                                                                 
040000        MOVE MID-IDKUNDNR-IN   TO WS-IDKUNDNR                             
040100        MOVE '7'               TO MFS-IDPFK                               
040200        MOVE SPACE             TO MFS-KDTRTYP                             
040300     END-IF                                                               
040400                                                                          
040500     IF WS-IDKUNDNR NUMERIC                                               
040600        MOVE WS-IDKUNDNR       TO W-IDKUNDNR                              
040700     ELSE                                                                 
040800        MOVE NEJ               TO NYCKEL-SW                               
040900     END-IF                                                               
041000     EJECT                                                                
041100     IF MID-IDORDNR-IN = ALL '+'                                          
041200        MOVE MID-IDORDNR-UT    TO WS-IDORDNR                              
041300        INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                
041400     ELSE                                                                 
041500        MOVE MID-IDORDNR-IN    TO WS-IDORDNR                              
041600        MOVE '7'               TO MFS-IDPFK                               
041700        MOVE SPACE             TO MFS-KDTRTYP                             
041800     END-IF                                                               
041900     IF WS-IDORDNR NUMERIC  AND WS-IDORDNR > ZERO                         
042000        MOVE WS-IDORDNR        TO W-IDORDNR                               
042100     ELSE                                                                 
042200        MOVE NEJ               TO NYCKEL-SW                               
042300     END-IF                                                               
042400                                                                          
042500     IF GODK-MID AND NYCKEL-OK                                            
042600        MOVE WS-IDKUNDNR          TO MOD-IDKUNDNR-UT                      
042700        INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE           
042800        IF WS-IDKUNDNR = ZERO                                             
042900           MOVE '     0'          TO MOD-IDKUNDNR-UT                      
043000        END-IF                                                            
043100        MOVE WS-IDDISTR           TO MOD-IDDISTR-UT                       
043200        INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE            
043300        MOVE WS-IDORDNR           TO MOD-IDORDNR-UT                       
043400        INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE            
043500     ELSE                                                                 
043600        MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR-UT                          
043700                                  MOD-IDKUNDNR-UT                         
043800                                  MOD-IDORDNR-UT                          
044000     END-IF                                                               
044100                                                                          
044200     IF NOT NYCKEL-OK                                                     
044300        MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                            
044400        PERFORM MFS-RENSA-ALLA-FAELT                                      
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800                                                                          
044900 C-NAESTA-SIDA SECTION.                                                   
045000                                                                          
045100     IF MID-IDARTNR-NEXT NUMERIC AND                                      
045110        MID-IDARTNR-NEXT > ZERO                                           
045200        MOVE MID-IDARTNR-NEXT  TO W-Q1-IDARTNR-MIN                        
045300        MOVE MID-IDLOPNR-NEXT  TO W-Q1-IDLOPNR-MIN                        
045400        MOVE MID-IDSEKVNR-NEXT TO W-Q1-IDSEKVNR-MIN                       
045500        MOVE MID-IDDC-NEXT     TO W-Q1-IDDC-MIN                           
045600        MOVE MID-KDORDBEK-NEXT TO W-Q1-KDORDBEK-MIN                       
045700     ELSE                                                                 
045800        MOVE MED-EJ-FLER-RADER TO MED-IDMFSFEL                            
045900        MOVE NEJ               TO ALLT-SW                                 
046000     END-IF                                                               
046100     .                                                                    
046200                                                                          
046300 D-FOERSTA-SIDA SECTION.                                                  
046400                                                                          
046500     MOVE ZERO                 TO W-Q1-IDARTNR-MIN                        
046600                                  W-Q1-IDLOPNR-MIN                        
046700                                  W-Q1-IDSEKVNR-MIN                       
046800                                  W-Q1-IDDC-MIN                           
046900                                  W-Q1-KDORDBEK-MIN                       
047000                                                                          
047010     IF MID-IDARTNR-NEXT NUMERIC AND                                      
047020        MID-IDARTNR-NEXT > ZERO                                           
047200        MOVE MID-IDARTNR-NEXT  TO MOD-IDARTNR-NEXT                        
047300        MOVE MID-IDLOPNR-NEXT  TO MOD-IDLOPNR-NEXT                        
047400        MOVE MID-IDSEKVNR-NEXT TO MOD-IDSEKVNR-NEXT                       
047500        MOVE MID-IDDC-NEXT     TO MOD-IDDC-NEXT                           
047600        MOVE MID-KDORDBEK-NEXT TO MOD-KDORDBEK-NEXT                       
047700     END-IF                                                               
047800     .                                                                    
047900     EJECT                                                                
048000                                                                          
048010 E-SKRIVSKYDDA-NYCKLAR SECTION.                                           
048020                                                                          
048030     MOVE MFS-STAENG-FAELT     TO MOD-IDTRANS1-ATTR                       
048040                                  MOD-IDTRANS2-ATTR                       
048050                                  MOD-IDTRANS3-ATTR                       
048060                                  MOD-IDTRANS4-ATTR                       
048070                                  MOD-IDDISTR-IN-ATTR                     
048080                                  MOD-IDKUNDNR-IN-ATTR                    
048090                                  MOD-IDORDNR-IN-ATTR                     
048091     MOVE MFS-ADD-SAETT-CURSOR TO MOD-FLANNULL-ATTR                       
048092     .                                                                    
048093     EJECT                                                                
048094                                                                          
048100 F-KOLLA-ATT-ORDER-FINNS SECTION.                                         
048200                                                                          
048300     PERFORM IMS-07-GU-ORQI-WDQ201                                        
048400     IF SEGMENT-FINNS                                                     
048500       IF OHUV-FLKLAR = JA                                                
048600         MOVE ERR-ORDER-AVSLUTAD                                          
048700                               TO MED-IDMFSFEL                            
048800         MOVE NEJ              TO ALLT-SW                                 
048900         MOVE NEJ     TO NYCKEL-SW                                        
049000         PERFORM MFS-RENSA-ALLA-FAELT                                     
049100       ELSE                                                               
049200         IF OHUV-IDSYSTEM NOT = '4231'                                    
049300           MOVE ERR-FEL-BILDSERIE                                         
049400                               TO MED-IDMFSFEL                            
049500           MOVE NEJ            TO ALLT-SW                                 
049600           MOVE NEJ            TO NYCKEL-SW                               
049700           PERFORM MFS-RENSA-ALLA-FAELT                                   
049800         ELSE                                                             
049900           MOVE OHUV-KDORDKL TO MOD-KDORDKL-UT                            
050000           MOVE OHUV-IDORDER TO W-Q1-IDORDER-UNIK                         
050100                                W-Q1-IDORDER-MIN                          
050200                                W-Q1-IDORDER-MAX                          
050300         END-IF                                                           
050400       END-IF                                                             
050500     EJECT                                                                
050600     ELSE                                                                 
050700        MOVE ERR-ORDER-SAKNAS  TO MED-IDMFSFEL                            
050800        MOVE NEJ               TO ALLT-SW                                 
050900        MOVE NEJ               TO NYCKEL-SW                               
051000        PERFORM MFS-RENSA-ALLA-FAELT                                      
051100     END-IF                                                               
051200     .                                                                    
051300     EJECT                                                                
051400                                                                          
051500 G-KONTROLLERA-BILDEN SECTION.                                            
051600                                                                          
051700     MOVE JA                   TO ALLT-SW                                 
051800                                                                          
051900     IF MID-FLANNULL NOT = '+'                                            
052000        IF MID-FLANNULL = 'J' OR 'Y' OR 'N'                               
052100           MOVE JA             TO ALLT-SW                                 
052200           IF MID-FLANNULL = 'Y'                                          
052300              MOVE JA          TO MID-FLANNULL                            
052400           END-IF                                                         
052500        ELSE                                                              
052600           MOVE MFS-ALFA-FAELT-FEL                                        
052700                               TO MOD-FLANNULL-ATTR                       
052800           MOVE ERR-UPPLYSTA-FEL                                          
052900                               TO MED-IDMFSFEL                            
053000           MOVE NEJ            TO ALLT-SW                                 
053100        END-IF                                                            
053200     ELSE                                                                 
053300        MOVE NEJ               TO MID-FLANNULL                            
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700                                                                          
053800 H-BEHANDLA-RADER SECTION.                                                
053900                                                                          
054000     IF MFS-FIRST  OR  MFS-NEXT                                           
054100                                                                          
054200        PERFORM HB-LAS-IN-13-RADER                                        
054300        IF  WS-INDEX-MOD = +1  AND  W-IDTRANS = '4232'                    
054400           MOVE JA             TO AVSLUTA-SW                              
054500        END-IF                                                            
054600     ELSE                                                                 
054700        IF MID-FLANNULL NOT = JA                                          
054800                                                                          
054900           IF MFS-UPDATE  OR  MFS-QUERY                                   
055000              PERFORM HE-UPPDATERA-AKT-SIDA                               
055100              PERFORM HD-UPPDATERA-OBEH-RADER                             
055200              PERFORM HB-LAS-IN-13-RADER                                  
055300              IF  WS-INDEX-MOD = +1                                       
055400                 MOVE JA       TO AVSLUTA-SW                              
055500              END-IF                                                      
055600           ELSE                                                           
055700              IF MFS-UPD-V                                                
055800                 PERFORM HG-UPPDATERA-RESTERANDE-RADER                    
055900                 MOVE JA       TO AVSLUTA-SW                              
056000              END-IF                                                      
056100           END-IF                                                         
056200        ELSE                                                              
056300           PERFORM HH-ANNULLERA-ORDER                                     
056400        END-IF                                                            
056500     END-IF                                                               
056600     .                                                                    
056700     EJECT                                                                
056800                                                                          
056900 HB-LAS-IN-13-RADER SECTION.                                              
057000                                                                          
057100     PERFORM MFS-RENSA-ALLA-FAELT                                         
057200                                                                          
057300     MOVE +1                   TO WS-INDEX-MOD                            
057400                                                                          
057500     PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                               
057600     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
057700              OR   WS-INDEX-MOD > WS-INDEX-MOD-MAX                        
057800                                                                          
057900                                                                          
058000           PERFORM HBA-REDIGERA-ORDERBEKR-RAD                             
058100                                                                          
058200           ADD +1              TO WS-INDEX-MOD                            
058300                                                                          
058400        PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                            
058500*-------SPARA STATUSKODEN FRÅN ORDERBEKRÄFTELSE LÄSNINGEN                 
058600        MOVE STATUS-WS      TO STATUS-OBKR-WS                             
058700     END-PERFORM                                                          
058800     PERFORM HBB-FIXA-BLADDRINGS-VARDEN                                   
058900     .                                                                    
059000     EJECT                                                                
059100                                                                          
059200 HBA-REDIGERA-ORDERBEKR-RAD SECTION.                                      
059300                                                                          
059400     MOVE OBKR-KDORDBEK        TO MOD-KDORDBEK(WS-INDEX-MOD)              
059500                                                                          
059600     MOVE 'B'                  TO MOD-KDBEHX(WS-INDEX-MOD)                
059700     MOVE OBKR-IDARTNR         TO MOD-IDARTNR-1--9(WS-INDEX-MOD)          
059800     MOVE '-'                  TO MOD-STRAEK(WS-INDEX-MOD)                
059900     MOVE OBKR-REKSIFFR        TO MOD-REKSIFFR(WS-INDEX-MOD)              
060000                                                                          
060100     MOVE OBKR-IDARTNR      TO W-IDARTNR                                  
060200     MOVE OHUV-IDSKYLT      TO W-IDSKYLT                                  
060300                                                                          
060400     PERFORM IMS-09-GU-BENA-WDD311                                        
060500     IF SEGMENT-FINNS                                                     
060600        MOVE TEXT-BEART     TO MOD-BEART(WS-INDEX-MOD)                    
060700     ELSE                                                                 
060800        MOVE SPACE          TO MOD-BEART(WS-INDEX-MOD)                    
060900     END-IF                                                               
061000                                                                          
061100     MOVE OBKR-IDDC            TO MOD-IDDC-RAD(WS-INDEX-MOD)              
061200                                                                          
061300     MOVE OBKR-KVBEART         TO MOD-KVANTAL(WS-INDEX-MOD)               
061400                                                                          
061500     MOVE OBKR-IDLOPNR         TO WS-AKT-IDLOPNR                          
061600     MOVE OBKR-IDSEKVNR        TO WS-AKT-IDSEKVNR                         
061700                                                                          
061800     MOVE WS-AKT-KEYS          TO MOD-KEYS(WS-INDEX-MOD)                  
061900     .                                                                    
062000     EJECT                                                                
062100                                                                          
062200 HBB-FIXA-BLADDRINGS-VARDEN SECTION.                                      
062300                                                                          
062400     IF OBKR-SEGMENT-FINNS                                                
062500        MOVE OBKR-IDARTNR      TO MOD-IDARTNR-NEXT                        
062600        MOVE OBKR-IDLOPNR      TO MOD-IDLOPNR-NEXT                        
062700        MOVE OBKR-IDSEKVNR     TO MOD-IDSEKVNR-NEXT                       
062800        MOVE OBKR-IDDC         TO MOD-IDDC-NEXT                           
062900        MOVE OBKR-KDORDBEK     TO MOD-KDORDBEK-NEXT                       
063000                                                                          
063100        MOVE MED-FLER-SIDOR    TO MED-IDMFSINF                            
063200     ELSE                                                                 
063300        MOVE ZERO              TO MOD-IDARTNR-NEXT                        
063400                                  MOD-IDLOPNR-NEXT                        
063500                                  MOD-IDSEKVNR-NEXT                       
063600                                  MOD-IDDC-NEXT                           
063700                                  MOD-KDORDBEK-NEXT                       
063800     END-IF                                                               
063900     .                                                                    
064000     EJECT                                                                
064100                                                                          
064200 HD-UPPDATERA-OBEH-RADER SECTION.                                         
064300                                                                          
064400     MOVE MID-RAD(1)           TO WS-AKTUELL-MID-RAD                      
064500     MOVE WS-AKT-IDARTNR       TO W-Q1-IDARTNR-MAX                        
064600     MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-MAX                        
064700     MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-MAX                       
064800     MOVE WS-AKT-IDDC          TO W-Q1-IDDC-MAX                           
064900     MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-MAX                       
065000                                                                          
065100     PERFORM IMS-01-GHU-ORQM-WDQ101-FOERE                                 
065200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
065300                                                                          
065400                                                                          
065500          MOVE JA       TO OBKR-FLOBOK                                    
065600          PERFORM IMS-06-REPL-ORQM-WDQ101                                 
065700                                                                          
065800        PERFORM IMS-02-GHN-ORQM-WDQ101-FOERE                              
065900     END-PERFORM                                                          
066000                                                                          
066100     MOVE ALL '9'              TO W-Q1-IDARTNR-MAX                        
066200                                  W-Q1-IDLOPNR-MAX                        
066300                                  W-Q1-IDSEKVNR-MAX                       
066400                                  W-Q1-IDDC-MAX                           
066500                                  W-Q1-KDORDBEK-MAX                       
066600     .                                                                    
066700     EJECT                                                                
066800 HE-UPPDATERA-AKT-SIDA SECTION.                                           
066900                                                                          
067000     MOVE +1 TO WS-INDEX-MID                                              
067100     PERFORM UNTIL WS-INDEX-MID > WS-INDEX-MID-MAX  OR                    
067200                   MID-KDORDBEK(WS-INDEX-MID) = ZERO                      
067300       MOVE MID-RAD(WS-INDEX-MID)                                         
067400                             TO WS-AKTUELL-MID-RAD                        
067500       MOVE WS-AKT-IDARTNR       TO W-Q1-IDARTNR-UNIK                     
067600       MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-UNIK                     
067700       MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-UNIK                    
067800       MOVE WS-AKT-IDDC          TO W-Q1-IDDC-UNIK                        
067900       MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-UNIK                    
068000                                                                          
068100       PERFORM IMS-05-GHU-ORQM-WDQ101-UNIK                                
068200       IF SEGMENT-FINNS                                                   
068300          MOVE JA       TO OBKR-FLOBOK                                    
068400          PERFORM IMS-06-REPL-ORQM-WDQ101                                 
068500       END-IF                                                             
068600       ADD +1                  TO WS-INDEX-MID                            
068700     END-PERFORM                                                          
068800     .                                                                    
068900     EJECT                                                                
069000                                                                          
069100 HG-UPPDATERA-RESTERANDE-RADER SECTION.                                   
069200                                                                          
069300      PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                              
069400                                                                          
069500      PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                          
069600                                                                          
069700         MOVE JA          TO OBKR-FLOBOK                                  
069800         PERFORM IMS-06-REPL-ORQM-WDQ101                                  
069900                                                                          
070000         PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                           
070100      END-PERFORM                                                         
070200     .                                                                    
070300     EJECT                                                                
070400                                                                          
070500 HH-ANNULLERA-ORDER SECTION.                                              
070600                                                                          
070700     MOVE MFS-KDMFSFOR         TO 4292-SPRAK                              
070800     MOVE OHUV-IDORDER         TO 4292-IDORDER                            
070900     MOVE WS-IDDISTR           TO 4292-IDDISTR                            
071000     MOVE WS-IDKUNDNR          TO 4292-IDKUNDNR                           
071100     MOVE W-IDORDNR            TO 4292-IDKUNDRF                           
071200                                                                          
071300     PERFORM IMS-INSERT-4292-MSG                                          
071400                                                                          
071500     MOVE 'W4O23101'           TO MFS-IDMOD                               
071600                                                                          
071700     MOVE '4231'               TO MOD-W4O23301(1:4)                       
071800                                                                          
071900     MOVE MED-ORDER-ANNULLERAD TO MED-IDMFSFEL                            
072000     CALL WMEDKONV USING MED-WMEDAREA                                     
072100     MOVE MED-MFSFEL           TO MOD-W4O23301(5:40)                      
072200                                                                          
072300     MOVE 4231-MOD-LAENGD      TO MSG-KVLL                                
072400     PERFORM IMS-INSERT-MSG                                               
072500     MOVE JA                   TO HOPP                                    
072600     .                                                                    
072700     EJECT                                                                
072800                                                                          
072900 I-STARTA-ORDERAVSLUT SECTION.                                            
073000                                                                          
073100     MOVE W-IDDISTR            TO 4298-MID-IDDISTR                        
073200     MOVE W-IDKUNDNR           TO 4298-MID-IDKUNDNR                       
073300     MOVE W-IDKUNDRF           TO 4298-MID-IDKUNDRF                       
073400     MOVE OHUV-IDORDER         TO 4298-MID-IDORDER                        
073500                                                                          
073600     COMPUTE 4298-LL = LENGTH OF 4298-MID-W4I29801 + 17                   
073700     PERFORM IMS-INSERT-4298-MSG                                          
073800     .                                                                    
073900     EJECT                                                                
074000                                                                          
074100 M-HOPPA-TILL-NY-BILD SECTION.                                            
074200                                                                          
074300     IF OHUV-FLVORKO = NEJ                                                
074400       MOVE 'W4O23101'           TO MFS-IDMOD                             
074500       MOVE '4231'               TO MOD-W4O23301(1:4)                     
074600                                                                          
074700       MOVE ERR-ORDER-AVSLUTAD   TO MED-IDMFSFEL                          
074800       CALL WMEDKONV USING MED-WMEDAREA                                   
074900       MOVE MED-MFSFEL           TO MOD-W4O23301(5:40)                    
075000       PERFORM MFS-RENSA-4231-MOD                                         
075100       MOVE WS-IDORDNR           TO WS-IDKUNDRF                           
075200       INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE                
075300       IF ENGLISH-TEXT                                                    
075400          MOVE 'ORDER NUMBER WAS' TO WS-4231-ORDERNR-TEXT                 
075500       ELSE                                                               
075600          MOVE 'ORDERNUMRET VAR ' TO WS-4231-ORDERNR-TEXT                 
075700       END-IF                                                             
075800       MOVE ORDERNR-TILL-4231    TO MOD-W4O23301(405:55)                  
075900       COMPUTE 4231-MOD-LAENGD = LENGTH OF 4231-MOD-W4O23101 + 4          
076000       MOVE 4231-MOD-LAENGD      TO MSG-KVLL                              
076100       PERFORM IMS-INSERT-MSG                                             
076200     ELSE                                                                 
076300       IF  OHUV-FLVORKO = JA                                              
076400         MOVE MFS-KDMFSFOR       TO 4225-SPRAK                            
076500         MOVE MFS-RENSA-FAELT    TO 4225-MID                              
076600         MOVE WS-IDDISTR         TO 4225-MID-IDDISTR-IN                   
076700         MOVE WS-IDORDNR         TO 4225-MID-IDORDNR-VOR                  
076800         PERFORM IMS-INSERT-4225-MSG                                      
076900       ELSE                                                               
077000         MOVE MFS-KDMFSFOR       TO 4224-SPRAK                            
077100         MOVE MFS-RENSA-FAELT    TO 4224-MID                              
077200         MOVE WS-IDDISTR         TO 4224-MID-IDDISTR-1-IN                 
077300         MOVE WS-IDORDNR         TO 4224-MID-IDORDNR-VOR                  
077400         PERFORM IMS-INSERT-4224-MSG                                      
077500       END-IF                                                             
077600     END-IF                                                               
077700     MOVE JA                   TO HOPP                                    
077800     .                                                                    
077900     EJECT                                                                
078000                                                                          
078100 Z-FINIT SECTION.                                                         
078200                                                                          
078300     IF MED-IDMFSFEL NOT = SPACE OR MED-IDMFSINF NOT = SPACE              
078400         CALL WMEDKONV USING MED-WMEDAREA                                 
078500         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
078600         MOVE MED-MFSINF       TO MOD-TEMFSINF                            
078700     END-IF                                                               
078800                                                                          
078900     IF NOT ALLT-OK AND NYCKEL-OK                                         
079000        PERFORM MFS-ROER-EJ-BILD                                          
079100     END-IF                                                               
079200                                                                          
079300     COMPUTE MSG-KVLL = LENGTH OF MOD-W4O23301 + 4                        
079400     PERFORM IMS-INSERT-MSG                                               
079500     .                                                                    
079600     EJECT                                                                
079700                                                                          
079800 MFS-ROER-EJ-BILD SECTION.                                                
079900                                                                          
080000     MOVE MFS-ROER-EJ-FAELT    TO MOD-FLANNULL                            
080100                                  MOD-KDORDKL-UT                          
080200                                                                          
080300     MOVE +1                   TO WS-INDEX                                
080400     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
080500                                                                          
080600        MOVE MFS-ROER-EJ-FAELT TO MOD-KDORDBEK(WS-INDEX)                  
080700                                  MOD-KDBEHX(WS-INDEX)                    
080800                                  MOD-IDARTNR(WS-INDEX)                   
080900                                  MOD-BEART(WS-INDEX)                     
081000                                  MOD-IDDC-RAD(WS-INDEX)                  
081100                                  MOD-KVANTAL(WS-INDEX)                   
081200                                  MOD-KEYS(WS-INDEX)                      
081300        ADD +1                 TO WS-INDEX                                
081400     END-PERFORM                                                          
081500     .                                                                    
081600     EJECT                                                                
081700                                                                          
081800 MFS-RENSA-MOD-RADER SECTION.                                             
081900                                                                          
082000     MOVE +1                 TO WS-INDEX                                  
082100     PERFORM UNTIL WS-INDEX > WS-INDEX-MOD-MAX                            
082200        MOVE MFS-RENSA-FAELT TO MOD-KDORDBEK(WS-INDEX)                    
082300                                MOD-KDBEHX(WS-INDEX)                      
082400                                MOD-IDARTNR(WS-INDEX)                     
082500                                MOD-BEART(WS-INDEX)                       
082600                                MOD-IDDC-RAD(WS-INDEX)                    
082700                                MOD-KVANTAL(WS-INDEX)                     
082800                                MOD-KEYS(WS-INDEX)                        
082900        ADD 1                TO WS-INDEX                                  
083000     END-PERFORM                                                          
083100     .                                                                    
083200                                                                          
083300 MFS-RENSA-ALLA-FAELT SECTION.                                            
083400                                                                          
083500     MOVE MFS-RENSA-FAELT    TO MOD-FLANNULL                              
083600                                                                          
083700     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-NEXT                          
083800                                MOD-IDLOPNR-NEXT                          
083900                                MOD-IDSEKVNR-NEXT                         
084000                                MOD-IDDC-NEXT                             
084100                                MOD-KDORDBEK-NEXT                         
084200                                                                          
084300     PERFORM MFS-RENSA-MOD-RADER                                          
084400     .                                                                    
084500     EJECT                                                                
084600 MFS-RENSA-4231-MOD SECTION.                                              
084700                                                                          
084800     MOVE MFS-RENSA-FAELT    TO 4231-MOD-TEDDI                            
084900                                4231-MOD-IDDISTR                          
085000                                4231-MOD-IDKUNDNR                         
085100                                4231-MOD-IDORDNR                          
085200                                4231-MOD-KDORDKL                          
085300                                4231-MOD-KDFRAKT                          
085400                                4231-MOD-IDDC-TVS                         
085500                                4231-MOD-KDFAKTYP                         
085600                                4231-MOD-BEKUNDRF                         
085700                                4231-MOD-FLLSBOK                          
085800                                4231-MOD-FLAUTPAC                         
085900                                4231-MOD-FLAUTFAK                         
086000                                4231-MOD-BEGMT                            
086100                                4231-MOD-ADGMT                            
086200                                4231-MOD-BEGMRK                           
086300                                4231-MOD-BEGMRK-RAD1                      
086400                                4231-MOD-IDFTG                            
086500                                4231-MOD-IDKONTO                          
086600                                4231-MOD-IDKST                            
086610                                4231-MOD-IDANALYS                         
086700                                                                          
086800                                                                          
086900     MOVE MFS-FORMATETS-ATTR TO 4231-MOD-IDDISTR-ATTR                     
087000                                4231-MOD-IDKUNDNR-ATTR                    
087100                                4231-MOD-IDORDNR-ATTR                     
087200                                4231-MOD-KDORDKL-ATTR                     
087300                                4231-MOD-KDFRAKT-ATTR                     
087400                                4231-MOD-IDDC-TVS-ATTR                    
087500                                4231-MOD-KDFAKTYP-ATTR                    
087600                                4231-MOD-FLLSBOK-ATTR                     
087700                                4231-MOD-FLAUTPAC-ATTR                    
087800                                4231-MOD-FLAUTFAK-ATTR                    
087900                                4231-MOD-IDFTG-ATTR                       
088000                                4231-MOD-IDKONTO-ATTR                     
088100                                4231-MOD-IDKST-ATTR                       
088110                                4231-MOD-IDANALYS-ATTR                    
088200     .                                                                    
088300     EJECT                                                                
088400 IMS-GET-MSG SECTION.                                                     
088500                                                                          
088600     MOVE '  QC' TO GODK-STATUSKODER                                      
088700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
088800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088900     PERFORM IMS-STATUSKONTROLL                                           
089000     .                                                                    
089100     SKIP3                                                                
089200 IMS-INSERT-MSG SECTION.                                                  
089300                                                                          
089400     IF ENGLISH-TEXT                                                      
089500       MOVE 'N' TO MFS-KDHUVOMR                                           
089600     END-IF                                                               
089700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
089800     MOVE SPACE TO GODK-STATUSKODER                                       
089900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
090000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
090100     PERFORM IMS-STATUSKONTROLL                                           
090200     .                                                                    
090300     EJECT                                                                
090400 IMS-INSERT-4292-MSG SECTION.                                             
090500                                                                          
090600     IF ENGLISH-TEXT                                                      
090700       MOVE 'N' TO MFS-KDHUVOMR                                           
090800     END-IF                                                               
090900     MOVE LOW-VALUE TO 4292-Z1 4292-Z2                                    
091000     MOVE SPACE TO GODK-STATUSKODER                                       
091100     CALL CBLTDLI USING ISRT 4292-PCB 4292-MSG-IO-AREA                    
091200     MOVE 4292-STATUS-CODE TO STATUS-WS                                   
091300     PERFORM IMS-STATUSKONTROLL                                           
091400     .                                                                    
091500     SKIP2                                                                
091600 IMS-INSERT-4298-MSG SECTION.                                             
091700                                                                          
091800     IF ENGLISH-TEXT                                                      
091900       MOVE 'N' TO MFS-KDHUVOMR                                           
092000     END-IF                                                               
092100     MOVE LOW-VALUE TO 4298-Z1 4298-Z2                                    
092200     MOVE SPACE TO GODK-STATUSKODER                                       
092300     CALL CBLTDLI USING ISRT 4298-PCB 4298-MSG-IO-AREA                    
092400     MOVE 4298-STATUS-CODE TO STATUS-WS                                   
092500     PERFORM IMS-STATUSKONTROLL                                           
092600     .                                                                    
092700     EJECT                                                                
092800 IMS-INSERT-4224-MSG SECTION.                                             
092900                                                                          
093000     MOVE SPACE TO GODK-STATUSKODER                                       
093100     CALL CBLTDLI USING ISRT 4224-PCB 4224-MSG-IO-AREA                    
093200     MOVE 4224-STATUS-CODE TO STATUS-WS                                   
093300     PERFORM IMS-STATUSKONTROLL                                           
093400     .                                                                    
093500     EJECT                                                                
093600 IMS-INSERT-4225-MSG SECTION.                                             
093700                                                                          
093800     MOVE SPACE TO GODK-STATUSKODER                                       
093900     CALL CBLTDLI USING ISRT 4225-PCB 4225-MSG-IO-AREA                    
094000     MOVE 4225-STATUS-CODE TO STATUS-WS                                   
094100     PERFORM IMS-STATUSKONTROLL                                           
094200     .                                                                    
094300     EJECT                                                                
094400 IMS-01-GHU-ORQM-WDQ101-FOERE SECTION.                                    
094500                                                                          
094600     STRING 'WLORQM01(WDQ101KY=>' W-WDQ101-KEY-MIN                        
094700                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
094800                    '&FLOBOK   =' NEJ ')'                                 
094900          DELIMITED BY SIZE INTO SSA1                                     
095000     MOVE '  GE'               TO GODK-STATUSKODER                        
095100     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
095200     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
095300     PERFORM IMS-STATUSKONTROLL                                           
095400     .                                                                    
095500     SKIP2                                                                
095600 IMS-02-GHN-ORQM-WDQ101-FOERE SECTION.                                    
095700                                                                          
095800     STRING 'WLORQM01(WDQ101KY <' W-WDQ101-KEY-MIN                        
095900                    '&WDQ101KY <' W-WDQ101-KEY-MAX                        
096000                    '&FLOBOK   =' NEJ ')'                                 
096100          DELIMITED BY SIZE INTO SSA1                                     
096200     MOVE '  GEGB'             TO GODK-STATUSKODER                        
096300     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
096400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
096500     PERFORM IMS-STATUSKONTROLL                                           
096600     .                                                                    
096700     SKIP2                                                                
096800 IMS-03-GHU-ORQM-WDQ101-MIN-MAX SECTION.                                  
096900                                                                          
097000     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
097100                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
097200                    '&FLOBOK   =' NEJ ')'                                 
097300          DELIMITED BY SIZE INTO SSA1                                     
097400     MOVE '  GE'               TO GODK-STATUSKODER                        
097500     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
097600     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
097700     PERFORM IMS-STATUSKONTROLL                                           
097800     .                                                                    
097900     EJECT                                                                
098000 IMS-04-GHN-ORQM-WDQ101-MIN-MAX SECTION.                                  
098100                                                                          
098200     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
098300                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
098400                    '&FLOBOK   =' NEJ ')'                                 
098500          DELIMITED BY SIZE INTO SSA1                                     
098600     MOVE '  GEGB'             TO GODK-STATUSKODER                        
098700     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
098800     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
098900     PERFORM IMS-STATUSKONTROLL                                           
099000     .                                                                    
099100     SKIP2                                                                
099200 IMS-05-GHU-ORQM-WDQ101-UNIK SECTION.                                     
099300                                                                          
099400     STRING 'WLORQM01(WDQ101KY =' W-WDQ101-KEY-UNIK                       
099500                    '&FLOBOK   =' NEJ ')'                                 
099600          DELIMITED BY SIZE  INTO SSA1                                    
099700     MOVE '  GE'               TO GODK-STATUSKODER                        
099800     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
099900     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
100000     PERFORM IMS-STATUSKONTROLL                                           
100100     .                                                                    
100200     SKIP2                                                                
100300 IMS-06-REPL-ORQM-WDQ101 SECTION.                                         
100400                                                                          
100500     MOVE '    '               TO GODK-STATUSKODER                        
100600     CALL CBLTDLI USING REPL ORQM-PCB DLI-IO-AREA-ORQM                    
100700     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
100800     PERFORM IMS-STATUSKONTROLL                                           
100900     .                                                                    
101000     EJECT                                                                
101100 IMS-07-GU-ORQI-WDQ201 SECTION.                                           
101200                                                                          
101300     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
101400          DELIMITED BY SIZE INTO SSA1                                     
101500     MOVE '  GE'               TO GODK-STATUSKODER                        
101600     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-ORQI01 SSA1               
101700     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
101800     PERFORM IMS-STATUSKONTROLL                                           
101900     .                                                                    
102000     SKIP2                                                                
102100 IMS-09-GU-BENA-WDD311 SECTION.                                           
102200                                                                          
102300     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
102400          DELIMITED BY SIZE INTO SSA1                                     
102500     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
102600          DELIMITED BY SIZE INTO SSA2                                     
102700     MOVE '  GE'               TO GODK-STATUSKODER                        
102800     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA SSA1 SSA2            
102900     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
103000     PERFORM IMS-STATUSKONTROLL                                           
103100     .                                                                    
103200     SKIP2                                                                
103300 IMS-STATUSKONTROLL SECTION.                                              
103400                                                                          
103500     SET STATUS-IX TO 1                                                   
103600     SEARCH GODK-STATUS                                                   
103700       AT END CALL FELLOG                                                 
103800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
103900     END-SEARCH                                                           
104000     .                                                                    

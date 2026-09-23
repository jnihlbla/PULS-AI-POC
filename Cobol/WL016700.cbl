000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     WL016700.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   APRIL 2005.                                              
000600*                                                                         
000700     REMARKS.                                                             
000800* WL016700 PROGRAM IS A REPLICA OF W4023300 PROGRAM                       
000900* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001000*                                                                         
001100*    NAMN:       CARPARTS.LDC.SPECORDERCONF                               
001200*                                                                         
001300*    FUNKTION.                                                            
001400*        PROGRAMMET HANTERAR SVARSBILD TILL                               
001500*        SPECIALORDERREGISTRERING                                         
001600*        VISAR AVVIKELSER (KOD 53, 58 & 59)                               
001700*        UPPDATERING AV GODKÄNDA RADER. FLOBOK BLIR = 'J'.                
001800*        ANNULLATION AV HEL ORDER MÖJLIG.                                 
001900*                                                                         
002000*        EFTER AVSLUTAD BEHANDLING SKER UTHOPP TILL ORDERHUVUD            
002100*        0165.                                                            
002200*                                                                         
002300*        PROGRAMMET ÄR ETT UPPDATERINGS-MPP                               
002400*        PROGRAMMET LÄSER      WLORQM (WDQ1)  ORDERBEKR.BAS               
002500*        PROGRAMMET LÄSER      WLORQI (WDQ2)  ORDERHUVUD SEK-IX           
002600*        PROGRAMMET LÄSER      WLBENA (WDD3)  BENÄMNINGSREGISTER          
002700*                                                                         
002800*    INDATA.                                                              
002900*        RESP:        WL0167I1                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        REQU:        WL0167O1                                            
003300*        REQU:        WL0165O1                                            
003400     EJECT                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600                                                                          
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(08)   VALUE 'WL016700'.            
004200                                                                          
004300 77  JA                          PIC X(1)   VALUE 'J'.                    
004400 77  NEJ                         PIC X(1)   VALUE 'N'.                    
004500 77  HOPP                        PIC X(1)   VALUE 'N'.                    
004600                                                                          
004700 77  WS-IDSKYLT-CN               PIC X(3)   VALUE 'RCN'.                  
004800 77  WS-IDSKYLT-GB               PIC X(3)   VALUE 'GB '.                  
004900 77  WS-CP-UTF8                  PIC X(4)   VALUE 'UTF8'.                 
005000 77  WS-CP-EBCDIC                PIC X(3)   VALUE '278'.                  
005100                                                                          
005200 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
005400 77  FILLER                      PIC X(08)   VALUE 'AAAAAAAA'.            
005500 77  ERROR-TEXT                  PIC X(32) VALUE SPACE.                   
005600 77  FILLER                      PIC X(08)   VALUE 'AAAABBBB'.            
005700 77  FELTEXT                     PIC X(32) VALUE SPACE.                   
005800 77  PGM-POS                     PIC X(32) VALUE SPACE.                   
005900 77  KDRC-DISPLAY                PIC Z(5).                                
006000 77  KDRC-DISP                   PIC 9(4)   VALUE ZERO.                   
006100                                                                          
006200 77  WS-INDEX                    PIC S9(9)  COMP-3    VALUE ZERO.         
006300 77  WS-INDEX-REQU               PIC S9(9)  COMP-3    VALUE ZERO.         
006400 77  WS-INDEX-REQU-MAX           PIC S9(9)  COMP-3    VALUE +500.         
006500                                                                          
006600 77  WS-INDEX-RESP-MAX           PIC S9(9)  COMP-3    VALUE +500.         
006700 77  WS-INDEX-RESP               PIC S9(9)  COMP-3    VALUE ZERO.         
006800 77  WS-KVRADER                  PIC  9(5)  COMP-3    VALUE ZERO.         
006900 77  WS-IDDISTR                  PIC X(4).                                
007000 77  WS-IDKUNDNR                 PIC X(6).                                
007100 77  WS-IDORDNR                  PIC X(5).                                
007200 01  ORDERNR-TILL-0165.                                                   
007300    03  FILLER                   PIC X(15).                               
007400    03  WS-0165-ORDERNR-TEXT     PIC X(17)  VALUE SPACE.                  
007500    03  WS-IDKUNDRF              PIC X(5).                                
007600     EJECT                                                                
007700                                                                          
007800 77  FILLER                      PIC X(08)   VALUE 'BBBBBBBB'.            
007900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
008000     88  ALLT-OK                             VALUE 'J'.                   
008100 77  NYCKEL-SW                   PIC X       VALUE 'J'.                   
008200     88  NYCKEL-OK                           VALUE 'J'.                   
008300 77  AVSLUTA-SW                  PIC X       VALUE 'N'.                   
008400     88  AVSLUTA                             VALUE 'J'.                   
008500                                                                          
008600 77  OBKR-RAD-FINNS-SW           PIC X       VALUE 'N'.                   
008700     88  OBKR-RAD-FINNS                      VALUE 'J'.                   
008800                                                                          
008900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009000     88  EGEN-MID                            VALUE '4233'.                
009100     88  GODK-MID                            VALUE '4231' '4232'          
009200                                                   '4233'.                
009300                                                                          
009400 01  WS-AKTUELL-REQU-RAD.                                                 
009500     03  WS-AKT-KDORDBEK         PIC 9(2).                                
009600     03  WS-AKT-IDARTNR          PIC 9(9).                                
009700     03  WS-AKT-FILLER           PIC X(1).                                
009800     03  WS-AKT-REKSIFFR         PIC 9(1).                                
009900     03  WS-AKT-IDDC             PIC X(2).                                
010000     03  WS-AKT-KEYS.                                                     
010100         05 WS-AKT-IDLOPNR       PIC 9(3).                                
010200         05 WS-AKT-IDSEKVNR      PIC 9(3).                                
010300                                                                          
010400 77  FILLER                      PIC X(08)   VALUE 'CCCCCCCC'.            
010500     EJECT                                                                
010600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010700 01  GENERELLA-SUBPROGRAM.                                                
010800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
011000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011300     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
011400 01  GEMENSAMMA-PROGRAM.                                                  
011500     03  W335PRQU                PIC X(8)    VALUE 'W335PRQU'.            
011600*        PRISFRÅGA                                                        
011700 01 FILLER                       PIC X(8) VALUE 'W335PRQU'.               
011800*   -COPY W335PRQU                                                        
011900     SKIP2                                                                
012000*    --- PARAMETERS TO ABEND                                              
012100                                                                          
012200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012500     SKIP3                                                                
012600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012700 01  MESSAGE-CODES.                                                       
012800     03  ORDER-NUMBER-WAS         PIC X(3)    VALUE '302'.                
012900     03  MED-EJ-FLER-RADER        PIC X(3)    VALUE '056'.                
013000     03  MED-ORDER-ANNULLERAD     PIC X(3)    VALUE '301'.                
013100     03  ERR-WRONG-KEY            PIC X(3)    VALUE '043'.                
013200     03  ERR-OBEHORIG             PIC X(3)    VALUE '405'.                
013300     03  ERR-ORDER-SAKNAS         PIC X(3)    VALUE '304'.                
013400     03  ERR-ORDER-AVSLUTAD       PIC X(3)    VALUE '299'.                
013500     03  ERR-FEL-BILDSERIE        PIC X(3)    VALUE '300'.                
013600     03  ERR-UPPLYSTA-FEL         PIC X(3)    VALUE '023'.                
013700     03  PRESS-EXECUTE-TO-APPROVE PIC X(3)    VALUE '308'.                
013800     EJECT                                                                
013900*   -COPY WMEDAREA                                                        
014000     EJECT                                                                
014100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014200 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
014300     SKIP3                                                                
014400*01  -COPY WZ01SUB                                                        
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
014700*01  -COPY WZ01SEND                                                       
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
015000     SKIP3                                                                
015100 01  REQU-AREA.                                                           
015200*    03  -COPY WZ01REQU                                                   
015300*    03  -COPY WL0167I1                                                   
015400     EJECT                                                                
015500 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
015600     SKIP3                                                                
015700 01  RESP-AREA.                                                           
015800*    03  -COPY WZ01RESP                                                   
015900*    03  -COPY WL0167O1                                                   
016000     EJECT                                                                
016100*01  -COPY WWDC99                                                         
016200     SKIP3                                                                
016300*01  -COPY WTRAUTF8                                                       
016400     EJECT                                                                
016500***********************************************************               
016600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016800                                                                          
016900 01  NYCKLAR-TILL-DLI.                                                    
017000     03  W-WDQ101-KEY-UNIK.                                               
017100         05  W-Q1-IDORDER-UNIK   PIC S9(7)   VALUE ZERO COMP-3.           
017200         05  W-Q1-IDARTNR-UNIK   PIC S9(9)   VALUE ZERO COMP-3.           
017300         05  W-Q1-IDLOPNR-UNIK   PIC S9(3)   VALUE ZERO COMP-3.           
017400         05  W-Q1-IDSEKVNR-UNIK  PIC S9(3)   VALUE ZERO COMP-3.           
017500         05  W-Q1-IDDC-UNIK      PIC  X(2)   VALUE ZERO.                  
017600         05  W-Q1-KDORDBEK-UNIK  PIC 9(2)    VALUE ZERO.                  
017700                                                                          
017800     03  W-WDQ101-KEY-MIN.                                                
017900         05  W-Q1-IDORDER-MIN    PIC S9(7)   VALUE ZERO COMP-3.           
018000         05  W-Q1-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
018100         05  W-Q1-IDLOPNR-MIN    PIC S9(3)   VALUE ZERO COMP-3.           
018200         05  W-Q1-IDSEKVNR-MIN   PIC S9(3)   VALUE ZERO COMP-3.           
018300         05  W-Q1-IDDC-MIN       PIC  X(2)   VALUE ZERO.                  
018400         05  W-Q1-KDORDBEK-MIN   PIC 9(2)    VALUE ZERO.                  
018500                                                                          
018600     03  W-WDQ101-KEY-MAX.                                                
018700         05  W-Q1-IDORDER-MAX    PIC S9(7) VALUE 9999999 COMP-3.          
018800         05  W-Q1-IDARTNR-MAX    PIC S9(9) VALUE 999999999 COMP-3.        
018900         05  W-Q1-IDLOPNR-MAX    PIC S9(3) VALUE 999  COMP-3.             
019000         05  W-Q1-IDSEKVNR-MAX   PIC S9(3) VALUE 999  COMP-3.             
019100         05  W-Q1-IDDC-MAX       PIC  X(2) VALUE '99'.                    
019200         05  W-Q1-KDORDBEK-MAX   PIC 9(2)  VALUE 99.                      
019300     EJECT                                                                
019400                                                                          
019500     03  W-IDGMTREF-X.                                                    
019600         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
019700         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
019800         05  W-IDKUNDRF.                                                  
019900           07  W-IDORDNR         PIC 9(7)    VALUE ZERO.                  
020000           07  FILLER            PIC X(3)    VALUE SPACE.                 
020100                                                                          
020200     03  W-IDARTNR-X.                                                     
020300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
020400     03  W-IDSKYLT-X.                                                     
020500         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
           03  W-IDDC-B6-X.                                                     
               05  W-IDDC-B6            PIC X(2).                               
020600     EJECT                                                                
020700*    --- STATUS-KOD FRÅN IMS                                              
020800 01  STATUS-WS                   PIC XX.                                  
020900     88  SEGMENT-FINNS                       VALUE '  '.                  
021000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021100     88  BASEN-SLUT                          VALUE 'GB'.                  
021200     SKIP2                                                                
021300 77  STATUS-OBKR-WS              PIC X(2)    VALUE 'GE'.                  
021400     88  OBKR-SEGMENT-FINNS                  VALUE '  '.                  
021500     SKIP2                                                                
021600 01  GODK-STATUSKODER.                                                    
021700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021800     SKIP3                                                                
021900 01  SSA1                        PIC X(160).                              
022000 01  SSA2                        PIC X(96).                               
022100                                                                          
022200     EJECT                                                                
022300*    --- IMS FUNKTIONSKODER                                               
022400*01  -COPY W0003                                                          
022500     EJECT                                                                
022600*    ---  DLI INPUT-OUTPUT AREA                                           
022700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
022800     SKIP3                                                                
022900 01  FILLER                      PIC X(16)   VALUE 'WDQ101-AREA'.         
023000 01  DLI-IO-AREA-ORQM.                                                    
023100     03  WLORQM01.                                                        
023200*        05  -COPY WDQ101                                                 
023300     EJECT                                                                
023400 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
023500 01  DLI-IO-AREA-ORQI01.                                                  
023600     03  WLORQI01.                                                        
023700*        05  -COPY WDQ201                                                 
023800     EJECT                                                                
023900 01  FILLER                      PIC X(16)   VALUE 'WDD311-AREA'.         
024000 01  DLI-IO-AREA-BENA.                                                    
024100     03  WLBENA11.                                                        
024200*        05  -COPY WDD311                                                 
024300     EJECT                                                                
       01  FILLER               PIC X(16)   VALUE 'WDB601  '.                   
       01  DLI-IO-WDB601.                                                       
      *    03  -COPY WDB601                                                     
           EJECT                                                                
024400                                                                          
024500*--MSG-AREOR FÖR HOPP TILL 4292-ORDERANNULLATION                          
024600*------------------------- 4298-ORDERAVSLUT                               
024700                                                                          
024800 01  FILLER                  PIC X(16)  VALUE '4292-MSG-IO-AREA'.         
024900 01  4292-MSG-IO-AREA.                                                    
025000     03  4292-LL               PIC S9(4)  VALUE +47  COMP SYNC.           
025100     03  4292-Z1               PIC X.                                     
025200     03  4292-Z2               PIC X.                                     
025300     03  4292-TRANSKOD         PIC X(8)   VALUE 'W4T292X '.               
025400     03  4292-IDTRANS          PIC X(4)   VALUE '4233'.                   
025500     03  4292-SPRAK            PIC X.                                     
025600     03  4292-IDORDER          PIC X(7).                                  
025700     03  4292-IDDISTR          PIC X(4).                                  
025800     03  4292-IDKUNDNR         PIC X(6).                                  
025900     03  4292-IDKUNDRF         PIC X(7).                                  
026000     03  FILLER                PIC X(6)   VALUE SPACE.                    
026100     EJECT                                                                
026200                                                                          
026300 01  FILLER                  PIC X(16)  VALUE '4298-MSG-IO-AREA'.         
026400 01  4298-MSG-IO-AREA.                                                    
026500     03  4298-LL               PIC S9(4)  VALUE +0 COMP SYNC.             
026600     03  4298-Z1               PIC X.                                     
026700     03  4298-Z2               PIC X.                                     
026800     03  4298-TRANSKOD         PIC X(8)   VALUE 'W4T298X '.               
026900     03  4298-IDTRANS          PIC X(4)   VALUE '4233'.                   
027000     03  4298-SPRAK            PIC X.                                     
027100     03  -COPY W4I29801  -PRE 4298-                                       
027200     EJECT                                                                
027300 LINKAGE SECTION.                                                         
027400*01  -COPY W0009   -PRE MSG-                                              
027500*01  -COPY W0009   -PRE 4292-                                             
027600     EJECT                                                                
027700*01  -COPY W0009   -PRE 4298-                                             
027800*01  -COPY W0008   -PRE BENA-                                             
027900     05  FILLER                  PIC X.                                   
028000     EJECT                                                                
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
028100*01  -COPY W0008   -PRE ORQI-                                             
028200     05  FILLER                  PIC X.                                   
028300*01  -COPY W0008   -PRE ORQM-                                             
028400     05  FILLER                  PIC X.                                   
028500     EJECT                                                                
028600 01  PRQU-WDG2-PCB               PIC X.                                   
028700 01  PRQU-WDC7-PCB               PIC X.                                   
028800 01  PRQU-SJKO-WDK6-PCB          PIC X.                                   
028900                                                                          
029000 PROCEDURE DIVISION  USING MSG-PCB 4292-PCB 4298-PCB                      
029100                          BENA-PCB WDB6-PCB ORQI-PCB ORQM-PCB             
029200                          PRQU-WDG2-PCB                                   
029300                          PRQU-WDC7-PCB                                   
029400                          PRQU-SJKO-WDK6-PCB.                             
029500                                                                          
029600     ENTRY 'DLITCBL' USING MSG-PCB 4292-PCB 4298-PCB                      
029700                          BENA-PCB WDB6-PCB ORQI-PCB ORQM-PCB             
029800                          PRQU-WDG2-PCB                                   
029900                          PRQU-WDC7-PCB                                   
030000                          PRQU-SJKO-WDK6-PCB.                             
030100                                                                          
030200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
030300     IF SUB-KDRC = 0                                                      
030400                                                                          
030500        PERFORM A-INIT                                                    
030600        PERFORM B-KOLLA-NYCKLAR                                           
030700        IF NYCKEL-OK                                                      
030800           IF ALLT-OK                                                     
030900              PERFORM F-KOLLA-ATT-ORDER-FINNS                             
031000              IF ALLT-OK                                                  
031100                 IF REQU-KDPGMACT = 'E'                                   
031200                 OR REQU-KDPGMACT = 'S'                                   
031300                    PERFORM G-KONTROLLERA-BILDEN                          
031400                 END-IF                                                   
031500                 IF ALLT-OK                                               
031600                    PERFORM H-BEHANDLA-RADER                              
031700                    IF AVSLUTA-SW = 'J'                                   
031800                       PERFORM I-STARTA-ORDERAVSLUT                       
031900                       PERFORM M-HOPPA-TILL-NY-BILD                       
032000                    END-IF                                                
032100                 END-IF                                                   
032200              END-IF                                                      
032300           END-IF                                                         
032400        END-IF                                                            
032500       PERFORM S02-RETURN-RESPONSE                                        
032600     END-IF                                                               
032700                                                                          
032800     MOVE +0 TO RETURN-CODE                                               
032900     GOBACK                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 A-INIT SECTION.                                                          
033300     MOVE 'STA A-INIT        ' TO PGM-POS                                 
033400                                                                          
033500     MOVE JA                   TO ALLT-SW                                 
033600                                  NYCKEL-SW                               
033700                                                                          
033800     MOVE ALL '+'              TO RESP-AREA                               
033900     MOVE 001                  TO RESP-IDMSGVER                           
034000     MOVE SPACE                TO RESP-IDMSG-ERROR                        
034100                                  RESP-IDMSG-INFO                         
034200                                  RESP-IDELMT-ERROR                       
034300                                                                          
034400                                                                          
034500     MOVE +2                   TO SPRAK-IX                                
034600     MOVE 'END A-INIT        ' TO PGM-POS                                 
034700     .                                                                    
034800     EJECT                                                                
034900 B-KOLLA-NYCKLAR SECTION.                                                 
035000     MOVE 'STA B-KOLL        ' TO PGM-POS                                 
035100                                                                          
035200     MOVE REQU-IDDC-KEY        TO RESP-IDDC-KEY                           
                                        W-IDDC-B6                               
035300                                                                          
035400     IF REQU-IDDISTR-KEY = ALL '+'                                        
035500        MOVE REQU-IDDISTR-KEY  TO WS-IDDISTR                              
035600        INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                
035700     ELSE                                                                 
035800        MOVE REQU-IDDISTR-KEY  TO WS-IDDISTR                              
035900        MOVE REQU-IDDISTR-KEY  TO RESP-IDDISTR-KEY                        
036000     END-IF                                                               
036100                                                                          
036200     IF WS-IDDISTR NUMERIC  AND  WS-IDDISTR > ZERO                        
036300        MOVE WS-IDDISTR        TO W-IDDISTR                               
036400     END-IF                                                               
036500                                                                          
036600     IF REQU-IDKUNDNR-KEY = ALL '+'                                       
036700        MOVE REQU-IDKUNDNR-KEY TO WS-IDKUNDNR                             
036800        INSPECT WS-IDKUNDNR REPLACING LEADING SPACE BY ZERO               
036900     ELSE                                                                 
037000        MOVE REQU-IDKUNDNR-KEY TO WS-IDKUNDNR                             
037100        MOVE REQU-IDKUNDNR-KEY TO RESP-IDKUNDNR-KEY                       
037200     END-IF                                                               
037300                                                                          
037400     IF WS-IDKUNDNR NUMERIC                                               
037500        MOVE WS-IDKUNDNR       TO W-IDKUNDNR                              
037600     END-IF                                                               
037700                                                                          
037800     IF REQU-IDORDNR-KEY = ALL '+'                                        
037900        MOVE REQU-IDORDNR-KEY  TO WS-IDORDNR                              
038000        INSPECT WS-IDORDNR REPLACING LEADING SPACE BY ZERO                
038100     ELSE                                                                 
038200        MOVE REQU-IDORDNR-KEY  TO WS-IDORDNR                              
038300        MOVE REQU-IDORDNR-KEY  TO RESP-IDORDNR-KEY                        
038400     END-IF                                                               
038500                                                                          
038600     IF WS-IDORDNR NUMERIC  AND WS-IDORDNR > ZERO                         
038700        MOVE WS-IDORDNR        TO W-IDORDNR                               
038800     END-IF                                                               
038900                                                                          
039000     IF NYCKEL-OK                                                         
039100        MOVE WS-IDKUNDNR          TO REQU-IDKUNDNR-KEY                    
039200        INSPECT REQU-IDKUNDNR-KEY REPLACING LEADING ZERO BY SPACE         
039300        IF WS-IDKUNDNR = ZERO                                             
039400           MOVE ZERO              TO REQU-IDKUNDNR-KEY                    
039500        END-IF                                                            
039600        MOVE WS-IDDISTR           TO REQU-IDDISTR-KEY                     
039700        INSPECT REQU-IDDISTR-KEY REPLACING LEADING ZERO BY SPACE          
039800        MOVE WS-IDORDNR           TO REQU-IDORDNR-KEY                     
039900        INSPECT REQU-IDORDNR-KEY REPLACING LEADING ZERO BY SPACE          
040000     END-IF                                                               
040100                                                                          
040200     IF NOT NYCKEL-OK                                                     
040300        MOVE ERR-WRONG-KEY     TO RESP-IDMSG-ERROR                        
040400        MOVE 'KEY'             TO RESP-IDELMT-ERROR                       
           ELSE                                                                 
              PERFORM IMS-GU-WDB601                                             
040500     END-IF                                                               
040600     MOVE 'END B-KOLL        ' TO PGM-POS                                 
040700     .                                                                    
040800     EJECT                                                                
040900                                                                          
041000                                                                          
041100 F-KOLLA-ATT-ORDER-FINNS SECTION.                                         
041200     MOVE 'STA F-KOLLA       '    TO PGM-POS                              
041300                                                                          
041400     PERFORM IMS-07-GU-ORQI-WDQ201                                        
041500     IF SEGMENT-FINNS                                                     
041600       IF OHUV-FLKLAR = JA                                                
041700         MOVE ERR-ORDER-AVSLUTAD  TO RESP-IDMSG-ERROR                     
041800         MOVE 'FLKLAR'            TO RESP-IDELMT-ERROR                    
041900         MOVE NEJ                 TO ALLT-SW                              
042000         MOVE NEJ                 TO NYCKEL-SW                            
042100       ELSE                                                               
042200         IF OHUV-IDSYSTEM NOT = '4231'                                    
042300           MOVE ERR-FEL-BILDSERIE TO RESP-IDMSG-ERROR                     
042400           MOVE 'IDSYSTEM'        TO RESP-IDELMT-ERROR                    
042500           MOVE NEJ               TO ALLT-SW                              
042600           MOVE NEJ               TO NYCKEL-SW                            
042700         ELSE                                                             
042800           MOVE OHUV-KDORDKL      TO RESP-KDORDKL-UT                      
042900           MOVE OHUV-IDORDER      TO W-Q1-IDORDER-UNIK                    
043000                                     W-Q1-IDORDER-MIN                     
043100                                     W-Q1-IDORDER-MAX                     
043200         END-IF                                                           
043300       END-IF                                                             
043400                                                                          
043500     ELSE                                                                 
043600                                                                          
043700        MOVE ERR-ORDER-SAKNAS  TO RESP-IDMSG-ERROR                        
043800        MOVE 'IDORDNR'         TO RESP-IDELMT-ERROR                       
043900        MOVE NEJ               TO ALLT-SW                                 
044000        MOVE NEJ               TO NYCKEL-SW                               
044100     END-IF                                                               
044200     MOVE 'END F-KOLLA       ' TO PGM-POS                                 
044300     .                                                                    
044400     EJECT                                                                
044500                                                                          
044600 G-KONTROLLERA-BILDEN SECTION.                                            
044700     MOVE 'STA G-KONTROLL    ' TO PGM-POS                                 
044800                                                                          
044900     MOVE JA                   TO ALLT-SW                                 
045000                                                                          
045100     IF REQU-FLANNULL NOT = '+'                                           
045200        IF REQU-FLANNULL = 'J' OR 'N'                                     
045300           MOVE JA             TO ALLT-SW                                 
045400        ELSE                                                              
045500           MOVE ERR-UPPLYSTA-FEL  TO RESP-IDMSG-ERROR                     
045600           MOVE 'FLANULL'         TO RESP-IDELMT-ERROR                    
045700           MOVE NEJ            TO ALLT-SW                                 
045800        END-IF                                                            
045900     ELSE                                                                 
046000        MOVE NEJ               TO RESP-FLANNULL                           
046100     END-IF                                                               
046200                                                                          
046300     IF  REQU-KVRADER NUMERIC                                             
046400     AND REQU-KVRADER < 501                                               
046500       MOVE REQU-KVRADER     TO WS-KVRADER                                
046600     ELSE                                                                 
046700       IF REQU-KVRADER = ALL '+'                                          
046800         MOVE ZERO             TO WS-KVRADER                              
046900       ELSE                                                               
047000         MOVE ERR-UPPLYSTA-FEL TO RESP-IDMSG-ERROR                        
047100         MOVE 'KVRADER'        TO RESP-IDELMT-ERROR                       
047200         MOVE NEJ              TO ALLT-SW                                 
047300       END-IF                                                             
047400     END-IF                                                               
047500     MOVE 'END G-KONTROLL    ' TO PGM-POS                                 
047600     .                                                                    
047700     EJECT                                                                
047800                                                                          
047900 H-BEHANDLA-RADER SECTION.                                                
048000     MOVE 'STA H-BEHANDLA    ' TO PGM-POS                                 
048100                                                                          
048200        IF REQU-FLANNULL NOT = JA                                         
048300                                                                          
048400           IF REQU-KDPGMACT = 'E'                                         
048500           OR REQU-KDPGMACT = 'S'                                         
048600              PERFORM HB-LAS-IN--RADER                                    
048700              IF  RESP-IDMSG-INFO = '308'                                 
048800              AND REQU-KDPGMACT = 'S'                                     
048900                 MOVE NEJ      TO AVSLUTA-SW                              
049000              ELSE                                                        
049100                   PERFORM HG-UPPDATERA-RESTERANDE-RADER                  
049200                   MOVE JA     TO AVSLUTA-SW                              
049300              END-IF                                                      
049400           END-IF                                                         
049500        ELSE                                                              
049600           PERFORM HH-ANNULLERA-ORDER                                     
049700        END-IF                                                            
049800     MOVE 'END H-BEHANDLA    ' TO PGM-POS                                 
049900     .                                                                    
050000     EJECT                                                                
050100                                                                          
050200 HB-LAS-IN--RADER SECTION.                                                
050300     MOVE 'STA HB-LAS-IN-    ' TO PGM-POS                                 
050400                                                                          
050500                                                                          
050600     MOVE +1                   TO WS-INDEX-RESP                           
050700     MOVE NEJ                  TO OBKR-RAD-FINNS-SW                       
050800                                                                          
050900     PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                               
051000     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
051100              OR   WS-INDEX-RESP > WS-INDEX-RESP-MAX                      
051200                                                                          
051300           IF SEGMENT-FINNS                                               
051400             MOVE JA           TO OBKR-RAD-FINNS-SW                       
051500           END-IF                                                         
051600                                                                          
051700           PERFORM HBA-REDIGERA-ORDERBEKR-RAD                             
051800                                                                          
051900           ADD +1              TO WS-INDEX-RESP                           
052000                                                                          
052100        PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                            
052200*-------SPARA STATUSKODEN FRÅN ORDERBEKRÄFTELSE LÄSNINGEN                 
052300        MOVE STATUS-WS      TO STATUS-OBKR-WS                             
052400     END-PERFORM                                                          
052500                                                                          
052600     COMPUTE WS-INDEX-RESP = WS-INDEX-RESP - 1                            
052700     END-COMPUTE                                                          
052800     MOVE WS-INDEX-RESP        TO RESP-KVRADER                            
052900                                                                          
053000     IF OBKR-RAD-FINNS                                                    
053100       IF REQU-KDPGMACT = 'S'                                             
053200         MOVE PRESS-EXECUTE-TO-APPROVE TO RESP-IDMSG-INFO                 
053300         MOVE NEJ                    TO AVSLUTA-SW                        
053400         MOVE NEJ             TO ALLT-SW                                  
053500       ELSE                                                               
053600         MOVE SPACE                    TO RESP-IDMSG-INFO                 
053700       END-IF                                                             
053800     END-IF                                                               
053900                                                                          
054000     MOVE 'END HB-LAS-IN-    ' TO PGM-POS                                 
054100     .                                                                    
054200     EJECT                                                                
054300                                                                          
054400 HBA-REDIGERA-ORDERBEKR-RAD SECTION.                                      
054500     MOVE 'STA HBA-REDIGERA  ' TO PGM-POS                                 
054600                                                                          
054700     MOVE OBKR-KDORDBEK        TO RESP-KDORDBEK(WS-INDEX-RESP)            
054800                                                                          
054900     MOVE 'B'                  TO RESP-KDBEHX(WS-INDEX-RESP)              
055000     MOVE OBKR-IDARTNR         TO RESP-IDARTNR-1--9(WS-INDEX-RESP)        
055100     MOVE OBKR-REKSIFFR        TO RESP-REKSIFFR(WS-INDEX-RESP)            
055200                                                                          
055300     MOVE OBKR-IDARTNR      TO W-IDARTNR                                  
055400     MOVE OHUV-IDSKYLT      TO W-IDSKYLT                                  
055500                                                                          
055600     MOVE REQU-IDDC-KEY  TO WS-IDDC                                       
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
056500     PERFORM IMS-09-GU-BENA-WDD311                                        
056600     IF SEGMENT-FINNS                                                     
056700       MOVE TEXT-BEART      TO TRAUTF8-TECONV-FROM                        
056800     ELSE                                                                 
056900       MOVE WS-CP-EBCDIC    TO TRAUTF8-KDCP                               
057000       MOVE SPACE           TO TRAUTF8-TECONV-FROM                        
057100     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-09-GU-BENA-WDD311                                       
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
057200* -- STRIP SPACE OR CONVERT TO UNICODE                                    
057300     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
057400* -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                           
057500     MOVE TRAUTF8-TECONV-TO TO RESP-BEART(WS-INDEX-RESP)                  
057600                                                                          
057700     MOVE OBKR-IDDC            TO RESP-IDDC-RAD(WS-INDEX-RESP)            
057800                                                                          
057900     MOVE OBKR-KVBEART         TO RESP-KVANTAL(WS-INDEX-RESP)             
058000                                                                          
058100     MOVE OBKR-IDLOPNR         TO WS-AKT-IDLOPNR                          
058200     MOVE OBKR-IDSEKVNR        TO WS-AKT-IDSEKVNR                         
058300                                                                          
058400     MOVE WS-AKT-KEYS          TO RESP-KEYS(WS-INDEX-RESP)                
058500     MOVE 'END HBA-REDIGERA  ' TO PGM-POS                                 
058600     .                                                                    
058700     EJECT                                                                
058800 HG-UPPDATERA-RESTERANDE-RADER SECTION.                                   
058900     MOVE 'STA HG-UPPDATERA  ' TO PGM-POS                                 
059000     MOVE REQU-KDORDBEK(1)      TO WS-AKT-KDORDBEK                        
059100     MOVE REQU-IDARTNR-1--9 (1) TO WS-AKT-IDARTNR                         
059200     MOVE '-'                   TO WS-AKT-FILLER                          
059300     MOVE REQU-REKSIFFR(1)      TO WS-AKT-REKSIFFR                        
059400     MOVE REQU-IDDC-RAD(1)      TO WS-AKT-IDDC                            
059500     MOVE REQU-KEYS(1)          TO WS-AKT-KEYS                            
059600                                                                          
059700     MOVE REQU-RAD(1)          TO WS-AKTUELL-REQU-RAD                     
059800     MOVE WS-AKT-IDARTNR       TO W-Q1-IDARTNR-MAX                        
059900     MOVE WS-AKT-IDLOPNR       TO W-Q1-IDLOPNR-MAX                        
060000     MOVE WS-AKT-IDSEKVNR      TO W-Q1-IDSEKVNR-MAX                       
060100     MOVE WS-AKT-IDDC          TO W-Q1-IDDC-MAX                           
060200     MOVE WS-AKT-KDORDBEK      TO W-Q1-KDORDBEK-MAX                       
060300                                                                          
060400      PERFORM IMS-03-GHU-ORQM-WDQ101-MIN-MAX                              
060500                                                                          
060600      PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                          
060700                                                                          
060800         MOVE JA          TO OBKR-FLOBOK                                  
060900         PERFORM IMS-06-REPL-ORQM-WDQ101                                  
061000                                                                          
061100         PERFORM IMS-04-GHN-ORQM-WDQ101-MIN-MAX                           
061200      END-PERFORM                                                         
061300*                                                                         
061400     MOVE WS-INDEX-REQU        TO WS-KVRADER                              
061500     MOVE WS-INDEX-REQU        TO RESP-KVRADER                            
061600     MOVE 'END HG-UPPDATERA  ' TO PGM-POS                                 
061700     .                                                                    
061800     EJECT                                                                
061900                                                                          
062000 HH-ANNULLERA-ORDER SECTION.                                              
062100     MOVE 'STA HH-ANNULLERA  ' TO PGM-POS                                 
062200                                                                          
062300     MOVE '2'                  TO 4292-SPRAK                              
062400     MOVE OHUV-IDORDER         TO 4292-IDORDER                            
062500     MOVE WS-IDDISTR           TO 4292-IDDISTR                            
062600     MOVE WS-IDKUNDNR          TO 4292-IDKUNDNR                           
062700     MOVE W-IDORDNR            TO 4292-IDKUNDRF                           
062800                                                                          
062900     PERFORM IMS-INSERT-4292-MSG                                          
063000                                                                          
063100                                                                          
063200                                                                          
063300                                                                          
063400*SVARA WEBBEN VIA WZ01                                                    
063500     MOVE JA                   TO HOPP                                    
063600     MOVE 'END HH-ANNULLERA  ' TO PGM-POS                                 
063700     .                                                                    
063800     EJECT                                                                
063900                                                                          
064000 I-STARTA-ORDERAVSLUT SECTION.                                            
064100     MOVE 'STA I-STARTA-     ' TO PGM-POS                                 
064200                                                                          
064300     MOVE W-IDDISTR            TO 4298-MID-IDDISTR                        
064400     MOVE W-IDKUNDNR           TO 4298-MID-IDKUNDNR                       
064500     MOVE W-IDKUNDRF           TO 4298-MID-IDKUNDRF                       
064600     MOVE OHUV-IDORDER         TO 4298-MID-IDORDER                        
064700                                                                          
064800     COMPUTE 4298-LL = LENGTH OF 4298-MID-W4I29801 + 17                   
064900     PERFORM IMS-INSERT-4298-MSG                                          
065000     MOVE 'END I-STARTA-     ' TO PGM-POS                                 
065100     .                                                                    
065200     EJECT                                                                
065300                                                                          
065400 M-HOPPA-TILL-NY-BILD SECTION.                                            
065500     MOVE 'STA M-HOPPA-TILL  ' TO PGM-POS                                 
065600                                                                          
065700                                                                          
065800       MOVE WS-IDORDNR           TO WS-IDKUNDRF                           
065900       INSPECT WS-IDKUNDRF REPLACING LEADING ZERO BY SPACE                
066000                                                                          
066100                                                                          
066200                                                                          
066300     MOVE 'END M-HOPPA-TILL  ' TO PGM-POS                                 
066400     .                                                                    
066500     EJECT                                                                
066600                                                                          
066700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
066800     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
066900                                                                          
067000     MOVE 'GETARG'               TO SUB-KDFUNC                            
067100     MOVE 'CARPARTS.LDC.SPECORDERCONF'     TO SUB-ADDISPABS               
067200     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
067300                                                                          
067400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
067500                                                                          
067600     IF SUB-KDRC > 0                                                      
067700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
067800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
067900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
068000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
068100     END-IF                                                               
068200     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
068300     .                                                                    
068400     SKIP3                                                                
068500 S02-RETURN-RESPONSE SECTION.                                             
068600     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
068700                                                                          
068800     MOVE 'RETURN'                   TO SUB-KDFUNC                        
068900     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
069000                                                                          
069100     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
069200                                                                          
069300     IF SUB-KDRC > 0                                                      
069400       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
069500       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
069600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
069700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
069800     END-IF                                                               
069900     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
070000     .                                                                    
070100     EJECT                                                                
070200                                                                          
070300 IMS-INSERT-4292-MSG SECTION.                                             
070400     MOVE 'STA IMS-INSERT-4292-MSG'  TO  PGM-POS                          
070500                                                                          
070600     MOVE LOW-VALUE TO 4292-Z1 4292-Z2                                    
070700     MOVE SPACE TO GODK-STATUSKODER                                       
070800     CALL CBLTDLI USING ISRT 4292-PCB 4292-MSG-IO-AREA                    
070900     MOVE 4292-STATUS-CODE TO STATUS-WS                                   
071000     PERFORM IMS-STATUSKONTROLL                                           
071100     .                                                                    
071200     SKIP2                                                                
071300 IMS-INSERT-4298-MSG SECTION.                                             
071400     MOVE 'STA IMS-INSERT-4298-MSG'  TO  PGM-POS                          
071500                                                                          
071600     MOVE LOW-VALUE TO 4298-Z1 4298-Z2                                    
071700     MOVE SPACE TO GODK-STATUSKODER                                       
071800     CALL CBLTDLI USING ISRT 4298-PCB 4298-MSG-IO-AREA                    
071900     MOVE 4298-STATUS-CODE TO STATUS-WS                                   
072000     PERFORM IMS-STATUSKONTROLL                                           
072100     .                                                                    
072200     EJECT                                                                
072300 IMS-03-GHU-ORQM-WDQ101-MIN-MAX SECTION.                                  
072400     MOVE 'STA IMS-03-GHU-ORQM-WDQ101   '  TO  PGM-POS                    
072500                                                                          
072600     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
072700                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
072800                    '&FLOBOK   =' NEJ ')'                                 
072900          DELIMITED BY SIZE INTO SSA1                                     
073000     MOVE '  GE'               TO GODK-STATUSKODER                        
073100     CALL CBLTDLI USING GHU ORQM-PCB DLI-IO-AREA-ORQM SSA1                
073200     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
073300     PERFORM IMS-STATUSKONTROLL                                           
073400     .                                                                    
073500     EJECT                                                                
073600 IMS-04-GHN-ORQM-WDQ101-MIN-MAX SECTION.                                  
073700     MOVE 'STA IMS-04-GHN-ORQM-WDQ101   '  TO  PGM-POS                    
073800                                                                          
073900     STRING 'WLORQM01(WDQ101KY>=' W-WDQ101-KEY-MIN                        
074000                    '&WDQ101KY<=' W-WDQ101-KEY-MAX                        
074100                    '&FLOBOK   =' NEJ ')'                                 
074200          DELIMITED BY SIZE INTO SSA1                                     
074300     MOVE '  GEGB'             TO GODK-STATUSKODER                        
074400     CALL CBLTDLI USING GHN ORQM-PCB DLI-IO-AREA-ORQM SSA1                
074500     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
074600     PERFORM IMS-STATUSKONTROLL                                           
074700     .                                                                    
074800     SKIP2                                                                
074900 IMS-06-REPL-ORQM-WDQ101 SECTION.                                         
075000     MOVE 'STA IMS-06-REPL-ORQM-WDQ101    '  TO  PGM-POS                  
075100                                                                          
075200     MOVE '    '               TO GODK-STATUSKODER                        
075300     CALL CBLTDLI USING REPL ORQM-PCB DLI-IO-AREA-ORQM                    
075400     MOVE ORQM-STATUS-CODE     TO STATUS-WS                               
075500     PERFORM IMS-STATUSKONTROLL                                           
075600     .                                                                    
075700     EJECT                                                                
075800 IMS-07-GU-ORQI-WDQ201 SECTION.                                           
075900     MOVE 'STA IMS-07-GU-ORQI-WDQ201      '  TO  PGM-POS                  
076000                                                                          
076100     STRING 'WLORQI01(WDQ2CSEQ =' W-IDGMTREF-X ')'                        
076200          DELIMITED BY SIZE INTO SSA1                                     
076300     MOVE '  GE'               TO GODK-STATUSKODER                        
076400     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA-ORQI01 SSA1               
076500     MOVE ORQI-STATUS-CODE     TO STATUS-WS                               
076600     PERFORM IMS-STATUSKONTROLL                                           
076700     .                                                                    
076800     SKIP2                                                                
076900 IMS-09-GU-BENA-WDD311 SECTION.                                           
077000     MOVE 'STA IMS-09-GU-BENA-WDD311      '  TO  PGM-POS                  
077100                                                                          
077200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
077300          DELIMITED BY SIZE INTO SSA1                                     
077400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
077500          DELIMITED BY SIZE INTO SSA2                                     
077600     MOVE '  GE'               TO GODK-STATUSKODER                        
077700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-BENA SSA1 SSA2            
077800     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
077900     PERFORM IMS-STATUSKONTROLL                                           
078000     .                                                                    
078100     SKIP2                                                                
       IMS-GU-WDB601    SECTION.                                                
           MOVE 'STA IMS-GU-WDB601      '  TO  PGM-POS                          
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
                                                                                
078200 IMS-STATUSKONTROLL SECTION.                                              
078300                                                                          
078400     SET STATUS-IX TO 1                                                   
078500     SEARCH GODK-STATUS                                                   
078600       AT END CALL FELLOG                                                 
078700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
078800     END-SEARCH                                                           
078900     .                                                                    

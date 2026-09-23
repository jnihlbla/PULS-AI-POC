000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2032400.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   94/09/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        BESTÄLLER LISTA SORTIMENT-ANALYS CDC                             
001000*                        REFILLUPPFÖLJNING SDC/NDC                        
001100*        VISSA URVAL KAN ANGES.                                           
001200*        LIST-PROGRAMMEN STARTAS VIA SOP MED OVANSTÅENDE                  
001300*        SOM PARAMETRAR.                                                  
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W2T324U                                             
001700*        MID:         W2I32401                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W2O32401                                            
002100*                                                                         
002200*                                                                         
002300* 2012-01-03  E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1                
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W2032400'.            
003400                                                                          
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  NEJ                         PIC X       VALUE 'N'.                   
003800 77  SW-URVAL-CDC                PIC X       VALUE 'N'.                   
003900 77  SW-URVAL-SDC-NDC            PIC X       VALUE 'N'.                   
004000 77  WS-MOD-TEMFSFEL             PIC X(40)   VALUE SPACE.                 
004100 77  IX                          PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +139  COMP SYNC.        
004300                                                                          
004400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004500     88  INDATA-OK                           VALUE 'J'.                   
004600     88  INDATA-FEL                          VALUE 'N'.                   
004700                                                                          
004800 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004900     88  ALLT-OK                             VALUE 'J'.                   
005000                                                                          
005100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005200     88  EGEN-MID                            VALUE '2324'.                
005300     EJECT                                                                
005400 01  GENERELLA-SUBPROGRAM.                                                
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005800       EJECT                                                              
005900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
006000*01 -COPY WMEDAREA                                                        
006100 01  MESSAGE-CODES.                                                       
006200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
006400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
006500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
006600     SKIP2                                                                
006700 01  LISTUPPSTART                PIC X(30) VALUE                          
006800                                 'LISTA UPPSTARTAD           '.           
006900     EJECT                                                                
007000 01  PROG-TO-PROG-SW.                                                     
007100*    03  -COPY WMSGSOP                                                    
007200     EJECT                                                                
007300 01  WS-PARAMETRAR.                                                       
007400     03  WS-IDUSER.                                                       
007500         05  IDUSER              PIC X(8) VALUE SPACE.                    
007600     03  WS-URVAL.                                                        
007700         05  KATEGORI-FOM        PIC X VALUE SPACE.                       
007800         05  KATEGORI-TOM        PIC X VALUE SPACE.                       
007900         05  KDPRODSL-FOM        PIC 9(2) VALUE ZERO.                     
008000         05  KDPRODSL-TOM        PIC 9(2) VALUE ZERO.                     
008100         05  IDLEVNR-FOM         PIC X(5) VALUE SPACE.                    
008200         05  IDANSK-FOM          PIC 9(3) VALUE ZERO.                     
008300         05  IDANSK-TOM          PIC 9(3) VALUE ZERO.                     
008400     EJECT                                                                
008500 01  WS-PARAMETRAR-2.                                                     
008600     03  WS-IDUSER-2.                                                     
008700         05  URV-IDUSER          PIC X(8) VALUE SPACE.                    
008800     03  WS-URV.                                                          
008900         05  URV-IDREFTAB        PIC X(1) VALUE SPACE.                    
009000     03  WS-IDDC.                                                         
009100         05  FILLER              PIC X(2) VALUE 'DC'.                     
009200         05  URV-IDDC            PIC X(2) VALUE SPACE.                    
009300     EJECT                                                                
009400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009500*                                                                         
009600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009700     SKIP3                                                                
009800*01  MID -COPY W2I32401                                                   
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010100     SKIP3                                                                
010200*01  -COPY WMSGAREA                                                       
010300     EJECT                                                                
010400     03  MOD REDEFINES MSG-AREA.                                          
010500*      05  -COPY W2O32401                                                 
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010800     SKIP3                                                                
010900*01  -COPY WMFSAREA                                                       
011000     EJECT                                                                
011100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011300     SKIP3                                                                
011400 01  NYCKLAR-TILL-DLI.                                                    
011500     03  W-WDGXKEY-X.                                                     
011600          05 W-IDHTYP            PIC X(4)    VALUE '2501'.                
011700          05 W-IDDC              PIC X(2)    VALUE SPACE.                 
011800          05 W-LOWVALUE          PIC X(24)   VALUE LOW-VALUE.             
011900     03  W-IDREFTAB-X.                                                    
012000         05  W-IDREFTAB          PIC X(1)    VALUE SPACE.                 
012100     03  W-IDDC-B6-X.                                                     
012200         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
012300*                                                                         
012400*    --- STATUS-KOD FRÅN IMS                                              
012500 01  STATUS-WS                   PIC XX.                                  
012600     88  SEGMENT-FINNS                       VALUE '  '.                  
012700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012900     SKIP2                                                                
013000 01  GODK-STATUSKODER.                                                    
013100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013200     EJECT                                                                
013300     SKIP3                                                                
013400 01  SSA1                        PIC X(64).                               
013500 01  SSA2                        PIC X(64).                               
013600     EJECT                                                                
013700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL250111'.                    
013800 01  DLI-IO-WL250111.                                                     
013900*    03  -COPY WDGX2502                                                   
014000     EJECT                                                                
014100                                                                          
014200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014300 01   DLI-IO-AREA-B601.                                                   
014400*     03  -COPY WDB601                                                    
014500     EJECT                                                                
014600*    --- IMS FUNKTIONSKODER                                               
014700*01  -COPY W0003                                                          
014800     EJECT                                                                
014900 LINKAGE SECTION.                                                         
015000*01  -COPY W0009   -PRE MSG-                                              
015100     EJECT                                                                
015200*01  -COPY W0009   -PRE ALT-                                              
015300     EJECT                                                                
015400*01  -COPY W0008   -PRE WDR2-                                             
015500     05  FILLER                  PIC X.                                   
015600     EJECT                                                                
015700*01  -COPY W0008      -PRE WDB6-                                          
015800     05  FILLER                  PIC X.                                   
015900     EJECT                                                                
016000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDR2-PCB                       
016100                           WDB6-PCB.                                      
016200 MAIN SECTION.                                                            
016300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDR2-PCB                       
016400                           WDB6-PCB.                                      
016500                                                                          
016600     PERFORM IMS-GET-MSG                                                  
016700     IF SEGMENT-FINNS                                                     
016800       PERFORM A-INIT                                                     
016900       IF MFS-UPDATE                                                      
017000          PERFORM B-KOLLA-INPUT                                           
017100          IF INDATA-OK                                                    
017200             PERFORM C-UPPDATERA                                          
017300             PERFORM MFS-RENSA-FAELT-IN                                   
017400          END-IF                                                          
017500       ELSE                                                               
017600          IF MID-URVAL = ALL '+'                                          
017700             PERFORM MFS-RENSA-FAELT-IN                                   
017800          ELSE                                                            
017900             PERFORM D-SAMMA-SIDA                                         
018000          END-IF                                                          
018100       END-IF                                                             
018200                                                                          
018300       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
018400       PERFORM IMS-INSERT-MSG                                             
018500     END-IF                                                               
018600                                                                          
018700     MOVE ZERO TO RETURN-CODE                                             
018800     GOBACK                                                               
018900     .                                                                    
019000     EJECT                                                                
019100 A-INIT SECTION.                                                          
019200                                                                          
019300     IF MSG-DUBBLA-TRANSKODER                                             
019400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I32401                 
019500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
019600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019700     ELSE                                                                 
019800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I32401                  
019900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
020000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020100     END-IF                                                               
020200                                                                          
020300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
020500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020600                                                                          
020700     MOVE LOW-VALUE TO MSG-AREA                                           
020800     MOVE 'W2O32401' TO MFS-IDMOD                                         
020900     MOVE '2324' TO MOD-IDTRANS                                           
021000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021100                                                                          
021200     IF NOT EGEN-MID                                                      
021300       MOVE SPACE TO MFS-KDTRTYP                                          
021400       MOVE '7' TO MFS-IDPFK                                              
021500     END-IF                                                               
021600                                                                          
021700     IF ENGLISH-TEXT                                                      
021800       MOVE 'GB ' TO MED-IDSKYLT                                          
021900     ELSE                                                                 
022000       MOVE 'S  ' TO MED-IDSKYLT                                          
022100     END-IF                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 B-KOLLA-INPUT SECTION.                                                   
022500                                                                          
022600     MOVE JA TO INDATA-SW                                                 
022700                                                                          
022800     IF MID-URVAL = ALL '+'                                               
022900        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
023000        CALL WMEDKONV USING MED-WMEDAREA                                  
023100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023200                           WS-MOD-TEMFSFEL                                
023300        PERFORM MFS-RENSA-FAELT-IN                                        
023400        MOVE NEJ TO INDATA-SW                                             
023500     END-IF                                                               
023600                                                                          
023700     PERFORM BA-KOLLA-URVAL                                               
023800                                                                          
023900     IF INDATA-OK                                                         
024000        IF SW-URVAL-CDC = JA                                              
024100           PERFORM BB-KOLLA-FOM-TOM                                       
024200        ELSE                                                              
024300           IF SW-URVAL-SDC-NDC = JA                                       
024400              PERFORM BC-KOLLA-IDREFTAB                                   
024500           END-IF                                                         
024600        END-IF                                                            
024700     END-IF                                                               
024800                                                                          
024900     IF INDATA-FEL                                                        
025000        IF WS-MOD-TEMFSFEL = SPACE                                        
025100           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
025200           CALL WMEDKONV USING MED-WMEDAREA                               
025300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
025400        END-IF                                                            
025500        PERFORM MFS-ROER-EJ-FAELT-IN                                      
025600     END-IF                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 BA-KOLLA-URVAL SECTION.                                                  
026000                                                                          
026100     MOVE NEJ TO SW-URVAL-CDC                                             
026200                 SW-URVAL-SDC-NDC                                         
026300                                                                          
026400     IF MID-KATEGORI-FOM NOT = ALL '+'                                    
026500        MOVE JA TO SW-URVAL-CDC                                           
026600        IF MID-KATEGORI-FOM = '1' OR '2' OR '3'                           
026700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KATEGORI-FOM-ATTR             
026800           MOVE MID-KATEGORI-FOM TO KATEGORI-FOM                          
026900        ELSE                                                              
027000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KATEGORI-FOM-ATTR               
027100           MOVE NEJ TO INDATA-SW                                          
027200        END-IF                                                            
027300     END-IF                                                               
027400                                                                          
027500     IF MID-KATEGORI-TOM NOT = ALL '+'                                    
027600        MOVE JA TO SW-URVAL-CDC                                           
027700        IF MID-KATEGORI-TOM  = '1' OR '2' OR '3'                          
027800           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KATEGORI-TOM-ATTR             
027900           MOVE MID-KATEGORI-TOM TO KATEGORI-TOM                          
028000        ELSE                                                              
028100           MOVE MFS-ALFA-FAELT-FEL TO MOD-KATEGORI-TOM-ATTR               
028200           MOVE NEJ TO INDATA-SW                                          
028300        END-IF                                                            
028400     END-IF                                                               
028500                                                                          
028600     IF MID-KDPRODSL-FOM NOT = ALL '+'                                    
028700        MOVE JA TO SW-URVAL-CDC                                           
028800        IF MID-KDPRODSL-FOM NOT NUMERIC                                   
028900           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-FOM-ATTR                
029000           MOVE NEJ TO INDATA-SW                                          
029100        ELSE                                                              
029200           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-FOM-ATTR              
029300           MOVE MID-KDPRODSL-FOM TO KDPRODSL-FOM                          
029400        END-IF                                                            
029500     END-IF                                                               
029600                                                                          
029700     IF MID-KDPRODSL-TOM NOT = ALL '+'                                    
029800        MOVE JA TO SW-URVAL-CDC                                           
029900        IF MID-KDPRODSL-TOM NOT NUMERIC                                   
030000           MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-TOM-ATTR                
030100           MOVE NEJ TO INDATA-SW                                          
030200        ELSE                                                              
030300           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDPRODSL-TOM-ATTR              
030400           MOVE MID-KDPRODSL-TOM TO KDPRODSL-TOM                          
030500        END-IF                                                            
030600     END-IF                                                               
030700                                                                          
030800     IF MID-IDLEVNR NOT = ALL '+'                                         
030900        MOVE JA TO SW-URVAL-CDC                                           
031000        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-ATTR                     
031100        MOVE MID-IDLEVNR TO IDLEVNR-FOM                                   
031200        IF IDLEVNR-FOM = SPACE                                            
031300           MOVE ZERO TO IDLEVNR-FOM                                       
031400        END-IF                                                            
031500     END-IF                                                               
031600                                                                          
031700     IF MID-IDANSK-FOM NOT = ALL '+'                                      
031800        MOVE JA TO SW-URVAL-CDC                                           
031900        IF MID-IDANSK-FOM NOT NUMERIC                                     
032000           MOVE MFS-NUM-FAELT-FEL TO MOD-IDANSK-FOM-ATTR                  
032100           MOVE NEJ TO INDATA-SW                                          
032200        ELSE                                                              
032300           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSK-FOM-ATTR                
032400           MOVE MID-IDANSK-FOM TO IDANSK-FOM                              
032500        END-IF                                                            
032600     END-IF                                                               
032700                                                                          
032800     IF MID-IDANSK-TOM NOT = ALL '+'                                      
032900        MOVE JA TO SW-URVAL-CDC                                           
033000        IF MID-IDANSK-TOM NOT NUMERIC                                     
033100           MOVE MFS-NUM-FAELT-FEL TO MOD-IDANSK-TOM-ATTR                  
033200           MOVE NEJ TO INDATA-SW                                          
033300        ELSE                                                              
033400           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSK-TOM-ATTR                
033500           MOVE MID-IDANSK-TOM TO IDANSK-TOM                              
033600        END-IF                                                            
033700     END-IF                                                               
033800                                                                          
033900     IF SW-URVAL-CDC = JA                                                 
034000        IF MID-IDDC = ALL '+' OR SPACE                                    
034100           CONTINUE                                                       
034200        ELSE                                                              
034300           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-ATTR                       
034400           MOVE NEJ TO INDATA-SW                                          
034500        END-IF                                                            
034600                                                                          
034700        IF MID-IDREFTAB = ALL '+' OR SPACE                                
034800           CONTINUE                                                       
034900        ELSE                                                              
035000           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDREFTAB-ATTR                   
035100           MOVE NEJ TO INDATA-SW                                          
035200        END-IF                                                            
035300     ELSE                                                                 
035400        IF MID-IDDC = ALL '+' OR SPACE                                    
035500           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-ATTR                       
035600           MOVE NEJ TO INDATA-SW                                          
035700        ELSE                                                              
035800           MOVE JA TO SW-URVAL-SDC-NDC                                    
035900           MOVE MID-IDDC TO W-IDDC-B6                                     
036000           PERFORM IMS-GU-WDB601                                          
036100           IF SEGMENT-FINNS                                               
036200           AND (DCS-SDC                                                   
036520           OR   DCS-NDC )                                                 
036600              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-ATTR                  
036700              MOVE MID-IDDC TO URV-IDDC                                   
036800           ELSE                                                           
036900              MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-ATTR                    
037000              MOVE NEJ TO INDATA-SW                                       
037100           END-IF                                                         
037200        END-IF                                                            
037300                                                                          
037400        IF MID-IDREFTAB = ALL '+' OR SPACE                                
037500           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDREFTAB-ATTR                   
037600           MOVE NEJ TO INDATA-SW                                          
037700        ELSE                                                              
037800           MOVE JA TO SW-URVAL-SDC-NDC                                    
037900        END-IF                                                            
038000     END-IF                                                               
038100     .                                                                    
038200     EJECT                                                                
038300 BB-KOLLA-FOM-TOM SECTION.                                                
038400                                                                          
038500     IF KATEGORI-FOM NOT = SPACE AND                                      
038600        KATEGORI-TOM = SPACE                                              
038700        MOVE KATEGORI-FOM TO KATEGORI-TOM                                 
038800     END-IF                                                               
038900     IF KATEGORI-FOM = SPACE AND KATEGORI-TOM = SPACE                     
039000        MOVE '1' TO KATEGORI-FOM                                          
039100        MOVE '3' TO KATEGORI-TOM                                          
039200     END-IF                                                               
039300                                                                          
039400     IF KDPRODSL-FOM > ZERO AND                                           
039500        KDPRODSL-TOM = ZERO                                               
039600        MOVE KDPRODSL-FOM TO KDPRODSL-TOM                                 
039700     END-IF                                                               
039800     IF KDPRODSL-FOM = ZERO AND KDPRODSL-TOM = ZERO                       
039900        MOVE 99 TO KDPRODSL-TOM                                           
040000     END-IF                                                               
040100                                                                          
040200     IF IDANSK-FOM > ZERO AND                                             
040300        IDANSK-TOM = ZERO                                                 
040400        MOVE IDANSK-FOM TO IDANSK-TOM                                     
040500     END-IF                                                               
040600     IF IDANSK-FOM = ZERO AND IDANSK-TOM = ZERO                           
040700        MOVE 999 TO IDANSK-TOM                                            
040800     END-IF                                                               
040900                                                                          
041000     IF KATEGORI-FOM > KATEGORI-TOM                                       
041100        MOVE MFS-NUM-FAELT-FEL TO MOD-KATEGORI-FOM-ATTR                   
041200        MOVE MFS-NUM-FAELT-FEL TO MOD-KATEGORI-TOM-ATTR                   
041300        MOVE NEJ TO INDATA-SW                                             
041400     END-IF                                                               
041500                                                                          
041600     IF KDPRODSL-FOM > KDPRODSL-TOM                                       
041700        MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-FOM-ATTR                   
041800        MOVE MFS-NUM-FAELT-FEL TO MOD-KDPRODSL-TOM-ATTR                   
041900        MOVE NEJ TO INDATA-SW                                             
042000     END-IF                                                               
042100                                                                          
042200     IF IDANSK-FOM > IDANSK-TOM                                           
042300        MOVE MFS-NUM-FAELT-FEL TO MOD-IDANSK-FOM-ATTR                     
042400        MOVE MFS-NUM-FAELT-FEL TO MOD-IDANSK-TOM-ATTR                     
042500        MOVE NEJ TO INDATA-SW                                             
042600     END-IF                                                               
042700     .                                                                    
042800     EJECT                                                                
042900 BC-KOLLA-IDREFTAB SECTION.                                               
043000                                                                          
043100     MOVE MID-IDDC     TO W-IDDC                                          
043200     MOVE MID-IDREFTAB TO W-IDREFTAB                                      
043300     PERFORM IMS-GU-2502                                                  
043400     IF SEGMENT-SAKNAS                                                    
043500        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDREFTAB-ATTR                      
043600        MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-ATTR                          
043700        MOVE NEJ TO INDATA-SW                                             
043800     ELSE                                                                 
043900        MOVE MID-IDREFTAB TO URV-IDREFTAB                                 
044000     END-IF                                                               
044100     .                                                                    
044200     EJECT                                                                
044300 C-UPPDATERA      SECTION.                                                
044400                                                                          
044500     IF SW-URVAL-CDC = JA                                                 
044600        MOVE '2324'   TO MSGSOP-IDTRANS                                   
044700        MOVE '1'      TO MSGSOP-KDMFSFOR                                  
044800        MOVE 'W231S1' TO MSGSOP-IDPROCESS                                 
044900        MOVE 'O'      TO MSGSOP-KDSOPFUNK                                 
045000                                                                          
045100        STRING 'IDUSER(' MSG-SIGNON-USERID ')URVAL('                      
045200               WS-URVAL ')'                                               
045300               DELIMITED BY SIZE INTO MSGSOP-TESYMBV                      
045400     ELSE                                                                 
045500        IF SW-URVAL-SDC-NDC = JA                                          
045610          IF DCS-SDC                                                      
045700            MOVE '2324'   TO MSGSOP-IDTRANS                               
045800            MOVE '1'      TO MSGSOP-KDMFSFOR                              
045900            MOVE 'W231S2' TO MSGSOP-IDPROCESS                             
046000            MOVE 'O'      TO MSGSOP-KDSOPFUNK                             
046100                                                                          
046200            STRING 'IDUSER(' MSG-SIGNON-USERID ')URVAL('                  
046300                   WS-URV ')DC(' WS-IDDC ')'                              
046400                   DELIMITED BY SIZE INTO MSGSOP-TESYMBV                  
046500          ELSE                                                            
046712            IF DCS-NDC                                                    
046800              MOVE '2324'   TO MSGSOP-IDTRANS                             
046900              MOVE '1'      TO MSGSOP-KDMFSFOR                            
047000              MOVE 'W231S3' TO MSGSOP-IDPROCESS                           
047100              MOVE 'O'      TO MSGSOP-KDSOPFUNK                           
047200                                                                          
047300              STRING 'IDUSER(' MSG-SIGNON-USERID ')URVAL('                
047400                     WS-URV ')DC(' WS-IDDC ')'                            
047500                     DELIMITED BY SIZE INTO MSGSOP-TESYMBV                
047600            END-IF                                                        
047700          END-IF                                                          
047800        END-IF                                                            
047900     END-IF                                                               
048000                                                                          
048100     PERFORM IMS-INSERT-ALTMSG                                            
048200     MOVE LISTUPPSTART TO MOD-TEMFSINF                                    
048300     .                                                                    
048400     EJECT                                                                
048500 D-SAMMA-SIDA      SECTION.                                               
048600                                                                          
048700     MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                  
048800     CALL WMEDKONV USING MED-WMEDAREA                                     
048900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
049000                                                                          
049100     PERFORM MFS-LAES-IN-IGEN                                             
049200     PERFORM MFS-ROER-EJ-FAELT-IN                                         
049300     .                                                                    
049400     EJECT                                                                
049500 MFS-RENSA-FAELT-IN SECTION.                                              
049600                                                                          
049700     MOVE MFS-RENSA-FAELT TO MOD-KATEGORI-FOM                             
049800                             MOD-KATEGORI-TOM                             
049900                             MOD-KDPRODSL-FOM                             
050000                             MOD-KDPRODSL-TOM                             
050100                             MOD-IDLEVNR                                  
050200                             MOD-IDANSK-FOM                               
050300                             MOD-IDANSK-TOM                               
050400                             MOD-IDDC                                     
050500                             MOD-IDREFTAB                                 
050600     .                                                                    
050700     EJECT                                                                
050800 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
050900                                                                          
051000     MOVE MFS-ROER-EJ-FAELT TO  MOD-KATEGORI-FOM                          
051100                                MOD-KATEGORI-TOM                          
051200                                MOD-KDPRODSL-FOM                          
051300                                MOD-KDPRODSL-TOM                          
051400                                MOD-IDLEVNR                               
051500                                MOD-IDANSK-FOM                            
051600                                MOD-IDANSK-TOM                            
051700                                MOD-IDDC                                  
051800                                MOD-IDREFTAB                              
051900     .                                                                    
052000     EJECT                                                                
052100 MFS-LAES-IN-IGEN SECTION.                                                
052200                                                                          
052300     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KATEGORI-FOM-ATTR                  
052400                                   MOD-KATEGORI-TOM-ATTR                  
052500                                   MOD-KDPRODSL-FOM-ATTR                  
052600                                   MOD-KDPRODSL-TOM-ATTR                  
052700                                   MOD-IDLEVNR-ATTR                       
052800                                   MOD-IDANSK-FOM-ATTR                    
052900                                   MOD-IDANSK-TOM-ATTR                    
053000                                   MOD-IDDC-ATTR                          
053100                                   MOD-IDREFTAB-ATTR                      
053200     .                                                                    
053300     EJECT                                                                
053400* --- IMS SEKTIONER ---                                                   
053500     SKIP3                                                                
053600 IMS-GET-MSG SECTION.                                                     
053700                                                                          
053800     MOVE '  QC' TO GODK-STATUSKODER                                      
053900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
054000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
054100     PERFORM IMS-STATUSKONTROLL                                           
054200     .                                                                    
054300     SKIP3                                                                
054400 IMS-INSERT-MSG SECTION.                                                  
054500                                                                          
054600     IF ENGLISH-TEXT                                                      
054700       MOVE 'N' TO MFS-KDHUVOMR                                           
054800     END-IF                                                               
054900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
055000     MOVE SPACE TO GODK-STATUSKODER                                       
055100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
055200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
055300     PERFORM IMS-STATUSKONTROLL                                           
055400     .                                                                    
055500     SKIP3                                                                
055600 IMS-INSERT-ALTMSG SECTION.                                               
055700                                                                          
055800     MOVE SPACE TO GODK-STATUSKODER                                       
055900     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
056000     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
056100     PERFORM IMS-STATUSKONTROLL                                           
056200     .                                                                    
056300     EJECT                                                                
056400 IMS-GU-2502 SECTION.                                                     
056500     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
056600          DELIMITED BY SIZE INTO SSA1                                     
056700     STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                        
056800          DELIMITED BY SIZE INTO SSA2                                     
056900     MOVE '  GE' TO GODK-STATUSKODER                                      
057000     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WL250111 SSA1 SSA2             
057100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
057200     PERFORM IMS-STATUSKONTROLL                                           
057300     .                                                                    
057400     SKIP2                                                                
057500 IMS-GU-WDB601    SECTION.                                                
057600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
057700          DELIMITED BY SIZE INTO SSA1                                     
057800     MOVE '  GE' TO GODK-STATUSKODER                                      
057900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
058000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
058100     PERFORM IMS-STATUSKONTROLL                                           
058200     .                                                                    
058300     EJECT                                                                
058400 IMS-STATUSKONTROLL SECTION.                                              
058500                                                                          
058600     SET STATUS-IX TO 1                                                   
058700     SEARCH GODK-STATUS                                                   
058800       AT END                                                             
058900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
059000         DELIMITED BY SIZE INTO FELTEXT                                   
059100         CALL FELLOG                                                      
059200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
059300     END-SEARCH                                                           
059400     .                                                                    

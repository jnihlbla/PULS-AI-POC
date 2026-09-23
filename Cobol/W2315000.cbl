000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2315000.                                    
000300 AUTHOR.                     HENRIK ARONSSON.                             
000400 DATE-WRITTEN.               OKT 1990.                                    
000500     REMARKS.                                                             
000600*    FUNKTION.                                                            
000700*        PROGRAMMET SKAPAR ARBETSFIL FÖR VCC-ARTIKLAR                     
000800*                                        RENAULT-ARTIKLAR                 
000900*                                        VTC-&VBC-ARTIKLAR                
001000*        AV INFILERNA.                                                    
001100*        W23151-FILEN STYR BEARBETNINGEN.                                 
001200*                                                                         
001300*                                                                         
001400*                                                                         
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP3                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900 FILE-CONTROL.                                                            
002000     SKIP3                                                                
002100**************** INFILER ***************                                  
002200     SKIP3                                                                
002300***************************** STYRFIL                                     
002400                                                                          
002500     SELECT  W23151          ASSIGN UT-S-W23150D1.                        
002600                                                                          
002700**************************** ORDERINGÅNGSREGISTERPOSTER                   
002800                                                                          
002900     SELECT  W23123          ASSIGN UT-S-W23150D2.                        
003000                                                                          
003100**************************** INSAMLAT FRÅN ANDRA SYSTEM                   
003200                                                                          
003300     SELECT  W23136          ASSIGN UT-S-W23150D3.                        
003400                                                                          
003500**************************** RO-INFORMATION                               
003600                                                                          
003700     SELECT  W23141          ASSIGN UT-S-W23150D4.                        
003800                                                                          
003900**************************** AVBOKADE RADER                               
004000                                                                          
004100     SELECT  W22509          ASSIGN UT-S-W23150D5.                        
004200                                                                          
004300**************************** OKS-INFORMATION                              
004400                                                                          
004500     SELECT  W23125          ASSIGN UT-S-W23150D9.                        
004600                                                                          
004700                                                                          
004800                                                                          
004900**************** UTFILER ***************                                  
005000     SKIP3                                                                
005100**************************** ARBETSFIL VCC-ARTIKLAR                       
005200                                                                          
005300     SELECT  W23152          ASSIGN UT-S-W23150D6.                        
005400                                                                          
005500     EJECT                                                                
005600 DATA DIVISION.                                                           
005700 FILE SECTION.                                                            
005800     SKIP3                                                                
005900 FD  W23151                                                               
006000     RECORDING F                                                          
006100     BLOCK 0                                                              
006200     LABEL RECORDS STANDARD.                                              
006300     SKIP1                                                                
006400*01  POST -COPY W231210    -L.                                            
006500                                                                          
006600 FD  W23123                                                               
006700     RECORDING F                                                          
006800     BLOCK 0                                                              
006900     LABEL RECORDS STANDARD.                                              
007000     SKIP1                                                                
007100*01  POST  -COPY W231212    -L.                                           
007200                                                                          
007300 FD  W23136                                                               
007400     RECORDING F                                                          
007500     BLOCK 0                                                              
007600     LABEL RECORDS STANDARD.                                              
007700     SKIP1                                                                
007800*01  POST -COPY W231214    -L.                                            
007900                                                                          
008000 FD  W23141                                                               
008100     RECORDING F                                                          
008200     BLOCK 0                                                              
008300     LABEL RECORDS STANDARD.                                              
008400     SKIP1                                                                
008500*01  POST -COPY W231217    -L.                                            
008600                                                                          
008700 FD  W22509                                                               
008800     RECORDING F                                                          
008900     BLOCK 0                                                              
009000     LABEL RECORDS STANDARD.                                              
009100     SKIP1                                                                
009200*01  POST -COPY W225P233   -L.                                            
009300                                                                          
009400 FD  W23125                                                               
009500     RECORDING F                                                          
009600     BLOCK 0                                                              
009700     LABEL RECORDS STANDARD.                                              
009800     SKIP1                                                                
009900*01  POST -COPY W231250    -L.                                            
010000                                                                          
010100 FD  W23152                                                               
010200     RECORDING F                                                          
010300     BLOCK 0                                                              
010400     LABEL RECORDS STANDARD.                                              
010500     SKIP1                                                                
010600*01  POST -COPY W231W001   -PRE W23152- -L.                               
010700                                                                          
010800 WORKING-STORAGE SECTION.                                                 
010900                                                                          
011000                                                                          
011100*    -COPY WY2000W2                                                       
011200     SKIP3                                                                
011300 01  RKOD                    PIC S9(4)               COMP SYNC.           
011400                                                                          
011500 01  KONSTANTER.                                                          
011600     03  JA                  PIC X       VALUE 'J'.                       
011700     03  NEJ                 PIC X       VALUE 'N'.                       
011800                                                                          
011900                                                                          
012000 01  VCBV-KTO                PIC 9(7).                                    
012100 01  FILLER REDEFINES VCBV-KTO.                                           
012200     03  FILLER              PIC 9(3).                                    
012300     03  SOEK-KONTO          PIC 9(4).                                    
012400                                                                          
012500 01  W.                                                                   
012600     05  W-PROGNAMN          PIC X(6)    VALUE 'W23150'.                  
012700     05  W-KORDAT-X.                                                      
012800         10  W-AA            PIC 9(2).                                    
012900         10  W-VV            PIC 9(2).                                    
013000         10  W-D             PIC 9.                                       
013100     05  W-KORDAT  REDEFINES W-KORDAT-X PIC S9(5).                        
013200                                                                          
013300     SKIP3                                                                
013400**************************************** INDEX                            
013500     SKIP1                                                                
013600 01  IDEX.                                                                
013700     05  IXKDC               PIC S9(9)               COMP  SYNC.          
013800     05  IXPER               PIC S9(9)               COMP  SYNC.          
013900     05  IX                  PIC S9(9)               COMP  SYNC.          
014000     SKIP1                                                                
014100 01  WS-DAGENS-AAVV          PIC 9(4).                                    
014200 01  FILLER REDEFINES WS-DAGENS-AAVV.                                     
014300     03 WS-DAGENS-AA         PIC 9(2).                                    
014400     03 WS-DAGENS-VV         PIC 9(2).                                    
014500                                                                          
014600 01  WS-INNEV-TIRP           PIC 9(2)    VALUE ZERO.                      
014700                                                                          
014800*01  -COPY WWPRODSL                                                       
014900                                                                          
015000 01  DYNAMISKA-SUBPROGRAM.                                                
015100     05  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
015200     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
015300     05  ABEND               PIC X(8)    VALUE 'ABEND'.                   
015400     05  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
015500     EJECT                                                                
015600*                                                                         
015700*******************************************  PARAMETRAR TILL              
015800*                                            POSTSUM                      
015900*01  -COPY W0005    -PRE POSTSUM-                                         
016000     EJECT                                                                
016100**************************************** ID-BEGREPP                       
016200     SKIP1                                                                
016300 01  W23151-ID.                                                           
016400     05  W23151-IDIDARTNR    PIC S9(9).                                   
016500     SKIP1                                                                
016600 01  W23136-ID.                                                           
016700     05  W23136-IDIDARTNR    PIC S9(9).                                   
016800     SKIP1                                                                
016900 01  W23123-ID.                                                           
017000     05  W23123-IDIDARTNR    PIC S9(9).                                   
017100     SKIP1                                                                
017200 01  W23141-ID.                                                           
017300     05  W23141-IDIDARTNR    PIC S9(9).                                   
017400     SKIP1                                                                
017500 01  W22509-ID.                                                           
017600     05  W22509-IDIDARTNR    PIC S9(9).                                   
017700     SKIP1                                                                
017800 01  W23125-ID.                                                           
017900     05  W23125-IDIDARTNR    PIC S9(9).                                   
018000     EJECT                                                                
018100 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W23150'.                  
018200 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
018300*01  -COPY WDATKORT                                                       
018400     EJECT                                                                
018500*01  -COPY WDATAREA                                                       
018600     EJECT                                                                
018700***************************** AREA FÖR STYRFIL, W22151                    
018800     SKIP1                                                                
018900*01  AREA  -COPY W231210    -PRE W23151-.                                 
019000     EJECT                                                                
019100***************************** AREA FÖR ORDERING.FIL, W23123               
019200     SKIP1                                                                
019300*01  AREA -COPY W231212    -PRE W23123-.                                  
019400     EJECT                                                                
019500***************************** AREA FÖR INS.FIL, W23136                    
019600     SKIP1                                                                
019700*01  AREA -COPY  W231214    -PRE W23136-.                                 
019800     EJECT                                                                
019900***************************** AREA FÖR RO FIL, W23141                     
020000     SKIP1                                                                
020100*01  AREA -COPY  W231217    -PRE W23141-.                                 
020200     EJECT                                                                
020300***************************** AREA FÖR AVB.RADER FIL, W22509              
020400     SKIP1                                                                
020500*01  AREA -COPY  W225P233   -PRE W22509-.                                 
020600     EJECT                                                                
020700***************************** AREA FÖR OKS FIL, W23125                    
020800     SKIP1                                                                
020900*01  AREA -COPY  W231250    -PRE W23125-.                                 
021000     EJECT                                                                
021100***************************** AREA FÖR ARBETSFILER                        
021200     SKIP1                                                                
021300*01  AREA -COPY W231W001   -PRE ARB-                                      
021400     EJECT                                                                
021500 PROCEDURE DIVISION.                                                      
021600                                                                          
021700     PERFORM A-INIT                                                       
021800     PERFORM B-SKAPA-ARBETSFILER                                          
021900     PERFORM Z-FINIT                                                      
022000     MOVE ZERO TO RKOD                                                    
022100     GOBACK                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 A-INIT SECTION.                                                          
022500                                                                          
022600     OPEN  INPUT  W23151 W23123 W23136 W23141 W22509 W23125               
022700           OUTPUT W23152                                                  
022800     CALL DATKORT USING W-PROGNAMN DATUMKORT-ID DATUMKORT                 
022900                                                                          
023000     MOVE LOW-VALUE TO W23151-ID                                          
023100                       W23136-ID                                          
023200                       W23123-ID                                          
023300                       W23141-ID                                          
023400                       W22509-ID                                          
023500                       W23125-ID                                          
023600                                                                          
023700     MOVE D-AAR   TO W-AA                                                 
023800                     WS-DAGENS-AA                                         
023900     MOVE D-VECKA TO W-VV                                                 
024000                     WS-DAGENS-VV                                         
024100     MOVE D-DAGNR TO W-D                                                  
024200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
024300                                                                          
024400     MOVE 'AAVV  '       TO DAT-KDDATFORM                                 
024500     MOVE WS-DAGENS-AAVV TO DAT-I-TIDATUM                                 
024600     CALL WDATKONV USING DAT-KDDATFORM                                    
024700                         DAT-I-TIDATUM                                    
024800                         DAT-O-TIDATUM                                    
024900                         DAT-KDSVAR                                       
025000     MOVE DAT-TIRP       TO WS-INNEV-TIRP                                 
025100     .                                                                    
025200     EJECT                                                                
025300 B-SKAPA-ARBETSFILER SECTION.                                             
025400                                                                          
025500     PERFORM S01-LAS-W23151                                               
025600                                                                          
025700     PERFORM UNTIL W23151-ID = HIGH-VALUE                                 
025800       PERFORM S21-NOLLA-ARBETSAREA                                       
025900       PERFORM BA-FLYTTA-W23151                                           
026000       PERFORM BB-HAMTA-INS-FIL                                           
026100       PERFORM BC-HAMTA-ORDERING-FIL                                      
026200       PERFORM BD-HAMTA-RO-FIL                                            
026300       PERFORM BE-HAMTA-AVB-FIL                                           
026400       PERFORM BF-HAMTA-OKS-FIL                                           
026500       PERFORM S11-SKRIV-ARBETSFIL                                        
026600       PERFORM S01-LAS-W23151                                             
026700     END-PERFORM                                                          
026800     .                                                                    
026900     EJECT                                                                
027000 BA-FLYTTA-W23151 SECTION.                                                
027100                                                                          
027200     MOVE '001'           TO ARB-IDPTYP                                   
027300     MOVE W23151-IDARTNR  TO ARB-IDARTNR                                  
027400     MOVE W23151-IDANSK   TO ARB-IDANSK                                   
027500     MOVE W23151-IDLEVNR  TO ARB-IDLEVNR                                  
027600     MOVE W23151-IDPROD   TO ARB-IDPROD                                   
027700     MOVE W23151-IDFKNGRP TO ARB-IDFKNGRP                                 
027800     MOVE W23151-KDVVKL   TO ARB-KDVVKL                                   
027900     MOVE W23151-KVQ      TO ARB-KVQ                                      
028000     MOVE W23151-KVOVERF  TO ARB-KVOVERF                                  
028100     MOVE W23151-KVSLUTKP TO ARB-KVSLUTKP                                 
028200     MOVE W23151-IDLKTO   TO ARB-IDLKTO                                   
028300     MOVE W23151-PRARTSTD TO ARB-PRARTSTD                                 
028400     MOVE W23151-KDLTK    TO ARB-KDLTK                                    
028500     MOVE W23151-KVBR     TO ARB-KVBR                                     
028600                                                                          
028700     MOVE W23151-KDPRODSL TO ARB-KDPRODSL                                 
028800                                                                          
028900     IF  W23151-KVAVROP-EFTERSLAP-X = LOW-VALUE                           
029000       MOVE ZERO TO ARB-KVAVROP-EFTERSLAP                                 
029100     ELSE                                                                 
029200       MOVE W23151-KVAVROP-EFTERSLAP                                      
029300                 TO ARB-KVAVROP-EFTERSLAP                                 
029400     END-IF                                                               
029500                                                                          
029600     MOVE +1 TO IXPER                                                     
029700     PERFORM UNTIL IXPER > +12                                            
029800       IF W23151-PLAN-INLEV-DEL (IXPER) = LOW-VALUE                       
029900         MOVE ZERO TO W23151-PLAN-INLEV-DEL (IXPER)                       
030000       ELSE                                                               
030100         MOVE W23151-PLAN-INLEV-DEL (IXPER)                               
030200                   TO ARB-PLAN-INLEV-DEL (IXPER)                          
030300       END-IF                                                             
030400       ADD +1 TO IXPER                                                    
030500     END-PERFORM                                                          
030600                                                                          
030700     MOVE W23151-FLJANEJ-C2    TO ARB-FLJANEJ-C2                          
030800     MOVE W23151-KVMP (1)      TO ARB-KVMP (1)                            
030900     MOVE W23151-KVPB-SATS (1) TO ARB-KVPB-SATS (1)                       
031000     MOVE W23151-KVPB-SEP (1)  TO ARB-KVPB-SEP (1)                        
031100     MOVE W23151-REDIRLEV (1)  TO ARB-REDIRLEV (1)                        
031200     MOVE W23151-KVAKS (1)     TO ARB-KVAKS (1)                           
031300     MOVE W23151-KVAKS-E (1)   TO ARB-KVAKS-E (1)                         
031400     MOVE W23151-KVAKS-F (1)   TO ARB-KVAKS-F (1)                         
031500     MOVE W23151-KVLS (1)      TO ARB-KVLS (1)                            
031600     MOVE W23151-KVRESS (1)    TO ARB-KVRESS (1)                          
031700     MOVE W23151-KVROS (1)     TO ARB-KVROS (1)                           
031800     MOVE W23151-KVSLAGER (1)  TO ARB-KVSLAGER (1)                        
031900                                                                          
032000     MOVE +1 TO IXPER                                                     
032100     PERFORM UNTIL IXPER > +12                                            
032200       MOVE W23151-BEHOVSDEL (1, IXPER)                                   
032300                             TO ARB-BEHOVSDEL (1, IXPER)                  
032400       ADD +1                TO IXPER                                     
032500     END-PERFORM                                                          
032600                                                                          
032700     IF W23151-FLJANEJ-C2 = JA                                            
032800       MOVE W23151-KVMP (2)      TO ARB-KVMP (2)                          
032900       MOVE W23151-KVPB-SATS (2) TO ARB-KVPB-SATS (2)                     
033000       MOVE W23151-KVPB-SEP (2)  TO ARB-KVPB-SEP (2)                      
033100       MOVE W23151-REDIRLEV (2)  TO ARB-REDIRLEV (2)                      
033200       MOVE W23151-KVAKS (2)     TO ARB-KVAKS (2)                         
033300       MOVE W23151-KVAKS-E (2)   TO ARB-KVAKS-E (2)                       
033400       MOVE W23151-KVAKS-F (2)   TO ARB-KVAKS-F (2)                       
033500       MOVE W23151-KVLS (2)      TO ARB-KVLS (2)                          
033600       MOVE W23151-KVRESS (2)    TO ARB-KVRESS (2)                        
033700       MOVE W23151-KVROS (2)     TO ARB-KVROS (2)                         
033800       MOVE W23151-KVSLAGER (2)  TO ARB-KVSLAGER (2)                      
033900                                                                          
034000       MOVE +1 TO IXPER                                                   
034100       PERFORM UNTIL IXPER > +12                                          
034200         MOVE W23151-BEHOVSDEL (2, IXPER)                                 
034300                             TO ARB-BEHOVSDEL (2, IXPER)                  
034400         ADD +1              TO IXPER                                     
034500       END-PERFORM                                                        
034600     END-IF                                                               
034700                                                                          
034800     IF  W23151-KDERS GREATER +20                                         
034900       MOVE +70 TO ARB-KDSORT2                                            
035000     ELSE                                                                 
035100       IF W23151-KDERS GREATER ZERO                                       
035200         MOVE +60 TO ARB-KDSORT2                                          
035300       ELSE                                                               
035400         IF W23151-FLAVRART = JA                                          
035500           MOVE +10 TO ARB-KDSORT2                                        
035600         ELSE                                                             
035700           IF W23151-KDHF GREATER ZERO                                    
035800             MOVE +20 TO ARB-KDSORT2                                      
035900           ELSE                                                           
036000             IF W23151-KDUART = 'M'                                       
036100               MOVE +30 TO ARB-KDSORT2                                    
036200             ELSE                                                         
036300               IF W23151-KDUART = 'S'                                     
036400                 MOVE +40 TO ARB-KDSORT2                                  
036500               ELSE                                                       
036600                 IF (W23151-REDIRLEV (1) = 1.00                           
036700                     AND W23151-FLJANEJ-C2 = NEJ)                         
036800                     OR (W23151-REDIRLEV (1) = 1.00                       
036900                     AND W23151-REDIRLEV (2) = 1.00)                      
037000                   MOVE +50  TO ARB-KDSORT2                               
037100                 ELSE                                                     
037200                   MOVE W23151-TIFINLV   TO TMP1-YYWWD                    
037300                   MOVE W-KORDAT         TO TMP2-YYWWD                    
037400                   PERFORM WY2000P2                                       
037500                   IF TMP1-YYWWD > TMP2-YYWWD                             
037600                     MOVE +80 TO ARB-KDSORT2                              
037700                   END-IF                                                 
037800                 END-IF                                                   
037900               END-IF                                                     
038000             END-IF                                                       
038100           END-IF                                                         
038200         END-IF                                                           
038300       END-IF                                                             
038400     END-IF                                                               
038500                                                                          
038600     IF ARB-KDSORT2 GREATER ZERO                                          
038700       MOVE NEJ TO ARB-FLJANEJ-NORMAL                                     
038800     ELSE                                                                 
038900       MOVE JA            TO ARB-FLJANEJ-NORMAL                           
039000       MOVE W23151-KDVVKL TO ARB-KDVVKL-SORT                              
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400 BB-HAMTA-INS-FIL SECTION.                                                
039500                                                                          
039600     PERFORM UNTIL W23136-ID NOT < W23151-ID                              
039700       PERFORM S02-LAS-W23136                                             
039800     END-PERFORM                                                          
039900                                                                          
040000     IF W23136-ID = W23151-ID                                             
040100       MOVE +1 TO IXKDC                                                   
040200       PERFORM UNTIL IXKDC > +2                                           
040300         MOVE W23136-KVANTAL-UTLEV-12P (IXKDC)                            
040400                         TO ARB-KVANTAL-UTLEV-12P (IXKDC)                 
040500         MOVE W23136-KVANTAL-TILL-AR (IXKDC)                              
040600                         TO ARB-KVANTAL-TILL-AR (IXKDC)                   
040700         MOVE W23136-KVANTAL-FRAN-AR (IXKDC)                              
040800                         TO ARB-KVANTAL-FRAN-AR (IXKDC)                   
040900         MOVE W23136-KVANTAL-FRAN-12P (IXKDC)                             
041000                         TO ARB-KVANTAL-FRAN-12P (IXKDC)                  
041100         MOVE W23136-KVANTAL-UTCLEAR (IXKDC)                              
041200                         TO ARB-KVANTAL-UTCLEAR (IXKDC)                   
041300         MOVE W23136-KVANTAL-INCLEAR (IXKDC)                              
041400                         TO ARB-KVANTAL-INCLEAR (IXKDC)                   
041500         MOVE W23136-KVANTAL-UPPINV (IXKDC)                               
041600                         TO ARB-KVANTAL-UPPINV  (IXKDC)                   
041700         MOVE W23136-KVANTAL-NEDINV (IXKDC)                               
041800                         TO ARB-KVANTAL-NEDINV  (IXKDC)                   
041900         MOVE W23136-KVANTAL-PLANINL (IXKDC)                              
042000                         TO ARB-KVANTAL-PLANINL (IXKDC)                   
042100         MOVE W23136-KVANTAL-OPLANINL (IXKDC)                             
042200                         TO ARB-KVANTAL-OPLANINL (IXKDC)                  
042300         ADD W23136-KVANTAL-LAN (IXKDC)                                   
042400                         W23136-KVANTAL-RETUR (IXKDC)                     
042500                         GIVING ARB-KVANTAL-LAN-RETUR (IXKDC)             
042600         MOVE W23136-KVANTAL-SKROT (IXKDC)                                
042700                         TO ARB-KVANTAL-SKROT (IXKDC)                     
042800         MOVE W23136-KVANTAL-K-ORDER (IXKDC)                              
042900                         TO ARB-KVANTAL-K-ORDER (IXKDC)                   
043000         ADD +1          TO IXKDC                                         
043100       END-PERFORM                                                        
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500 BC-HAMTA-ORDERING-FIL SECTION.                                           
043600                                                                          
043700     PERFORM UNTIL W23123-ID NOT < W23151-ID                              
043800       PERFORM S03-LAS-W23123                                             
043900     END-PERFORM                                                          
044000                                                                          
044100     IF W23123-ID = W23151-ID                                             
044200       MOVE +1 TO IXPER                                                   
044300       PERFORM UNTIL IXPER > +12                                          
044400         ADD W23123-KVOI-PROG    (IXPER)                                  
044500                         TO ARB-SUOI-PROGNOS-12P-C1                       
044600         ADD W23123-KVOI-DIV     (IXPER)                                  
044700                         TO ARB-SUOI-DIVERSE-12P-C1                       
044800         ADD W23123-KVOI-SATS    (IXPER)                                  
044900                         TO ARB-SUOI-SATS-12P-C1                          
045000         ADD W23123-KVOI-SDC     (IXPER)                                  
045100                         TO ARB-SUOI-PROGNOS-12P-C2                       
045200         ADD W23123-KVOI-NDC     (IXPER)                                  
045300                         TO ARB-SUOI-PROGNOS-12P-C2                       
045400         ADD +1          TO IXPER                                         
045500       END-PERFORM                                                        
045600                                                                          
045700       MOVE W23123-KVOI-PROG    (1)                                       
045800                           TO ARB-KVOI-PROGNOSPAV-C1                      
045900       MOVE W23123-KVOI-DIV     (1)                                       
046000                           TO ARB-KVOI-DIVERSE-C1                         
046100       MOVE W23123-KVOI-SATS    (1)                                       
046200                           TO ARB-KVOI-SATS-C1                            
046300       MOVE W23123-KVOI-SDC     (1)                                       
046400                           TO ARB-KVOI-PROGNOSPAV-C2                      
046500       ADD  W23123-KVOI-NDC     (1)                                       
046600                           TO ARB-KVOI-PROGNOSPAV-C2                      
046700                                                                          
046800       MOVE +1 TO IXPER                                                   
046900       PERFORM UNTIL IXPER > WS-INNEV-TIRP                                
047000         COMPUTE ARB-SUOI-AR-C1 = ARB-SUOI-AR-C1 +                        
047100             (W23123-KVOI-PROG    (IXPER) +                               
047200              W23123-KVOI-DIV     (IXPER) +                               
047300              W23123-KVOI-SATS    (IXPER))                                
047400                                                                          
047500         COMPUTE ARB-SUOI-AR-C2 = ARB-SUOI-AR-C2 +                        
047600             (W23123-KVOI-SDC     (IXPER) +                               
047700              W23123-KVOI-NDC     (IXPER))                                
047800                                                                          
047900         ADD +1 TO IXPER                                                  
048000       END-PERFORM                                                        
048100     END-IF                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 BD-HAMTA-RO-FIL SECTION.                                                 
048500                                                                          
048600     PERFORM UNTIL W23141-ID NOT < W23151-ID                              
048700       PERFORM S05-LAS-W23141                                             
048800     END-PERFORM                                                          
048900                                                                          
049000     IF W23141-ID = W23151-ID                                             
049100       MOVE W23141-CLAGERDEL (1) TO ARB-RO-DEL (1)                        
049200       MOVE W23141-CLAGERDEL (2) TO ARB-RO-DEL (2)                        
049300     END-IF                                                               
049400     .                                                                    
049500     EJECT                                                                
049600 BE-HAMTA-AVB-FIL SECTION.                                                
049700                                                                          
049800     PERFORM UNTIL W22509-ID NOT < W23151-ID                              
049900       PERFORM S06-LAS-W22509                                             
050000     END-PERFORM                                                          
050100                                                                          
050200     IF W22509-ID = W23151-ID                                             
050300       MOVE    +1 TO IX                                                   
050400       PERFORM UNTIL IX > +3                                              
050500         ADD W22509-KVAVBRAD-CDC-1-2  (IX)                                
050600             W22509-KVAVBRAD-CDC-3-4 (IX)                                 
050700             GIVING ARB-KVAVBRAD (1, IX)                                  
050800                                                                          
050900         ADD W22509-KVINORD-CDC-1-2  (IX)                                 
051000             W22509-KVINORD-CDC-3-4 (IX)                                  
051100             GIVING ARB-KVINORD (1, IX)                                   
051200                                                                          
051300         ADD W22509-KVFYSAVV-CDC-1-2  (IX)                                
051400             W22509-KVFYSAVV-CDC-3-4 (IX)                                 
051500             GIVING ARB-KVFYSAVV (1)                                      
051600                                                                          
051700         ADD W22509-KVAVBRAD-SDC-1-2  (IX)                                
051800             W22509-KVAVBRAD-SDC-3-4 (IX)                                 
051900             GIVING ARB-KVAVBRAD (2, IX)                                  
052000                                                                          
052100         ADD W22509-KVINORD-SDC-1-2  (IX)                                 
052200             W22509-KVINORD-SDC-3-4 (IX)                                  
052300             GIVING ARB-KVINORD (2, IX)                                   
052400                                                                          
052500         ADD W22509-KVFYSAVV-SDC-1-2  (IX)                                
052600             W22509-KVFYSAVV-SDC-3-4 (IX)                                 
052700             GIVING ARB-KVFYSAVV (2)                                      
052800         ADD +1 TO IX                                                     
052900       END-PERFORM                                                        
053000     END-IF                                                               
053100     .                                                                    
053200     EJECT                                                                
053300                                                                          
053400 BF-HAMTA-OKS-FIL SECTION.                                                
053500                                                                          
053600     PERFORM UNTIL W23125-ID NOT < W23151-ID                              
053700       PERFORM S07-LAS-W23125                                             
053800     END-PERFORM                                                          
053900                                                                          
054000     IF W23125-ID = W23151-ID                                             
054100        MOVE W23125-KVOKS-C1 TO ARB-KVOKS(1)                              
054200        MOVE W23125-KVOKS-C2 TO ARB-KVOKS(2)                              
054300     END-IF                                                               
054400     .                                                                    
054500     EJECT                                                                
054600                                                                          
054700                                                                          
054800 S01-LAS-W23151 SECTION.                                                  
054900                                                                          
055000     READ W23151 INTO W23151-AREA END                                     
055100       MOVE HIGH-VALUE TO W23151-ID                                       
055200     END-READ                                                             
055300                                                                          
055400     IF W23151-ID NOT = HIGH-VALUE                                        
055500       MOVE W23151-IDARTNR TO W23151-IDIDARTNR                            
055600       MOVE 'W23151'       TO POSTSUM-FDNAMN                              
055700       MOVE 'W23150D1'     TO POSTSUM-DDNAMN2                             
055800       CALL POSTSUM USING POSTSUM-PARM                                    
055900     END-IF                                                               
056000     .                                                                    
056100     EJECT                                                                
056200 S02-LAS-W23136 SECTION.                                                  
056300                                                                          
056400     READ W23136 INTO W23136-AREA END                                     
056500       MOVE HIGH-VALUE TO W23136-ID                                       
056600     END-READ                                                             
056700                                                                          
056800     IF W23136-ID NOT = HIGH-VALUE                                        
056900       MOVE W23136-IDARTNR TO W23136-IDIDARTNR                            
057000       MOVE 'W23136'       TO POSTSUM-FDNAMN                              
057100       MOVE 'W23150D3'     TO POSTSUM-DDNAMN2                             
057200       CALL POSTSUM USING POSTSUM-PARM                                    
057300     END-IF                                                               
057400     .                                                                    
057500     EJECT                                                                
057600 S03-LAS-W23123 SECTION.                                                  
057700                                                                          
057800     READ W23123 INTO W23123-AREA END                                     
057900       MOVE HIGH-VALUE TO W23123-ID                                       
058000     END-READ                                                             
058100                                                                          
058200     IF W23123-ID NOT = HIGH-VALUE                                        
058300       MOVE W23123-IDARTNR TO W23123-IDIDARTNR                            
058400       MOVE 'W23123'       TO POSTSUM-FDNAMN                              
058500       MOVE 'W23150D2'     TO POSTSUM-DDNAMN2                             
058600       CALL POSTSUM USING POSTSUM-PARM                                    
058700     END-IF                                                               
058800     .                                                                    
058900     EJECT                                                                
059000 S05-LAS-W23141 SECTION.                                                  
059100                                                                          
059200     READ W23141 INTO W23141-AREA END                                     
059300       MOVE HIGH-VALUE TO W23141-ID                                       
059400     END-READ                                                             
059500                                                                          
059600     IF W23141-ID NOT = HIGH-VALUE                                        
059700       MOVE W23141-IDARTNR TO W23141-IDIDARTNR                            
059800       MOVE 'W23141'       TO POSTSUM-FDNAMN                              
059900       MOVE 'W23150D4'     TO POSTSUM-DDNAMN2                             
060000       CALL POSTSUM USING POSTSUM-PARM                                    
060100     END-IF                                                               
060200     .                                                                    
060300     EJECT                                                                
060400 S06-LAS-W22509 SECTION.                                                  
060500                                                                          
060600     READ W22509 INTO W22509-AREA END                                     
060700       MOVE HIGH-VALUE TO W22509-ID                                       
060800     END-READ                                                             
060900                                                                          
061000     IF W22509-ID NOT = HIGH-VALUE                                        
061100       MOVE W22509-IDARTNR TO W22509-IDIDARTNR                            
061200       MOVE 'W22509'   TO POSTSUM-FDNAMN                                  
061300       MOVE 'W23150D5' TO POSTSUM-DDNAMN2                                 
061400       CALL POSTSUM USING POSTSUM-PARM                                    
061500     END-IF                                                               
061600     .                                                                    
061700     EJECT                                                                
061800 S07-LAS-W23125 SECTION.                                                  
061900                                                                          
062000     READ W23125 INTO W23125-AREA END                                     
062100       MOVE HIGH-VALUE TO W23125-ID                                       
062200     END-READ                                                             
062300                                                                          
062400     IF W23125-ID NOT = HIGH-VALUE                                        
062500       MOVE W23125-IDARTNR TO W23125-IDIDARTNR                            
062600       MOVE 'W23125'       TO POSTSUM-FDNAMN                              
062700       MOVE 'W23150D9'     TO POSTSUM-DDNAMN2                             
062800       CALL POSTSUM USING POSTSUM-PARM                                    
062900     END-IF                                                               
063000     .                                                                    
063100     EJECT                                                                
063200 S11-SKRIV-ARBETSFIL SECTION.                                             
063300                                                                          
063400     MOVE ARB-KDPRODSL           TO TEST-KDPRODSL                         
063500     IF KDPRODSL-VOLVO-ALL                                                
063600*****  ARBETSFIL FÖR VCC-ARTIKLAR OCH CARPAC                              
063700       WRITE W23152-POST FROM ARB-AREA                                    
063800       MOVE 'W23152'   TO POSTSUM-FDNAMN                                  
063900       MOVE 'W23150D6' TO POSTSUM-DDNAMN2                                 
064000       CALL POSTSUM USING POSTSUM-PARM                                    
064100     END-IF                                                               
064200                                                                          
064300     EJECT                                                                
064400     .                                                                    
064500 S21-NOLLA-ARBETSAREA SECTION.                                            
064600                                                                          
064700     MOVE SPACE TO ARB-IDPTYP                                             
064800                   ARB-FLJANEJ-NORMAL                                     
064900                   ARB-IDLEVNR                                            
065000     MOVE ZERO  TO ARB-KDVVKL-SORT                                        
065100                   ARB-KDSORT2                                            
065200                   ARB-IDARTNR                                            
065300                   ARB-IDANSK                                             
065400                   ARB-IDFKNGRP                                           
065500                   ARB-IDPROD                                             
065600                   ARB-KDVVKL                                             
065700                   ARB-KVQ                                                
065800                   ARB-KVOVERF                                            
065900                   ARB-KVSLUTKP                                           
066000                   ARB-IDLKTO                                             
066100                   ARB-PRARTSTD                                           
066200                   ARB-KDLTK                                              
066300                   ARB-KVBR                                               
066400                   ARB-KVAVROP-EFTERSLAP                                  
066500                   ARB-KDPRODSL                                           
066600                                                                          
066700     MOVE +1 TO IXPER                                                     
066800     PERFORM UNTIL IXPER > +12                                            
066900       MOVE ZERO TO ARB-KVAVROP-PLANINL (IXPER)                           
067000       ADD +1    TO IXPER                                                 
067100     END-PERFORM                                                          
067200                                                                          
067300     MOVE SPACE TO ARB-FLJANEJ-C2                                         
067400                                                                          
067500     MOVE +1 TO IXKDC                                                     
067600     PERFORM UNTIL IXKDC > +2                                             
067700       MOVE ZERO TO ARB-KVMP      (IXKDC)                                 
067800                    ARB-KVPB-SATS (IXKDC)                                 
067900                    ARB-KVPB-SEP  (IXKDC)                                 
068000                    ARB-REDIRLEV  (IXKDC)                                 
068100                    ARB-KVAKS     (IXKDC)                                 
068200                    ARB-KVAKS-E   (IXKDC)                                 
068300                    ARB-KVAKS-F   (IXKDC)                                 
068400                    ARB-KVLS      (IXKDC)                                 
068500                    ARB-KVRESS    (IXKDC)                                 
068600                    ARB-KVOKS     (IXKDC)                                 
068700                    ARB-KVROS     (IXKDC)                                 
068800                    ARB-KVSLAGER  (IXKDC)                                 
068900                                                                          
069000       MOVE +1 TO IXPER                                                   
069100       PERFORM UNTIL IXPER > +12                                          
069200         MOVE ZERO TO ARB-KVBEHOV-PERIOD (IXKDC, IXPER)                   
069300         ADD +1    TO IXPER                                               
069400       END-PERFORM                                                        
069500                                                                          
069600       MOVE ZERO TO ARB-TIRODAT           (IXKDC)                         
069700                    ARB-KVRORAD-0-4       (IXKDC)                         
069800                    ARB-KVRORAD-5-8       (IXKDC)                         
069900                    ARB-KVRORAD-9         (IXKDC)                         
070000                    ARB-KVANTAL-UTLEV-12P (IXKDC)                         
070100                    ARB-KVANTAL-TILL-AR   (IXKDC)                         
070200                    ARB-KVANTAL-FRAN-AR   (IXKDC)                         
070300                    ARB-KVANTAL-FRAN-12P  (IXKDC)                         
070400                    ARB-KVANTAL-UTCLEAR   (IXKDC)                         
070500                    ARB-KVANTAL-INCLEAR   (IXKDC)                         
070600                    ARB-KVANTAL-UPPINV    (IXKDC)                         
070700                    ARB-KVANTAL-NEDINV    (IXKDC)                         
070800                    ARB-KVANTAL-PLANINL   (IXKDC)                         
070900                    ARB-KVANTAL-OPLANINL  (IXKDC)                         
071000                    ARB-KVANTAL-LAN-RETUR (IXKDC)                         
071100                    ARB-KVANTAL-SKROT     (IXKDC)                         
071200                    ARB-KVANTAL-K-ORDER   (IXKDC)                         
071300                                                                          
071400       MOVE +1 TO IX                                                      
071500       PERFORM UNTIL IX > +3                                              
071600         MOVE ZERO TO ARB-KVAVBRAD (IXKDC, IX)                            
071700                      ARB-KVINORD  (IXKDC, IX)                            
071800         ADD +1    TO IX                                                  
071900       END-PERFORM                                                        
072000                                                                          
072100       MOVE ZERO TO ARB-KVFYSAVV (IXKDC)                                  
072200       ADD +1    TO  IXKDC                                                
072300     END-PERFORM                                                          
072400                                                                          
072500     MOVE ZERO TO ARB-KVOI-PROGNOSPAV-C1                                  
072600                  ARB-KVOI-DIVERSE-C1                                     
072700                  ARB-KVOI-SATS-C1                                        
072800                  ARB-KVOI-PROGNOSPAV-C2                                  
072900                  ARB-KVOI-DIVERSE-C2                                     
073000                  ARB-SUOI-AR-C1                                          
073100                  ARB-SUOI-AR-C2                                          
073200                  ARB-SUOI-PROGNOS-12P-C1                                 
073300                  ARB-SUOI-DIVERSE-12P-C1                                 
073400                  ARB-SUOI-SATS-12P-C1                                    
073500                  ARB-SUOI-PROGNOS-12P-C2                                 
073600                  ARB-SUOI-DIVERSE-12P-C2                                 
073700     .                                                                    
073800     EJECT                                                                
073900 Z-FINIT SECTION.                                                         
074000                                                                          
074100     CLOSE W23151 W23123 W23136 W23141 W22509 W23125                      
074200           W23152                                                         
074300                                                                          
074400     MOVE 'S' TO POSTSUM-OPKOD                                            
074500     CALL POSTSUM USING POSTSUM-PARM                                      
074600     .                                                                    
074700     EJECT                                                                
074800     EJECT                                                                
074900*    -COPY WY2000P2                                                       

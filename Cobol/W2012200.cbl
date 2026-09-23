000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W20122.                                                  
000800*AUTHOR.       IDK   BS.                                                  
000900*DATE-WRITTEN. MAJ   79.                                                  
001100*    FUNKTION.                                                            
001200*              TP-PROGRAM O R D E R I N G Å N G S G R A F.                
001300*    INDATA.                                                              
001400*        TRANSAKTION:  W2T122                                             
001500*        MOD:  W2I12201                                                   
001600*    UTDATA:                                                              
001700*        MOD:  W2O12201                                                   
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000*                                                                         
002100 DATA DIVISION.                                                           
002200*                                                                         
002300 WORKING-STORAGE SECTION.                                                 
002312                                                                          
002313*    -COPY WY2000W9                                                       
002314     SKIP3                                                                
002315*    -COPY WY2000W5                                                       
002316     SKIP3                                                                
002500 77  IDPGM                   PIC X(8)   VALUE 'W2012200'.                 
002910 77  JA                      PIC X      VALUE 'J'.                        
003000 77  NEJ                     PIC X      VALUE 'N'.                        
003100 77  DOWHIL-SNURRA           PIC S9(3)  COMP-3.                           
003300***** VÄRDET FÅS FRÅN MIDEN *****                                         
003400 77  WS-IDARTNR              PIC X(9).                                    
003500***** VÄRDENA FÅS FRÅN WDD1 *****                                         
003600 77  WS-KDCLAGER             PIC X.                                       
003700 77  WS-TIFINLV              PIC 9(5).                                    
003800*                                                                         
003900*****  SWITCHAR                                                           
004000 77  FEL-SW                  PIC X.                                       
004100*                                                                         
004200*****  INDEX                                                              
004300 77  WS-KVOI-OCCUR-8-INDX    PIC S9(9)  VALUE ZERO COMP SYNC.             
004400 77  WS-KVOI-OCCUR-16-INDX   PIC S9(9)  VALUE ZERO COMP SYNC.             
004500 77  WS-INDX                 PIC S9(9)  VALUE ZERO COMP SYNC.             
004600 77  MOD-INDX                PIC S9(9)  VALUE ZERO COMP SYNC.             
004700 77  INDX-KDCLAGER           PIC S9(9)  VALUE ZERO COMP SYNC.             
004710 77  PER-IX                  PIC S9(5)  VALUE ZERO COMP SYNC.             
004720 77  VECKA-IX                PIC S9(5)  VALUE ZERO COMP SYNC.             
004800     EJECT                                                                
004900 77  SENASTE-AAR-P-2         PIC 9(3).                                    
005000 77  VIKT                    PIC 9(7)V99  COMP-3.                         
005030                                                                          
005040 01  WS-TISEKEL-AA.                                                       
005050     03  WS-DAGENS-SEKEL     PIC 99.                                      
005060     03  WS-DAGENS-AA        PIC 99.                                      
005070 01  FILLER REDEFINES WS-TISEKEL-AA.                                      
005080     03  WS-DAGENS-TIAAAA    PIC 9(4).                                    
005090 01  WS-INLAST-TIAAAA        PIC 9(4).                                    
005091 01  FILLER  REDEFINES WS-INLAST-TIAAAA.                                  
005092     03  FILLER              PIC 9(2).                                    
005093     03  WS-INLAST-AA        PIC 9(2).                                    
005094 01  DAGENS-PERIOD           PIC 9.                                       
005095                                                                          
005100 01  SENASTE-AAAAP-X.                                                     
005101     03  SENASTE-AAAAP-SEKEL PIC 9(2).                                    
005110     03  SENASTE-AAR-PERIOD-X.                                            
005200       05  SENASTE-TIAAP-AAR     PIC 99.                                  
005300       05  SENASTE-TIAAP-PERIOD  PIC 9.                                   
005400     03  SENASTE-AAR-PERIOD REDEFINES SENASTE-AAR-PERIOD-X                
005410                                 PIC 9(3).                                
005420 01  FILLER  REDEFINES SENASTE-AAAAP-X.                                   
005430     03  SENASTE-TIAAAAP-AAAA  PIC 9(4).                                  
005440     03  FILLER              PIC 9.                                       
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700     05  WDATKONV            PIC X(08)   VALUE 'WDATKONV'.                
005730     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
005740     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
005750     03  W005INIT            PIC X(8)    VALUE 'W005INIT'.                
005800     EJECT                                                                
005900*---------------------------------------- DATUMAREA                       
006000 01  DATUMAREA.                                                           
006100*  03  -COPY WDATAREA                                                     
006200*---------------------------------------- PARAMETRAR TILL W005INIT        
006220*01    -COPY WMSGINIT                                                     
006300     SKIP3                                                                
006400 01  HELPFLT-FOR-DIAGRAM.                                                 
006500     03  Y-MIN               PIC S9(7)   COMP-3  VALUE +9999999.          
006600     03  Y-MAX               PIC S9(7)   COMP-3  VALUE ZERO.              
006700     03  STEGLGD             PIC S9(7)   COMP-3  VALUE ZERO.              
006800     03  HALV-STEGLGD        PIC S9(7)   COMP-3  VALUE ZERO.              
006900     03  Y-INDX              PIC S9(9)   COMP SYNC  VALUE ZERO.           
007000     03  X-INDX              PIC S9(9)   COMP SYNC  VALUE ZERO.           
007100     03  Y-NIVA              PIC S9(9)   COMP SYNC  VALUE ZERO.           
007200     03  OLD-Y-NIVA          PIC S9(9)   COMP SYNC  VALUE ZERO.           
007300     03  STAPEL-TKN          PIC X(3)            VALUE '***'.             
007400     03  STAPEL-LINJE        PIC X               VALUE '!'.               
007500     EJECT                                                                
007600*****  MEDDELANDEN.                                                       
007700 01  FELMEDD.                                                             
007800   03  MEDD01                PIC X(27)   VALUE 'PART NUMBER IS NOT        
007900-                                              ' NUMERIC'.                
008000   03  MEDD02                PIC X(35)   VALUE 'THIS ARTICLE IS NO        
008100-                                             'T IN THE DATABASE'.        
008200   03  MEDD03                PIC X(15)   VALUE 'ARTIKELN ERSATT'.         
008300   03  MEDD04.                                                            
008400     05  MEDD04-TEXT         PIC X(16)   VALUE 'ERSÄTTNINGSKOD ='.        
008500     05  MEDD04-KOD          PIC 99.                                      
008600   03  MEDD05                PIC X(18) VALUE 'CLAGER NOT NUMERIC'.        
008700     SKIP3                                                                
008800*****  MELLANLAGRING AV TIAAP  GÖRS REDEFINES PÅ                          
008900 01  WS-TIAAP                PIC  9(3).                                   
009000*                                                                         
009100 01  WS-TIAAP-RED            REDEFINES WS-TIAAP.                          
009200   03 WS-TIAAP-AAR           PIC  99.                                     
009300   03 WS-TIAAP-PERIOD        PIC 9.                                       
009400*                                                                         
009500     SKIP3                                                                
009600*****  LAGRING AV UPPGIFTER FRÅN O-I BASEN                                
009700*                                                                         
009710 01  WS-KVOI-TOT             PIC 9(9).                                    
009800 01  WS-KVOI-FLTB.                                                        
009900   03  WS-KVOI-OCCUR-16      PIC S9(7)   OCCURS 16.                       
009910                                                                          
009920*    -COPY W221PERT                                                       
010023                                                                          
010030     SKIP2                                                                
010100*---------------------------------------- DB-NYCKLAR                      
010200 01  W-IDARTNR-X.                                                         
010300     03  W-IDARTNR           PIC S9(9)       COMP-3.                      
010400 01  W-IDSKYLT-X.                                                         
010500     03  W-IDSKYLT           PIC X(3)   VALUE 'S  '.                      
010800 01  W-TIAAAA-X.                                                          
010900     03  W-TIAAAA            PIC 9(4)   VALUE ZERO.                       
011100     EJECT                                                                
011200*01  -COPY WMFSAREA.                                                      
011400     EJECT                                                                
011500*    -COPY WMSGAREA                                                       
011700     EJECT                                                                
011800*    03 AREA -COPY W2O12201  -PRE MOD- -RED MSG-AREA                      
012000     SKIP3                                                                
012100*    -COPY W2I12201 -PRE MID-                                             
012300     EJECT                                                                
012400*****  ARBETSAREOR FÖR IMS-SEKTIONERNA.                                   
012500*                                                                         
012600 01  IMS-WS.                                                              
012700   03  FILLER                PIC X(8)    VALUE 'IMS-WS  '.                
012800     SKIP2                                                                
012900*****  STATUSKOD FRÅN IMS                                                 
013000   03  STATUS-WS             PIC XX.                                      
013100     88  SEGMENT-FINNS       VALUE '  '.                                  
013200     88  SEGMENT-SAKNAS      VALUE 'GE'.                                  
013300*                                                                         
013400   03  GODK-STATUSKODER.                                                  
013500     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX  PIC XX.               
013600*                                                                         
013700     SKIP3                                                                
013800****  IMS-FUNKTIONSKODER.                                                 
013900*    03 -COPY W0003                                                       
014100     SKIP3                                                                
014200*****  SSA                                                                
014300 01  SSA1                PIC X(60).                                       
014400 01  SSA2                PIC X(60).                                       
014500 01  SSA3                PIC X(60).                                       
014600     EJECT                                                                
014700****  DLI-IO-AREAN.                                                       
014800 01  DLI-IO-AREA-01.                                                      
015000*03  WLARTC01 -COPY WDK601                                                
015200     EJECT                                                                
015210 01  DLI-IO-AREA-11.                                                      
015300*03  WLARTC11 -COPY WDK611                                                
015800     EJECT                                                                
015810 01  FILLER.                                                              
015811 03  DLI-IO-AREA          PIC X(2500) VALUE SPACE.                        
015820     SKIP2                                                                
015900*03  WLOIGB01 -COPY WDL801 -PRE OIGB01- -RED DLI-IO-AREA.                 
016100     SKIP3                                                                
016200*03  WLOIGB11 -COPY WDL811              -RED DLI-IO-AREA.                 
016400     SKIP3                                                                
017100*03  WLBENA11 -COPY WDD311 -PRE BENA11- -RED DLI-IO-AREA.                 
017300     EJECT                                                                
017400 LINKAGE SECTION.                                                         
017500*    -COPY W0009 -PRE MSG-                                                
017700     SKIP3                                                                
017800*    -COPY W0008 -PRE USEA-                                               
018000    05 FILLER              PIC X.                                         
018100     EJECT                                                                
018110*    -COPY W0008 -PRE ARTC-                                               
018120    05 WLARTC-KONKAT-KEY   PIC X.                                         
018130     EJECT                                                                
018200*    -COPY W0008 -PRE OIGB-                                               
018400     05  KONKAT-KEY          PIC X.                                       
018500     EJECT                                                                
018600*    -COPY W0008 -PRE BENA-                                               
018800     05  FILLER              PIC X.                                       
018900     EJECT                                                                
019000 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
019010                                  ARTC-PCB                                
019100                                OIGB-PCB BENA-PCB.                        
019200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
019210                                   ARTC-PCB                               
019300                                OIGB-PCB BENA-PCB.                        
019400     PERFORM IMS-LAES-MID                                                 
019500     IF SEGMENT-FINNS                                                     
019600       MOVE NEJ TO FEL-SW                                                 
019700       PERFORM LGD-JUSTERING-MID                                          
019800       IF WS-IDARTNR NOT NUMERIC                                          
019900         MOVE MEDD01 TO MOD-MESSAGE                                       
020000       ELSE                                                               
020100         IF  WS-KDCLAGER NOT NUMERIC                                      
020200           MOVE MEDD05 TO MOD-MESSAGE                                     
020300         ELSE                                                             
020400           MOVE 'IDAG  '       TO DAT-KDDATFORM                           
020500           CALL WDATKONV  USING DAT-KDDATFORM                             
020600                                DAT-I-TIDATUM                             
020700                                DAT-O-TIDATUM                             
020800                                DAT-KDSVAR                                
020900           MOVE DAT-TISEKEL    TO WS-DAGENS-SEKEL                         
020901                                  SENASTE-AAAAP-SEKEL                     
020910           MOVE DAT-TIAA-PPER  TO SENASTE-TIAAP-AAR WS-DAGENS-AA          
021000           MOVE DAT-TIP        TO SENASTE-TIAAP-PERIOD                    
021010                                  DAGENS-PERIOD                           
021100           IF  SENASTE-TIAAP-PERIOD > 1                                   
021200             SUBTRACT 1 FROM SENASTE-TIAAP-PERIOD                         
021300           ELSE                                                           
021400             SUBTRACT 1 FROM SENASTE-TIAAAAP-AAAA                         
021500             MOVE 8 TO SENASTE-TIAAP-PERIOD                               
021600           END-IF                                                         
021601                                                                          
021602*          -- MINSKA ÅRET MED 2                                           
021610           MOVE SENASTE-TIAAP-AAR TO TMP1-YY                              
021620           PERFORM WY2000P9                                               
021630           SUBTRACT 2 FROM TMP1-YY                                        
021640           PERFORM WY2000P9                                               
021700           COMPUTE SENASTE-AAR-P-2 = TMP1-YY * 10                         
021800                             + SENASTE-TIAAP-PERIOD                       
021810                                                                          
021900           PERFORM LAES-ARTIKELBAS                                        
022000           IF SEGMENT-FINNS                                               
022100             PERFORM NOLLSTAELLNING-AV-FLT                                
022200             PERFORM LAES-O-I-BAS                                         
022300             IF SEGMENT-FINNS                                             
022400               PERFORM UTLAEGG-AV-GRAF                                    
022500             END-IF                                                       
022600           ELSE                                                           
022700             MOVE MEDD02 TO MOD-MESSAGE                                   
022800           END-IF                                                         
022900         END-IF                                                           
023000       END-IF                                                             
023100       IF FEL-SW NOT = JA                                                 
023200         MOVE +1246     TO MSG-KVLL                                       
023300       ELSE                                                               
023400         MOVE +68       TO MSG-KVLL                                       
023500       END-IF                                                             
023600       PERFORM IMS-SKRIV-MOD                                              
023700     END-IF                                                               
023800     MOVE ZERO TO RETURN-CODE                                             
023900     GOBACK                                                               
024000     CONTINUE.                                                            
024100     EJECT                                                                
024200 LGD-JUSTERING-MID SECTION.                                               
024300     IF MSG-DUBBLA-TRANSKODER                                             
024400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I12201                 
024500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
024600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024700     ELSE                                                                 
024800       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W2I12201                   
024900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
025000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025100     END-IF                                                               
025110     MOVE ALL '+' TO MSGI-WMSGINIT                                        
025120     MOVE '001'             TO MSGI-KDCALL                                
025130     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
025131     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
025132     MOVE '2122'            TO MSGI-IDTRANS                               
025140     IF MFS-IDTRANS = '2122'                                              
025150     OR (MID-IDARTNR-IN NUMERIC                                           
025151     AND MID-IDARTNR-IN > ZERO)                                           
025160         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
025170     END-IF                                                               
025180     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
025190     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
025400     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
025800     IF MID-KDCLAGER-IN NOT NUMERIC                                       
025900       MOVE 1               TO WS-KDCLAGER                                
026000     ELSE                                                                 
026100       MOVE MID-KDCLAGER-IN TO WS-KDCLAGER                                
026200     END-IF                                                               
026300     MOVE LOW-VALUE TO MOD-W2O12201                                       
026400     MOVE '2122' TO MOD-TRANS                                             
026500     MOVE 'W2O12201' TO MFS-IDMOD                                         
026600     MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                    
026700     MOVE WS-KDCLAGER TO MOD-KDCLAGER-UT                                  
026800     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
026900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
027000                             MOD-MESSAGE                                  
027100                             MOD-KDCLAGER-IN                              
027200     CONTINUE.                                                            
027300     EJECT                                                                
027400 LAES-ARTIKELBAS SECTION.                                                 
027500*                                                                         
027600*****  ARTIKEL SEGMENT (ARTC01)                                           
027700     MOVE WS-IDARTNR TO W-IDARTNR                                         
027800     PERFORM IMS-GET-ARTC01-ARTIKEL                                       
027900     IF SEGMENT-FINNS                                                     
028000       MOVE ART-TIFINLV TO WS-TIFINLV                                     
028100       PERFORM IMS-GET-ARTC11                                             
028200       IF  SEGMENT-FINNS                                                  
028300         MOVE CLAG-KDLTK TO MOD-KDLTK                                     
029100         IF  CLAG-KDERS > +19                                             
029200           MOVE MEDD03 TO MOD-MESSAGE                                     
029300         ELSE                                                             
029400           IF  CLAG-KDERS > +0                                            
029500             MOVE 'ERSÄTTNINGSKOD =' TO MEDD04-TEXT                       
029600             MOVE CLAG-KDERS TO MEDD04-KOD                                
029700             MOVE MEDD04 TO MOD-MESSAGE                                   
029800           END-IF                                                         
029900         END-IF                                                           
030000       ELSE                                                               
030100         MOVE ZERO       TO MOD-KDLTK                                     
030200       END-IF                                                             
031600         PERFORM IMS-GET-BENA11                                           
031700         MOVE BENA11-TEXT-BEART TO MOD-BENAMNING                          
031900       MOVE SPACE TO STATUS-WS                                            
032000     END-IF                                                               
032100     CONTINUE.                                                            
032200     EJECT                                                                
032300 NOLLSTAELLNING-AV-FLT SECTION.                                           
032400     MOVE ZERO TO WS-KVOI-FLTB                                            
032500     CONTINUE.                                                            
032600     EJECT                                                                
032700 LAES-O-I-BAS SECTION.                                                    
032800     PERFORM IMS-GET-OIGB01-ARTIKEL                                       
032900     IF SEGMENT-FINNS                                                     
032910                                                                          
032920           MOVE +1 TO WS-INDX                                             
032930           PERFORM UNTIL WS-INDX > +16                                    
032940             MOVE ZERO TO                                                 
032950                          WS-KVOI-OCCUR-16 (WS-INDX)                      
032960             ADD +1 TO WS-INDX                                            
032970           END-PERFORM                                                    
032980                                                                          
032990************* START ÅR ************                                       
032991     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA - 2                              
032992     MOVE W-TIAAAA   TO WS-INLAST-TIAAAA                                  
032993                                                                          
032994     PERFORM IMS-GET-OIGB11-SEGMENT                                       
032995                                                                          
032996     IF SEGMENT-FINNS                                                     
032997                                                                          
032998       MOVE DAGENS-PERIOD    TO PER-IX                                    
032999       MOVE PER-VECKA-FOM (PER-IX)                                        
033000                             TO VECKA-IX                                  
033010                                                                          
033020       PERFORM UNTIL VECKA-IX > +52                                       
033030                                                                          
033040         MOVE ZERO           TO WS-KVOI-TOT                               
033050         PERFORM UNTIL VECKA-IX >                                         
033060                       PER-VECKA-TOM (PER-IX)                             
033070                                                                          
033071           IF WS-KDCLAGER = '1' OR '0'                                    
033080              ADD AAR-KVOI-PROG (VECKA-IX)                                
033090                             TO WS-KVOI-TOT                               
033091           END-IF                                                         
033092           IF WS-KDCLAGER = '2' OR '0'                                    
033093              ADD AAR-KVOI-SDC  (VECKA-IX)                                
033094                             TO WS-KVOI-TOT                               
033095              ADD AAR-KVOI-NDC  (VECKA-IX)                                
033096                             TO WS-KVOI-TOT                               
033097           END-IF                                                         
033098           ADD +1            TO VECKA-IX                                  
033099         END-PERFORM                                                      
033100                                                                          
033101         COMPUTE WS-KVOI-OCCUR-16-INDX = 16 - (8 *                        
033102                (SENASTE-TIAAAAP-AAAA - WS-INLAST-TIAAAA) +               
033103                (SENASTE-TIAAP-PERIOD - PER-IX) )                         
033104         END-COMPUTE                                                      
033105         ADD WS-KVOI-TOT     TO WS-KVOI-OCCUR-16                          
033106                                (WS-KVOI-OCCUR-16-INDX)                   
033110                                                                          
033120         ADD +1              TO PER-IX                                    
033130                                                                          
033140       END-PERFORM                                                        
033150     END-IF                                                               
033160************* FÖREGÅENDE ÅR ************                                  
033170     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA - 1                              
033180     MOVE W-TIAAAA   TO WS-INLAST-TIAAAA                                  
033190                                                                          
033191     PERFORM IMS-GET-OIGB11-SEGMENT                                       
033192                                                                          
033193     IF SEGMENT-FINNS                                                     
033194                                                                          
033195       MOVE +1               TO PER-IX                                    
033196       MOVE PER-VECKA-FOM (PER-IX)                                        
033197                             TO VECKA-IX                                  
033198                                                                          
033199       PERFORM UNTIL VECKA-IX > +52                                       
033200                                                                          
033210         MOVE ZERO           TO WS-KVOI-TOT                               
033220         PERFORM UNTIL VECKA-IX >                                         
033230                       PER-VECKA-TOM (PER-IX)                             
033240                                                                          
033261           IF WS-KDCLAGER = '1' OR '0'                                    
033262              ADD AAR-KVOI-PROG (VECKA-IX)                                
033263                             TO WS-KVOI-TOT                               
033264           END-IF                                                         
033265           IF WS-KDCLAGER = '2' OR '0'                                    
033266              ADD AAR-KVOI-SDC  (VECKA-IX)                                
033267                             TO WS-KVOI-TOT                               
033268              ADD AAR-KVOI-NDC  (VECKA-IX)                                
033269                             TO WS-KVOI-TOT                               
033270           END-IF                                                         
033271           ADD +1            TO VECKA-IX                                  
033280         END-PERFORM                                                      
033290                                                                          
033292         COMPUTE WS-KVOI-OCCUR-16-INDX = 16 - (8 *                        
033293                (SENASTE-TIAAAAP-AAAA - WS-INLAST-TIAAAA) +               
033294                (SENASTE-TIAAP-PERIOD - PER-IX) )                         
033295         END-COMPUTE                                                      
033296         ADD WS-KVOI-TOT     TO WS-KVOI-OCCUR-16                          
033297                                (WS-KVOI-OCCUR-16-INDX)                   
033298                                                                          
033299         ADD +1              TO PER-IX                                    
033300                                                                          
033310       END-PERFORM                                                        
033320     END-IF                                                               
033330************* I ÅR ************                                           
033340     COMPUTE W-TIAAAA = WS-DAGENS-TIAAAA                                  
033350     MOVE W-TIAAAA   TO WS-INLAST-TIAAAA                                  
033360                                                                          
033370     PERFORM IMS-GET-OIGB11-SEGMENT                                       
033380                                                                          
033390     IF SEGMENT-FINNS                                                     
033391                                                                          
033392       MOVE +1               TO PER-IX                                    
033393       MOVE PER-VECKA-FOM (PER-IX)                                        
033394                             TO VECKA-IX                                  
033395                                                                          
033396       IF DAGENS-PERIOD > 1                                               
033397                                                                          
033398         PERFORM UNTIL VECKA-IX >                                         
033399                         PER-VECKA-TOM (DAGENS-PERIOD - 1)                
033400                                                                          
033410           MOVE ZERO         TO WS-KVOI-TOT                               
033420           PERFORM UNTIL VECKA-IX >                                       
033430                         PER-VECKA-TOM (PER-IX)                           
033440                                                                          
033461             IF WS-KDCLAGER = '1' OR '0'                                  
033462                ADD AAR-KVOI-PROG (VECKA-IX)                              
033463                             TO WS-KVOI-TOT                               
033464             END-IF                                                       
033465             IF WS-KDCLAGER = '2' OR '0'                                  
033466                ADD AAR-KVOI-SDC  (VECKA-IX)                              
033467                             TO WS-KVOI-TOT                               
033468                ADD AAR-KVOI-NDC  (VECKA-IX)                              
033469                             TO WS-KVOI-TOT                               
033470             END-IF                                                       
033471             ADD +1          TO VECKA-IX                                  
033480           END-PERFORM                                                    
033490                                                                          
033492           COMPUTE WS-KVOI-OCCUR-16-INDX = 16 - (8 *                      
033493                  (SENASTE-TIAAAAP-AAAA - WS-INLAST-TIAAAA) +             
033494                  (SENASTE-TIAAP-PERIOD - PER-IX) )                       
033495           END-COMPUTE                                                    
033496           ADD WS-KVOI-TOT   TO WS-KVOI-OCCUR-16                          
033497                                (WS-KVOI-OCCUR-16-INDX)                   
033498                                                                          
033499           ADD +1            TO PER-IX                                    
033500                                                                          
033510         END-PERFORM                                                      
033520       END-IF                                                             
033530                                                                          
033540     END-IF                                                               
033600                                                                          
036400       MOVE +9999999 TO Y-MIN                                             
036500       MOVE ZERO TO Y-MAX                                                 
036600       MOVE ZERO TO WS-KVOI-OCCUR-16-INDX                                 
036700       PERFORM UNTIL                                                      
036800        NOT ( WS-KVOI-OCCUR-16-INDX < 16 )                                
036900         ADD +1 TO WS-KVOI-OCCUR-16-INDX                                  
037000         IF  WS-KVOI-OCCUR-16 (WS-KVOI-OCCUR-16-INDX) < Y-MIN             
037100           MOVE WS-KVOI-OCCUR-16 (WS-KVOI-OCCUR-16-INDX)                  
037200                                 TO Y-MIN                                 
037300         END-IF                                                           
037400         IF  WS-KVOI-OCCUR-16 (WS-KVOI-OCCUR-16-INDX) > Y-MAX             
037500           MOVE WS-KVOI-OCCUR-16 (WS-KVOI-OCCUR-16-INDX)                  
037600                                 TO Y-MAX                                 
037700         END-IF                                                           
037800       END-PERFORM                                                        
037900       MOVE SPACE TO STATUS-WS                                            
038000     END-IF                                                               
038100     CONTINUE.                                                            
038200     EJECT                                                                
038300 UTLAEGG-AV-GRAF SECTION.                                                 
038400     IF Y-MAX > Y-MIN                                                     
038500       COMPUTE STEGLGD ROUNDED = (Y-MAX - Y-MIN) / 15                     
038600       IF STEGLGD > ZERO                                                  
038700         DIVIDE STEGLGD BY 2 GIVING HALV-STEGLGD ROUNDED                  
038800*** LÄGG UT X-AXEL ***************************************                
038900         MOVE SENASTE-TIAAP-AAR TO WS-TIAAP-AAR                           
039000         MOVE SENASTE-TIAAP-PERIOD TO WS-TIAAP-PERIOD                     
039100         MOVE 16 TO X-INDX                                                
039200         PERFORM UNTIL X-INDX <= ZERO                                     
039400           IF WS-TIAAP-PERIOD = ZERO                                      
039500             MOVE 8 TO WS-TIAAP-PERIOD                                    
039510             IF WS-TIAAP-AAR = 00                                         
039520               MOVE 99     TO WS-TIAAP-AAR                                
039530             ELSE                                                         
039600               SUBTRACT 1 FROM WS-TIAAP-AAR                               
039610             END-IF                                                       
039700           END-IF                                                         
039800           MOVE WS-TIAAP TO MOD-STAPELBIT-NUM (1, X-INDX)                 
039900           MOVE '-' TO MOD-STAPELSLUT (1, X-INDX)                         
040000           SUBTRACT 1 FROM X-INDX   WS-TIAAP-PERIOD                       
040100         END-PERFORM                                                      
040110                                                                          
040200         MOVE ZERO TO Y-INDX                                              
040300         PERFORM UNTIL Y-INDX >= 16                                       
040500           ADD 1 TO Y-INDX                                                
040600           COMPUTE MOD-YAXELVARDE (Y-INDX) =                              
040700           Y-MIN + STEGLGD * (Y-INDX - 1)                                 
040800           MOVE '!' TO MOD-YAXEL (Y-INDX)                                 
040900         END-PERFORM                                                      
040910                                                                          
041000         MOVE ZERO TO X-INDX                                              
041100         PERFORM UNTIL X-INDX >= 16                                       
041300           ADD 1 TO X-INDX                                                
041400           COMPUTE Y-NIVA = 1 +                                           
041500               (WS-KVOI-OCCUR-16 (X-INDX) + HALV-STEGLGD - Y-MIN)         
041600           /                                                              
041700               STEGLGD                                                    
041800           IF  Y-NIVA > +16                                               
041900             MOVE +16 TO Y-NIVA                                           
042000           END-IF                                                         
042100           MOVE Y-NIVA TO Y-INDX                                          
042200           IF  Y-NIVA NOT = 1                                             
042300           OR  WS-KVOI-OCCUR-16 (X-INDX) NOT = ZERO                       
042400             MOVE STAPEL-TKN TO MOD-STAPELBIT (Y-NIVA, X-INDX)            
042500           ELSE                                                           
042600             MOVE SPACE TO MOD-STAPELBIT (Y-NIVA, X-INDX)                 
042700           END-IF                                                         
042800           IF  X-INDX > 1                                                 
042900             SUBTRACT +1 FROM X-INDX GIVING WS-INDX                       
043000             PERFORM UNTIL                                                
043100              NOT ( Y-INDX < OLD-Y-NIVA - 1 )                             
043200               ADD +1 TO Y-INDX                                           
043300               MOVE STAPEL-LINJE TO MOD-STAPELSLUT                        
043400                                    (Y-INDX, WS-INDX)                     
043500             END-PERFORM                                                  
043600             PERFORM UNTIL                                                
043700              NOT ( Y-INDX > OLD-Y-NIVA + 1 )                             
043800               SUBTRACT +1 FROM Y-INDX                                    
043900               MOVE STAPEL-LINJE TO MOD-STAPELSLUT                        
044000                                    (Y-INDX, WS-INDX)                     
044100             END-PERFORM                                                  
044200           END-IF                                                         
044300           IF  X-INDX = 16                                                
044400             MOVE Y-NIVA TO Y-INDX                                        
044500             MOVE ZERO TO OLD-Y-NIVA                                      
044600             PERFORM UNTIL                                                
044700              NOT ( Y-INDX > OLD-Y-NIVA + 1 )                             
044800               SUBTRACT +1 FROM Y-INDX                                    
044900               MOVE STAPEL-LINJE TO MOD-STAPELSLUT                        
045000                                    (Y-INDX, X-INDX)                      
045100             END-PERFORM                                                  
045200           END-IF                                                         
045300           MOVE Y-NIVA TO OLD-Y-NIVA                                      
045400         END-PERFORM                                                      
045500       END-IF                                                             
045600     END-IF                                                               
045700     CONTINUE.                                                            
045800 X-SKAPA-DIAGRAM. EXIT.                                                   
045900     CONTINUE.                                                            
046000     EJECT                                                                
046100*****  IMS SEKTIONER                                                      
046200*                                                                         
046300 IMS-LAES-MID SECTION.                                                    
046400     MOVE '  QC' TO GODK-STATUSKODER                                      
046500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
046600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
046700     PERFORM IMS-STATUSKONTROLL                                           
046800     CONTINUE.                                                            
046900     SKIP3                                                                
047000 IMS-SKRIV-MOD SECTION.                                                   
047010     IF MSGI-IDLAND-SPR = 'GB'                                            
047020        MOVE 'N' TO MFS-KDHUVOMR                                          
047030     END-IF                                                               
047100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
047200     MOVE SPACE TO GODK-STATUSKODER                                       
047300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
047400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
047500     PERFORM IMS-STATUSKONTROLL                                           
047600     CONTINUE.                                                            
047700     SKIP3                                                                
047800 IMS-GET-ARTC01-ARTIKEL SECTION.                                          
047900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
048000          DELIMITED BY SIZE INTO SSA1                                     
048100     MOVE '  GE' TO GODK-STATUSKODER                                      
048200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
048300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
048400     PERFORM IMS-STATUSKONTROLL                                           
048500     CONTINUE.                                                            
048600     SKIP3                                                                
048700 IMS-GET-ARTC11 SECTION.                                                  
048800     MOVE 'WLARTC11' TO SSA1                                              
048900     MOVE '  GE' TO GODK-STATUSKODER                                      
049000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
049100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
049200     PERFORM IMS-STATUSKONTROLL                                           
049300     CONTINUE.                                                            
050400     SKIP3                                                                
050500 IMS-GET-OIGB01-ARTIKEL SECTION.                                          
050600     STRING 'WLOIGB01(IDARTNR  =' W-IDARTNR-X ')'                         
050700          DELIMITED BY SIZE INTO SSA1                                     
050800     MOVE '  GE' TO GODK-STATUSKODER                                      
050900     CALL CBLTDLI USING GU OIGB-PCB DLI-IO-AREA SSA1                      
051000     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
051100     PERFORM IMS-STATUSKONTROLL                                           
051200     CONTINUE.                                                            
051300     SKIP3                                                                
051400 IMS-GET-OIGB11-SEGMENT SECTION.                                          
051500     STRING 'WLOIGB11(TIAAAA   =' W-TIAAAA-X ')'                          
051600          DELIMITED BY SIZE INTO SSA1                                     
051800     MOVE '  GE' TO GODK-STATUSKODER                                      
051900     CALL CBLTDLI USING GNP OIGB-PCB DLI-IO-AREA SSA1                     
052000     MOVE OIGB-STATUS-CODE TO STATUS-WS                                   
052100     PERFORM IMS-STATUSKONTROLL                                           
052200     CONTINUE.                                                            
052300     SKIP3                                                                
052400 IMS-GET-BENA11 SECTION.                                                  
052500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
052600     DELIMITED BY SIZE INTO SSA1                                          
052700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
052800     DELIMITED BY SIZE INTO SSA2                                          
052900     MOVE '  ' TO GODK-STATUSKODER                                        
053000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
053100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
053200     PERFORM IMS-STATUSKONTROLL                                           
053300     CONTINUE.                                                            
053400     SKIP3                                                                
053500 IMS-STATUSKONTROLL SECTION.                                              
053600     SKIP2                                                                
053700     SET STATUS-IX TO 1                                                   
053800     SEARCH GODK-STATUS                                                   
053810       AT END                                                             
053820         CALL FELLOG                                                      
053900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
054000     END-SEARCH                                                           
054100     IF  STATUS-WS = 'GE' OR 'GB'                                         
054200       MOVE 'GE' TO STATUS-WS                                             
054300     ELSE                                                                 
054400       MOVE SPACE TO STATUS-WS                                            
054500     END-IF                                                               
054600     .                                                                    
054700     EJECT                                                                
054800*    -COPY WY2000P9                                                       

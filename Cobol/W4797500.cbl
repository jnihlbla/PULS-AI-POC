000100*COMPOPT CPARM=FASTSRT                                                    
000200 ID  DIVISION.                                                            
000400 PROGRAM-ID.    W4797500.                                                 
000500 AUTHOR.        SVANTE BJÖRKBERG.                                         
000600 DATE-WRITTEN.  AUG 1980.                                                 
000700                                                                          
001000*    FUNKTION:                                                            
001100*                                                                         
001200*        PROGRAMMET SKAPAR EN LISTA MED PACKUNDERLAG                      
001300*        FÖR ARKIVERING PÅ FICHE. DESSA ANVÄNDS SOM BESLUTS-              
001400*        UNDERLAG VID LEVERANS-ANMÄRKNINGAR.                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*                                                                         
001800*        U0016    - OM RETURKOD FRÅN SORT                                 
001900     EJECT                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*- - - - - - - - - - - - INFIL:                                           
002700*                        - -  FIL MED SELEKTERADE POSTER UR               
002800*                             KOLLIREGISTRET                              
002900     SELECT W47974                       ASSIGN TO W47975D1.              
003000     SKIP2                                                                
003100*- - - - - - - - - - - - LISTFIL CDC; PV:                                 
003200*                        - -  PACKUNDERLAG, HISTORIK - FICHE              
003300     SELECT W47975-003-LIST-DC11         ASSIGN TO W47975D4.              
003400     SKIP2                                                                
005500*- - - - - - - - - - - - SORTFIL:                                         
005600     SELECT SORTFIL                      ASSIGN TO W47975DS.              
005700     EJECT                                                                
005800 DATA DIVISION.                                                           
005900     SKIP2                                                                
006000 FILE SECTION.                                                            
006100     SKIP3                                                                
006200 FD  W47974                                                               
006300     LABEL RECORD   STANDARD                                              
006400     RECORDING      V                                                     
006500     BLOCK CONTAINS 0.                                                    
006600     SKIP2                                                                
006700*    -COPY W479A01      -L.                                               
006800     SKIP2                                                                
006900*    EJECT                                                                
007000 FD  W47975-003-LIST-DC11                                                 
007100     LABEL RECORD    STANDARD                                             
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS 0.                                                    
007400 01  LIST-PACKU-PV-DC11  PIC X(100).                                      
010611     EJECT                                                                
010620 SD  SORTFIL.                                                             
010700*01  POST   -COPY W479A01   -PRE SORT-                                    
010800     EJECT                                                                
010900 WORKING-STORAGE SECTION.                                                 
010901*    -- CHECKED BY WY2000                                                 
010910     SKIP3                                                                
011000 01  SIDA                         PIC 9(05) VALUE 0 COMP-3.               
011100 01  RAD                          PIC 9(03) VALUE 0 COMP-3.               
011200 01  NY-KUNDORDER                 PIC X     VALUE 'J'.                    
011300*- - - - - - - - - - - - - -  GENERERAT PROGRAM-NAMN                      
011400 77   PROGRAM-NAMN        PIC X(8)   VALUE 'W4797500'.                    
011500     SKIP2                                                                
011600*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
011700                                                                          
011800 77  JA                          PIC X(1)    VALUE 'J'.                   
011900 77  NEJ                         PIC X(1)    VALUE 'N'.                   
012000 77  FORSTA-RAD                  PIC X(1)    VALUE 'J'.                   
012100 77  FL-SKRIV-RUBRIK             PIC X(1)    VALUE 'N'.                   
012200 77  STRACK                      PIC X(1)    VALUE '-'.                   
012300     SKIP2                                                                
012400*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
012410*      --- VALID IDDC CODES                                               
012420*                                                                         
012430*01    -COPY WWDC99                                                       
012440       EJECT                                                              
012500                                                                          
012600 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
012700     SKIP2                                                                
012800*- - - - - - - - - - - - - -                                              
012900 01  KUNDORDER.                                                           
013000   03  SPAR-IDDC                 PIC X(2)  VALUE SPACE.                   
013100   03  SPAR-IDDISTR              PIC S9(5) VALUE 0 COMP-3.                
013200   03  SPAR-IDKUNDNR             PIC S9(7) VALUE 0 COMP-3.                
013300   03  SPAR-IDKUNDRF             PIC X(10).                               
013400                                                                          
013500   03  FILLER                    REDEFINES SPAR-IDKUNDRF.                 
013600     05  SPAR-IDORDNR            PIC 9(5).                                
013700     05  FILLER                  PIC X(5).                                
013800                                                                          
013900 01  SPAR-IDKUNDRF-RO            PIC X(10).                               
014000                                                                          
014100 01  FILLER                      REDEFINES SPAR-IDKUNDRF-RO.              
014200   03  SPAR-IDORDNR-RO           PIC 9(5).                                
014300   03  FILLER                    PIC X(5).                                
014400                                                                          
014500 01  SPAR-IDPRODNR               PIC S9(7) VALUE ZERO COMP-3.             
014600                                                                          
014700 01  SPAR-TIFAKT                 PIC S9(7)   COMP-3.                      
014800     EJECT                                                                
014900 01  ARB-IDKUNDRF.                                                        
015000   03  ARB-IDORDNR               PIC 9(05).                               
015100   03  FILLER                    PIC X(05).                               
015200                                                                          
015300 01  ARB-BEGMT.                                                           
015400   03  ARB-BEGMT-RAD1            PIC X(27).                               
015500   03  ARB-BEGMT-RAD2            PIC X(27).                               
015600                                                                          
015700 01  ARB-ADGMT.                                                           
015800   03  ARB-ADGMT-GATA            PIC X(27).                               
015900   03  ARB-ADGMT-PADR            PIC X(27).                               
015910   03  ARB-ADGMT-LAND            PIC X(27).                               
016000                                                                          
016100 01  ARB-BEGMRK.                                                          
016200   03  ARB-BEGMRK-DEL1           PIC X(30).                               
016300   03  ARB-BEGMRK-DEL2           PIC X(30).                               
016400   03  ARB-BEGMRK-DEL3           PIC X(34).                               
016500                                                                          
016600     EJECT                                                                
016700 01  DYNAMISKA-SUBPROGRAM.                                                
016800   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
016900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
017200     SKIP3                                                                
017300*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
017400                                                                          
017500 01  RETURKODER.                                                          
017600   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
017700   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
017800   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
017900     EJECT                                                                
018000*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
018100                                                                          
018200*01  -COPY W0005       -PRE  POSTSUM-.                                    
018700     EJECT                                                                
019200 01  WSORT-AREA                  PIC X(300).                              
019300     SKIP3                                                                
019400                                                                          
019500*01  AREA  -COPY W479A01   -PRE SRTA01-    -RED WSORT-AREA                
019600     EJECT                                                                
019700                                                                          
019800*01  AREA  -COPY W4797402  -PRE SRTA03-    -RED WSORT-AREA                
019900     EJECT                                                                
020000                                                                          
020100*01  AREA  -COPY W479A11   -PRE SRTA11-    -RED WSORT-AREA                
020200     EJECT                                                                
020300                                                                          
020400*01  AREA  -COPY W479A21   -PRE SRTA21-    -RED WSORT-AREA                
020500     EJECT                                                                
020600                                                                          
020700*01  AREA  -COPY W479A01   -PRE ARBA01-                                   
020800     EJECT                                                                
020900 01  FILLER                      PIC X(24)   VALUE                        
021000                                            'LIST-AREA-START   '.         
021100     SKIP3                                                                
021200*                                                                         
021300 01  PACKU-RUBRIK.                                                        
021400                                                                          
021500     03  PACKU-RUBRIK1.                                                   
021600                                                                          
021700         05  FILLER               PIC X(02)   VALUE SPACE.                
021800         05  RUB1-BOLAG           PIC X(12).                              
021900         05  FILLER               PIC X(10)   VALUE 'DISTR'.              
022000         05  FILLER               PIC X(05)   VALUE 'KUND'.               
022100         05  FILLER               PIC X(03)   VALUE 'DC'.                 
022200         05  FILLER               PIC X(05)   VALUE 'FK'.                 
022300         05  FILLER               PIC X(10)   VALUE 'ORDER'.              
022400         05  FILLER               PIC X(09)   VALUE 'PRODNR'.             
022500         05  FILLER               PIC X(22)   VALUE                       
022600                                              'GODSMOTTAGARE:'.           
022700         05  RUB1-TIFAKT          PIC Z(5)9.                              
022800     SKIP2                                                                
022900     03  PACKU-RUBRIK2.                                                   
023000         05  FILLER               PIC X(02)   VALUE SPACE.                
023100         05  RUB2-LISTNR          PIC X(10).                              
023200                                                                          
023300         05  FILLER               PIC X(02)   VALUE SPACE.                
023400         05  RUB2-IDDISTR         PIC Z(4)9.                              
023500         05  FILLER               PIC X(02)   VALUE SPACE.                
023600         05  RUB2-IDKUNDNR        PIC Z(6)9.                              
023700         05  FILLER               PIC X(01)   VALUE SPACE.                
023800         05  RUB2-IDDC            PIC X(2).                               
023900         05  FILLER               PIC X(01)   VALUE SPACE.                
024000         05  RUB2-KDFRAKT         PIC Z(1)9.                              
024100         05  FILLER               PIC X(03)   VALUE SPACE.                
024200         05  RUB2-IDORDNR         PIC Z(4)9.                              
024300         05  FILLER               PIC X(04)   VALUE SPACE.                
024400         05  RUB2-IDPRODNR        PIC Z(06)9.                             
024500         05  FILLER               PIC X(03)   VALUE SPACE.                
024600         05  RUB2-BEGMT-RAD1      PIC X(35).                              
024700     EJECT                                                                
024800     03  PACKU-RUBRIK3.                                                   
024900                                                                          
025000         05  FILLER               PIC X(56)   VALUE SPACE.                
025100         05  RUB3-BEGMT-RAD2      PIC X(35).                              
025200     SKIP2                                                                
025300     03  PACKU-RUBRIK4.                                                   
025400                                                                          
025500         05  FILLER               PIC X(14)   VALUE SPACE.                
025600         05  FILLER               PIC X(12)   VALUE 'VÅR REF.'.           
025700         05  FILLER               PIC X(08)   VALUE 'REGDAT'.             
025800         05  FILLER               PIC X(04)   VALUE 'KL'.                 
025900         05  FILLER               PIC X(18)   VALUE 'BEGPAC'.             
026000         05  RUB4-ADGMT-GATA      PIC X(35).                              
026100     SKIP2                                                                
026200     03  PACKU-RUBRIK5.                                                   
026300                                                                          
026400         05  FILLER               PIC X(13)   VALUE SPACE.                
026500         05  RUB5-BEVARREF        PIC X(13).                              
026600         05  RUB5-TIORDREG        PIC Z(5)9.                              
026700         05  FILLER               PIC X(03)   VALUE SPACE.                
026800         05  RUB5-KDORDKL         PIC 9(01).                              
026900         05  FILLER               PIC X(02)   VALUE SPACE.                
027000         05  RUB5-TIBEGPAC        PIC Z(5)9.                              
027100         05  FILLER               PIC X(12)   VALUE SPACE.                
027200         05  RUB5-ADGMT-PADR      PIC X(35).                              
027210     SKIP2                                                                
027220     03  PACKU-RUBRIK6.                                                   
027230                                                                          
027240         05  FILLER               PIC X(56)   VALUE SPACE.                
027293         05  RUB6-ADGMT-LAND      PIC X(35).                              
027300     EJECT                                                                
027400 01  RUBRIK-KUNDORDER.                                                    
027500     03  PACKU-RUBRIK7.                                                   
027600                                                                          
027700         05  FILLER               PIC X(02)   VALUE SPACE.                
027800         05  FILLER               PIC X(09)   VALUE 'PERSON'.             
027900         05  FILLER               PIC X(09)   VALUE 'VAGNNR'.             
028000         05  FILLER               PIC X(12)   VALUE 'NETTOVIKT'.          
028100         05  FILLER               PIC X(11)   VALUE 'NETTOVOL'.           
028200         05  FILLER               PIC X(09)   VALUE 'ANTRAD'.             
028300         05  FILLER               PIC X(08)   VALUE 'REG.ANSV'.           
028400     SKIP2                                                                
028500     03  PACKU-RUBRIK8.                                                   
028600                                                                          
028700         05  FILLER               PIC X(03)   VALUE SPACE.                
028800         05  RUB8-KDPERSON        PIC Z(2)9.                              
028900         05  FILLER               PIC X(04)   VALUE SPACE.                
029000         05  RUB8-KDORDLOT        PIC X(02).                              
029100         05  RUB8-STRACK          PIC X(01).                              
029200         05  RUB8-IDLOTNR         PIC 9(03).                              
029300         05  FILLER               PIC X(04)   VALUE SPACE.                
029400         05  RUB8-VKORDNTO        PIC Z(5)9.9.                            
029500         05  FILLER               PIC X(04)   VALUE SPACE.                
029600         05  RUB8-VLORDNTO        PIC Z(3)9.999.                          
029700         05  FILLER               PIC X(03)   VALUE SPACE.                
029800         05  RUB8-KVORDRAD        PIC Z(4)9.                              
029900         05  FILLER               PIC X(04)   VALUE SPACE.                
030000         05  RUB8-IDUSER          PIC X(08).                              
030100         SKIP2                                                            
030200     03  PACKU-RUBRIK10.                                                  
030300                                                                          
030400         05  FILLER               PIC X(02)   VALUE SPACE.                
030500         05  FILLER               PIC X(19)   VALUE                       
030600                                              'GODSMÄRKNING:'.            
030700         05  RUB10-BEGMRK-DEL1    PIC X(30).                              
030800     EJECT                                                                
030900     03  PACKU-RUBRIK11.                                                  
031000                                                                          
031100         05  FILLER               PIC X(21)   VALUE SPACE.                
031200         05  RUB11-BEGMRK-DEL2    PIC X(30).                              
031300     SKIP2                                                                
031400     03  PACKU-RUBRIK12.                                                  
031500                                                                          
031600         05  FILLER               PIC X(21)   VALUE SPACE.                
031700         05  RUB12-BEGMRK-DEL3    PIC X(34).                              
031800     SKIP2                                                                
031900     03  PACKU-RUBRIK14-DEL1.                                             
032000                                                                          
032100         05  FILLER               PIC X(02)   VALUE SPACE.                
032200         05  FILLER               PIC X(19)   VALUE                       
032300                                              'LAGERINSTRUKTION:'.        
032400         05  RUB14-BELAGINS-DEL1  PIC X(60).                              
032410     SKIP2                                                                
032420     03  PACKU-RUBRIK14-DEL2.                                             
032430                                                                          
032440         05  FILLER               PIC X(21)   VALUE SPACE.                
032470         05  RUB14-BELAGINS-DEL2  PIC X(60).                              
032500     EJECT                                                                
032600 01  PACKU-DETALJ1.                                                       
032700     03  FILLER                   PIC X       VALUE SPACE.                
032800     03  FILLER                   PIC X(08)   VALUE 'KOLLI'.              
032900     03  FILLER                   PIC X(10)   VALUE SPACE.                
033000     03  FILLER                   PIC X(07)   VALUE 'PACDAT'.             
033100     03  FILLER                   PIC X(07)   VALUE 'FAKDAT'.             
033200     03  FILLER                   PIC X(08)   VALUE 'LASTDAT'.            
033300     03  FILLER                   PIC X(08)   VALUE 'ANTRAD'.             
033400     03  FILLER                   PIC X(10)   VALUE 'BTOVIKT'.            
033500     03  FILLER                   PIC X(07)   VALUE 'BTOVOL'.             
033600     03  FILLER                   PIC X(08)   VALUE 'PACKARE'.            
033700     03  FILLER                   PIC X(08)   VALUE 'KOLLIKOD'.           
033800     SKIP2                                                                
033900 01  PACKU-DETALJ2.                                                       
034000     03  FILLER                   PIC X       VALUE SPACE.                
034100     03  DET2-IDKOLLI             PIC Z(4)9.                              
034200     03  FILLER                   PIC X       VALUE SPACE.                
034300     03  DET2-ADFLGEO             PIC X(04).                              
034400     03  DET2-ADFLOMR             PIC Z(2)9.                              
034500     03  FILLER                   PIC X       VALUE SPACE.                
034600     03  DET2-ADRUTNIV            PIC Z(2)9.                              
034700     03  FILLER                   PIC X       VALUE SPACE.                
034800     03  DET2-TIPACKN             PIC Z(5)9.                              
034900     03  FILLER                   PIC X       VALUE SPACE.                
035000     03  DET2-TIFAKT              PIC Z(5)9.                              
035100     03  FILLER                   PIC X(02)   VALUE SPACE.                
035200     03  DET2-TILASTN             PIC Z(5)9.                              
035300     03  FILLER                   PIC X       VALUE SPACE.                
035400     03  DET2-KVORDRAD            PIC Z(4)9.                              
035500     03  FILLER                   PIC X(02)   VALUE SPACE.                
035600     03  DET2-VKORDBTO-KOLLI      PIC Z(5)9.9.                            
035700     03  FILLER                   PIC X       VALUE SPACE.                
035800     03  DET2-VLORDBTO-KOLLI      PIC Z(3)9.999.                          
035900     03  FILLER                   PIC X       VALUE SPACE.                
036000     03  DET2-IDPLOCK             PIC Z(6)9.                              
036100     03  FILLER                   PIC X(02)   VALUE SPACE.                
036200     03  DET2-KDKOLLI             PIC X(08).                              
036300     EJECT                                                                
036400 01  PACKU-DETALJ3.                                                       
036500     03  FILLER                   PIC X(02)   VALUE SPACE.                
036600     03  FILLER                   PIC X(05)   VALUE 'RONR'.               
036700     03  FILLER                   PIC X(13)   VALUE SPACE.                
036800     03  FILLER                   PIC X(07)   VALUE 'RADNR'.              
036900     03  FILLER                   PIC X(11)   VALUE 'ARTIKELNR'.          
037000     03  FILLER                   PIC X(03)   VALUE 'UR'.                 
037100     03  FILLER                   PIC X(20)   VALUE 'BENÄMNING'.          
037200     03  FILLER                   PIC X(08)   VALUE 'BES ANT'.            
037300     03  FILLER                   PIC X(09)   VALUE 'LEV ANT'.            
037400     03  FILLER                   PIC X(05)   VALUE 'KOLLI'.              
037500     SKIP2                                                                
037600 01  PACKU-DETALJ4.                                                       
037700     03  FILLER                   PIC X       VALUE SPACE.                
037800     03  DET4-IDORDNR-RO          PIC Z(05).                              
037900     03  FILLER                   PIC X(13)   VALUE SPACE.                
038000     03  DET4-IDPURAD             PIC Z(4)9.                              
038100     03  FILLER                   PIC X(02)   VALUE SPACE.                
038200     03  DET4-IDARTNR             PIC Z(8)9.                              
038300     03  DET4-STRACK              PIC X(01).                              
038400     03  DET4-REKSIFFR            PIC 9(01).                              
038500     03  FILLER                   PIC X       VALUE SPACE.                
038600     03  DET4-KDARTURS            PIC X(2).                               
038700     03  FILLER                   PIC X       VALUE SPACE.                
038800     03  DET4-BEART-SVE           PIC X(19).                              
038900     03  DET4-KVBEART             PIC Z(06)9.                             
039000     03  FILLER                   PIC X       VALUE SPACE.                
039100     03  DET4-KVLEVART            PIC Z(6)9.                              
039200     03  FILLER                   PIC X       VALUE SPACE.                
039300     03  DET4-IDKOLLI             PIC Z(05).                              
039400     03  FILLER                   PIC X(03)   VALUE SPACE.                
039500     03  DET4-KVANNANT            PIC X.                                  
039600     EJECT                                                                
039700 PROCEDURE DIVISION.                                                      
039800     SKIP2                                                                
039900     PERFORM A-INIT                                                       
040000                                                                          
040100     SORT SORTFIL ASCENDING SORT-IDDC                                     
040200                            SORT-IDDISTR                                  
040300                            SORT-IDKUNDNR                                 
040400                            SORT-IDKUNDRF                                 
040500                            SORT-IDPRODNR                                 
040600                            SORT-IDPTYP                                   
040800                            SORT-IDKOLLI                                  
040900                            SORT-IDARTNR                                  
041000          USING   W47974                                                  
041100          OUTPUT PROCEDURE B-BEARBETNING                                  
041200     SKIP2                                                                
041300     IF SORT-RETURN > ZERO                                                
041400        DISPLAY '***  W4797500  - FEL VID SORTERING'                      
041500        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
041600     ELSE                                                                 
041700                                                                          
041800        PERFORM Z-FINIT                                                   
041900        MOVE ZERO TO RETURN-CODE                                          
042000        GOBACK                                                            
042100                                                                          
042200     END-IF                                                               
042300     .                                                                    
042400     EJECT                                                                
042500 A-INIT SECTION.                                                          
042600     SKIP2                                                                
042700     OPEN OUTPUT W47975-003-LIST-DC11                                     
043590                                                                          
043600     MOVE ZERO         TO SPAR-IDKUNDRF                                   
043700     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
043800     .                                                                    
043900     EJECT                                                                
044000 B-BEARBETNING SECTION.                                                   
044100     SKIP2                                                                
044200     PERFORM S01-LAS-SORTERAD-W47974                                      
044300                                                                          
044400     PERFORM UNTIL SORTFIL-EOF = JA                                       
044500                                                                          
044600       EVALUATE SORT-IDPTYP                                               
044700         WHEN  'A01'                                                      
044800           PERFORM BA-IDPTYP-A01                                          
044900         WHEN  'A03'                                                      
045000           PERFORM BB-IDPTYP-A03                                          
045100         WHEN  'A11'                                                      
045200           PERFORM BC-IDPTYP-A11                                          
045300         WHEN  'A21'                                                      
045400           PERFORM BD-IDPTYP-A21                                          
045500       END-EVALUATE                                                       
045600                                                                          
045700       PERFORM S01-LAS-SORTERAD-W47974                                    
045800     END-PERFORM                                                          
045900     .                                                                    
046000     EJECT                                                                
046100 BA-IDPTYP-A01      SECTION.                                              
046200                                                                          
046300     IF (SRTA01-IDDC     NOT = SPAR-IDDC)                                 
046400     OR (SRTA01-IDDISTR  NOT = SPAR-IDDISTR)                              
046500     OR (SRTA01-IDKUNDNR NOT = SPAR-IDKUNDNR)                             
046600     OR (SRTA01-IDKUNDRF NOT = SPAR-IDKUNDRF)                             
046700     OR (SRTA01-IDPRODNR NOT = SPAR-IDPRODNR)                             
046800        MOVE 'J' TO NY-KUNDORDER                                          
046900     END-IF                                                               
047000                                                                          
047100     MOVE SRTA01-W479A01    TO ARBA01-W479A01                             
047200     MOVE ARBA01-BEGMT      TO ARB-BEGMT                                  
047300     MOVE ARBA01-ADGMT      TO ARB-ADGMT                                  
047400     MOVE ARBA01-BEGMRK     TO ARB-BEGMRK                                 
047500     MOVE SRTA01-IDDC       TO SPAR-IDDC                                  
047600     MOVE SRTA01-IDDISTR    TO SPAR-IDDISTR                               
047900                                                                          
048000     MOVE SRTA01-IDKUNDNR   TO SPAR-IDKUNDNR                              
048100     MOVE SRTA01-IDKUNDRF   TO SPAR-IDKUNDRF                              
048200     MOVE SRTA01-IDPRODNR   TO SPAR-IDPRODNR                              
048300     MOVE JA                TO FORSTA-RAD                                 
048400     .                                                                    
048500     EJECT                                                                
048600 BB-IDPTYP-A03      SECTION.                                              
048700                                                                          
048800     MOVE SRTA03-TIFAKT     TO SPAR-TIFAKT                                
048900                                                                          
049000     IF NY-KUNDORDER = 'J'                                                
049100        MOVE 'N' TO NY-KUNDORDER                                          
049200        MOVE 14  TO RAD                                                   
049300        PERFORM S01-RUBRIK-UTSKRIFT                                       
049400        PERFORM S02-RUBRIK-UTSKRIFT                                       
049500     END-IF                                                               
049600                                                                          
049700     ADD 3 TO RAD                                                         
049800     MOVE NEJ TO FL-SKRIV-RUBRIK                                          
049900     MOVE SORT-IDDC TO WS-IDDC                                            
050010     EVALUATE TRUE                                                        
050100      WHEN CDC-SE                                                         
050110           WRITE LIST-PACKU-PV-DC11 FROM PACKU-DETALJ1 AFTER 3            
050600     END-EVALUATE                                                         
050800     .                                                                    
050900     EJECT                                                                
051000 BC-IDPTYP-A11      SECTION.                                              
051100                                                                          
051200     IF RAD > 56                                                          
051300        MOVE 5 TO RAD                                                     
051400        PERFORM S01-RUBRIK-UTSKRIFT                                       
051500     END-IF                                                               
051600                                                                          
051610     MOVE SORT-IDDC        TO WS-IDDC                                     
051700     IF FL-SKRIV-RUBRIK = JA                                              
051800        ADD 3 TO RAD                                                      
052010      EVALUATE TRUE                                                       
052020       WHEN CDC-SE                                                        
052100          WRITE LIST-PACKU-PV-DC11 FROM PACKU-DETALJ1 AFTER 3             
052700      END-EVALUATE                                                        
052800        MOVE NEJ TO FL-SKRIV-RUBRIK                                       
052900     END-IF                                                               
053000                                                                          
053100     ADD 1 TO RAD                                                         
053200     MOVE SRTA11-IDKOLLI          TO  DET2-IDKOLLI                        
053300     MOVE SRTA11-ADFLGEO          TO  DET2-ADFLGEO                        
053400     MOVE SRTA11-ADFLOMR          TO  DET2-ADFLOMR                        
053500     MOVE SRTA11-ADRUTNIV         TO  DET2-ADRUTNIV                       
053600     MOVE SRTA11-TIPACKN          TO  DET2-TIPACKN                        
053700     MOVE SRTA11-TIFAKT           TO  DET2-TIFAKT                         
053800     MOVE SRTA11-TILASTN          TO  DET2-TILASTN                        
053900     MOVE SRTA11-KVORDRAD         TO  DET2-KVORDRAD                       
054000     MOVE SRTA11-VKORDBTO-KOLLI   TO  DET2-VKORDBTO-KOLLI                 
054100     MOVE SRTA11-VLORDBTO-KOLLI   TO  DET2-VLORDBTO-KOLLI                 
054200     MOVE SRTA11-IDPLOCK          TO  DET2-IDPLOCK                        
054300     MOVE SRTA11-KDKOLLI          TO  DET2-KDKOLLI                        
054510     EVALUATE TRUE                                                        
054520      WHEN CDC-SE                                                         
054600       WRITE LIST-PACKU-PV-DC11 FROM PACKU-DETALJ2 AFTER 1                
055100     END-EVALUATE                                                         
055300     .                                                                    
055400     EJECT                                                                
055500 BD-IDPTYP-A21      SECTION.                                              
055600                                                                          
055610     MOVE SORT-IDDC       TO WS-IDDC                                      
055700     EVALUATE TRUE                                                        
055800       WHEN FORSTA-RAD = JA                                               
055900         ADD 3 TO RAD                                                     
055910         EVALUATE TRUE                                                    
055920          WHEN CDC-SE                                                     
056200             WRITE LIST-PACKU-PV-DC11 FROM PACKU-DETALJ3 AFTER 3          
056820          END-EVALUATE                                                    
056900         MOVE NEJ TO FORSTA-RAD                                           
057000                                                                          
057100       WHEN RAD > 56                                                      
057200         MOVE 8 TO RAD                                                    
057300         PERFORM S01-RUBRIK-UTSKRIFT                                      
057510         EVALUATE TRUE                                                    
057520          WHEN CDC-SE                                                     
057600             WRITE LIST-PACKU-PV-DC11 FROM PACKU-DETALJ3 AFTER 2          
058100          END-EVALUATE                                                    
058300         MOVE NEJ TO FL-SKRIV-RUBRIK                                      
058400                                                                          
058500       WHEN FL-SKRIV-RUBRIK = JA                                          
058600         ADD 3 TO RAD                                                     
058610         EVALUATE TRUE                                                    
058620          WHEN CDC-SE                                                     
058900             WRITE LIST-PACKU-PV-DC11 FROM PACKU-DETALJ3 AFTER 3          
059400          END-EVALUATE                                                    
059600         MOVE NEJ TO FL-SKRIV-RUBRIK                                      
059700     END-EVALUATE                                                         
059800                                                                          
059900     MOVE SRTA21-IDKUNDRF-RO             TO SPAR-IDKUNDRF-RO              
060000                                                                          
060100     MOVE SPAR-IDORDNR-RO                TO DET4-IDORDNR-RO               
060200     MOVE SRTA21-IDPURAD                 TO DET4-IDPURAD                  
060300     MOVE SRTA21-IDARTNR                 TO DET4-IDARTNR                  
060400     MOVE STRACK                         TO DET4-STRACK                   
060500     MOVE SRTA21-REKSIFFR                TO DET4-REKSIFFR                 
060600     MOVE SRTA21-KDARTURS                TO DET4-KDARTURS                 
060700     MOVE SRTA21-BEART-SVE               TO DET4-BEART-SVE                
060800     MOVE SRTA21-KVBEART                 TO DET4-KVBEART                  
060900     MOVE SRTA21-KVLEVART                TO DET4-KVLEVART                 
061000     MOVE SRTA21-IDKOLLI                 TO DET4-IDKOLLI                  
061100     IF SRTA21-KVANNANT > 0                                               
061200        MOVE '*'   TO DET4-KVANNANT                                       
061300     ELSE                                                                 
061400        MOVE SPACE TO DET4-KVANNANT                                       
061500     END-IF                                                               
061600     ADD 1 TO RAD                                                         
061610     EVALUATE TRUE                                                        
061620      WHEN CDC-SE                                                         
061900       WRITE LIST-PACKU-PV-DC11 FROM PACKU-DETALJ4 AFTER 1                
062400     END-EVALUATE                                                         
062600     .                                                                    
062700     EJECT                                                                
062800 S01-RUBRIK-UTSKRIFT SECTION.                                             
062900                                                                          
063000       MOVE JA  TO FL-SKRIV-RUBRIK                                        
063100       MOVE SPAR-TIFAKT           TO RUB1-TIFAKT                          
063301       MOVE 'CAR PARTS  ' TO RUB1-BOLAG                                   
063302       MOVE SORT-IDDC     TO WS-IDDC                                      
063310       EVALUATE TRUE                                                      
063320         WHEN CDC-SE                                                      
063500           WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK1 AFTER PAGE         
064100       END-EVALUATE                                                       
064300                                                                          
064400       MOVE ARBA01-IDDISTR        TO RUB2-IDDISTR                         
064500       MOVE ARBA01-IDKUNDNR       TO RUB2-IDKUNDNR                        
064600       MOVE ARBA01-IDDC           TO RUB2-IDDC                            
064700       MOVE ARBA01-KDFRAKT        TO RUB2-KDFRAKT                         
064800       MOVE SPAR-IDORDNR          TO RUB2-IDORDNR                         
064900       MOVE ARB-BEGMT-RAD1        TO RUB2-BEGMT-RAD1                      
065000       MOVE ARBA01-IDPRODNR       TO RUB2-IDPRODNR                        
065210        EVALUATE TRUE                                                     
065220         WHEN CDC-SE                                                      
065300            MOVE 'W47975-003'     TO RUB2-LISTNR                          
065400            WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK2 AFTER 1           
066000         END-EVALUATE                                                     
066200                                                                          
066300       MOVE ARB-BEGMT-RAD2        TO RUB3-BEGMT-RAD2                      
066310        EVALUATE TRUE                                                     
066320          WHEN CDC-SE                                                     
066600            WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK3 AFTER 1           
067210         END-EVALUATE                                                     
067300                                                                          
067400       MOVE ARB-ADGMT-GATA        TO RUB4-ADGMT-GATA                      
067610        EVALUATE TRUE                                                     
067620          WHEN CDC-SE                                                     
067700            WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK4 AFTER 1           
068200         END-EVALUATE                                                     
068400                                                                          
068500       MOVE ARBA01-BEVARREF       TO RUB5-BEVARREF                        
068600       MOVE ARBA01-TIORDREG       TO RUB5-TIORDREG                        
068700       MOVE ARBA01-KDORDKL        TO RUB5-KDORDKL                         
068800       MOVE ARBA01-TIBEGPAC       TO RUB5-TIBEGPAC                        
068900       MOVE ARB-ADGMT-PADR        TO RUB5-ADGMT-PADR                      
069110        EVALUATE TRUE                                                     
069120          WHEN CDC-SE                                                     
069200            WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK5 AFTER 1           
069700         END-EVALUATE                                                     
069710                                                                          
069800       MOVE ARB-ADGMT-LAND        TO RUB6-ADGMT-LAND                      
069810        EVALUATE TRUE                                                     
069820          WHEN CDC-SE                                                     
069830            WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK6 AFTER 1           
069897         END-EVALUATE                                                     
069900     .                                                                    
070000     EJECT                                                                
070100 S02-RUBRIK-UTSKRIFT SECTION.                                             
070200                                                                          
070300        MOVE SORT-IDDC           TO WS-IDDC                               
070310        EVALUATE TRUE                                                     
070320         WHEN CDC-SE                                                      
070500            WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK7 AFTER 2           
071000         END-EVALUATE                                                     
071200                                                                          
071300       MOVE ARBA01-KDPERSON       TO RUB8-KDPERSON                        
071400       MOVE ARBA01-KDORDLOT       TO RUB8-KDORDLOT                        
071500       MOVE STRACK                TO RUB8-STRACK                          
071600       MOVE ARBA01-IDLOTNR        TO RUB8-IDLOTNR                         
071700       MOVE ARBA01-VKORDNTO       TO RUB8-VKORDNTO                        
071800       MOVE ARBA01-VLORDNTO       TO RUB8-VLORDNTO                        
071900       MOVE ARBA01-KVORDRAD       TO RUB8-KVORDRAD                        
072000       MOVE ARBA01-IDUSER         TO RUB8-IDUSER                          
072210        EVALUATE TRUE                                                     
072220          WHEN CDC-SE                                                     
072300            WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK8 AFTER 1           
072800         END-EVALUATE                                                     
073000                                                                          
073100       MOVE ARB-BEGMRK-DEL1       TO RUB10-BEGMRK-DEL1                    
073310        EVALUATE TRUE                                                     
073320          WHEN CDC-SE                                                     
073400            WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK10 AFTER 2          
073900         END-EVALUATE                                                     
074100                                                                          
074200       MOVE ARB-BEGMRK-DEL2       TO RUB11-BEGMRK-DEL2                    
074410        EVALUATE TRUE                                                     
074420          WHEN CDC-SE                                                     
074500            WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK11 AFTER 1          
075000         END-EVALUATE                                                     
075200                                                                          
075300       MOVE ARB-BEGMRK-DEL3       TO RUB12-BEGMRK-DEL3                    
075510        EVALUATE TRUE                                                     
075520          WHEN CDC-SE                                                     
075600            WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK12 AFTER 1          
076100         END-EVALUATE                                                     
076300                                                                          
076400      MOVE ARBA01-BELAGINS-DEL1   TO RUB14-BELAGINS-DEL1                  
076500      MOVE ARBA01-BELAGINS-DEL2   TO RUB14-BELAGINS-DEL2                  
076610      EVALUATE TRUE                                                       
076620      WHEN CDC-SE                                                         
076700        WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK14-DEL1 AFTER 2         
076710        WRITE LIST-PACKU-PV-DC11 FROM PACKU-RUBRIK14-DEL2 AFTER 1         
077200      END-EVALUATE                                                        
077400     .                                                                    
077500     EJECT                                                                
077600 S01-LAS-SORTERAD-W47974 SECTION.                                         
077700     SKIP2                                                                
077800     RETURN SORTFIL   INTO WSORT-AREA                                     
077900                      AT END MOVE JA TO SORTFIL-EOF                       
078000     END-RETURN                                                           
078100                                                                          
078200     IF SORTFIL-EOF = NEJ                                                 
078300                                                                          
078400        MOVE 'W47974'            TO POSTSUM-FDNAMN                        
078500        MOVE 'W47974D1'          TO POSTSUM-DDNAMN2                       
078600        MOVE SRTA01-IDPTYP       TO POSTSUM-TRANSTYP                      
078700        CALL POSTSUM   USING POSTSUM-PARM                                 
078800                                                                          
078900     END-IF                                                               
079000     .                                                                    
079100     EJECT                                                                
079200 Z-FINIT SECTION.                                                         
079300     SKIP2                                                                
079400                                                                          
080100     CLOSE W47975-003-LIST-DC11                                           
080300     SKIP2                                                                
080400                                                                          
080500     MOVE 'S' TO POSTSUM-OPKOD                                            
080600     CALL POSTSUM USING POSTSUM-PARM                                      
080700     .                                                                    

000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4611000.                                                 
001000*AUTHOR.        N. N.                                                     
001100*DATE-WRITTEN.  NOV 1984.                                                 
001200*REMARKS.                                                                 
001300                                                                          
001700*    FUNKTION:                                                            
001800                                                                          
001900*        FILER FRÅN FAKTURERING,ORDERBEKRÄFTELSE OCH BIPACKNING           
002000*        INNEHÅLLER POSTER SOM SKALL KOMPLETTERAS FRÅN                    
002100*        URSPRUNGSBASEN (WDA5).                                           
002200                                                                          
002300*        POSTER SOM SKALL KOMPLETTERAS HAR RONR EJ = NOLL I               
002400*        SORTDELEN.DESSA POSTER LÄMNAS TILL SORTEN,ÖVRIGA POSTER          
002500*        DIREKT TILL UTFIL.                                               
002600                                                                          
002700*        EFTER SORTEN KOMPLETTERAS POSTERNA MED DEALERREFERENS            
002800*        FRÅN URSPRUNGSBAS (WDA5) OCH SKRIV PÅ W46111 FIL.                
002900                                                                          
003000*    ABENDKODER:                                                          
003100                                                                          
003200*        U0016    - OM RETURKOD FRÅN SORT                                 
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003510 CONFIGURATION SECTION.                                                   
003520 SPECIAL-NAMES.                                                           
003530     ALPHABET Y2000 IS X'05' THRU X'09' X'00' THRU X'04'.                 
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     SKIP2                                                                
004000*- - - - - - - - - - - - INFIL:                                           
004100*                        - -  FAKTURAPOSTER                               
004200*                        - -  BIPACKNING                                  
004300*                        - -  ORDERBEKRÄFTELSER                           
004400*                        - -  AVSTÄMNINGS POST(ON-ORDER SALDO)            
004500     SELECT W46110-IN                    ASSIGN TO UT-S-W46110D1.         
004600     SKIP2                                                                
004700*- - - - - - - - - - - - UTFIL:                                           
004800*                        - -  W46111 NUMRERAD  FIL                        
004900     SELECT W46111-UT                    ASSIGN TO UT-S-W46110D2.         
005000     SKIP2                                                                
005100*- - - - - - - - - - - - SORTFIL:                                         
005200     SELECT SORTFIL                      ASSIGN TO UT-S-W46110DS.         
005300     EJECT                                                                
005400 DATA DIVISION.                                                           
005500     SKIP2                                                                
005600 FILE SECTION.                                                            
005700     SKIP3                                                                
005800 FD  W46110-IN                                                            
005900     RECORDING      V                                                     
006000     BLOCK CONTAINS 0.                                                    
006100     SKIP2                                                                
006200 01  IN-POST10.                                                           
006300*02  AREA10  -COPY W461S010   -PRE IN-.                                   
006400     EJECT                                                                
006500 01  IN-POST11.                                                           
006600*02  AREA11 -COPY W461S011    -L.                                         
006700     SKIP2                                                                
006800 01  IN-POST12.                                                           
006900*02  AREA12 -COPY W461S012    -L.                                         
007000     SKIP2                                                                
007100 01  IN-POST13.                                                           
007200*02  AREA13 -COPY W461S013    -L.                                         
007300     SKIP2                                                                
007400 01  IN-POST15.                                                           
007500*02  AREA15 -COPY W461S015    -L.                                         
007600     SKIP2                                                                
007700 01  IN-POST16.                                                           
007800*02  AREA16 -COPY W461S016    -L.                                         
007900     EJECT                                                                
008000 01  IN-POST20.                                                           
008100*02  AREA20 -COPY W461S020    -L.                                         
008200     SKIP2                                                                
008300 01  IN-POST01.                                                           
008400*02  AREA01  -COPY W461S001     -L.                                       
008500     SKIP2                                                                
008600 01  IN-POST02.                                                           
008700*02  AREA02  -COPY W461S002    -L.                                        
008800     SKIP2                                                                
008900 01  IN-POST03.                                                           
009000*02  AREA03 -COPY W461S003    -L.                                         
009100     SKIP2                                                                
009200 01  IN-POST04.                                                           
009300*02  AREA4 -COPY W461S004    -L.                                          
009400     EJECT                                                                
009500 01  IN-POST05.                                                           
009600*02  AREA5 -COPY W461S005    -L.                                          
009700     SKIP2                                                                
009800 01  IN-POST06.                                                           
009900*02  AREA6 -COPY W461S006    -L.                                          
010000     SKIP2                                                                
010100 01  IN-POST07.                                                           
010200*02  AREA7 -COPY W461S007   -L.                                           
010300     SKIP2                                                                
010400 01  IN-POST08.                                                           
010500*02  AREA8 -COPY W461S008    -L                                           
010600     EJECT                                                                
010700 FD  W46111-UT                                                            
010800     RECORDING      V                                                     
010900     BLOCK CONTAINS 0.                                                    
011000     SKIP2                                                                
011100 01  UT-POST01.                                                           
011200*02  AREA1 -COPY W461S001   -PRE UT-.                                     
011300     EJECT                                                                
011400 01  UT-POST02.                                                           
011500*02  AREA2 -COPY W461S002   -PRE UT-.                                     
011600     EJECT                                                                
011700 01  UT-POST03.                                                           
011800*02  AREA3 -COPY W461S003   -PRE UT-.                                     
011900     EJECT                                                                
012000 01  UT-POST04.                                                           
012100*02  AREA4 -COPY W461S004   -PRE UT-.                                     
012200     EJECT                                                                
012300 01  UT-POST05.                                                           
012400*02  AREA5 -COPY W461S005   -PRE UT-.                                     
012500     EJECT                                                                
012600 01  UT-POST06.                                                           
012700*02  AREA6 -COPY W461S006   -PRE UT-.                                     
012800     EJECT                                                                
012900 01  UT-POST07.                                                           
013000*02  AREA7 -COPY W461S007   -PRE UT-.                                     
013100     EJECT                                                                
013200 01  UT-POST08.                                                           
013300*02  AREA8 -COPY W461S008   -PRE UT-.                                     
013400     EJECT                                                                
013500 01  UT-POST10.                                                           
013600*02  AREA10 -COPY W461S010   -PRE UT-.                                    
013700     EJECT                                                                
013800 01  UT-POST11.                                                           
013900*02  AREA11 -COPY W461S011   -PRE UT-.                                    
014000     EJECT                                                                
014100 01  UT-POST12.                                                           
014200*02  AREA12 -COPY W461S012   -PRE UT-.                                    
014300     EJECT                                                                
014400 01  UT-POST13.                                                           
014500*02  AREA13 -COPY W461S013   -PRE UT-.                                    
014600     EJECT                                                                
014700 01  UT-POST15.                                                           
014800*02  AREA15 -COPY W461S015   -PRE UT-.                                    
014900     EJECT                                                                
015000 01  UT-POST16.                                                           
015100*02  AREA16 -COPY W461S016   -PRE UT-.                                    
015200     EJECT                                                                
015300 01  UT-POST20.                                                           
015400*02  AREA20 -COPY W461S020   -PRE UT-.                                    
015500     EJECT                                                                
015600 SD  SORTFIL                                                              
015700                .                                                         
015800     SKIP2                                                                
015900 01  SORT-POST01.                                                         
016000*02  AREA1 -COPY W461S001   -PRE SORT-.                                   
016001*Y2K-SORT                                                                 
016010     04  FILLER REDEFINES SORT-BIP-W461S001.                              
016020       06  FILLER             PIC X(11).                                  
016030       06  SORT-DECADE        PIC X.                                      
016040       06  SORT-SMALL-DATE    PIC S9(5) COMP-3.                           
016100     EJECT                                                                
016200 01  SORT-POST02.                                                         
016300*02  AREA2 -COPY W461S002      -PRE SORT-.                                
016400     EJECT                                                                
016500 01  SORT-POST03.                                                         
016600*02  AREA3 -COPY W461S003    -L.                                          
016700     SKIP2                                                                
016800 01  SORT-POST04.                                                         
016900*02  AREA4 -COPY W461S004    -L.                                          
017000     SKIP2                                                                
017100 01  SORT-POST05.                                                         
017200*02  AREA5 -COPY W461S005    -L.                                          
017300     SKIP2                                                                
017400 01  SORT-POST06.                                                         
017500*02  AREA6 -COPY W461S006    -L.                                          
017600     SKIP2                                                                
017700 01  SORT-POST07.                                                         
017800*02  AREA7 -COPY W461S007    -L.                                          
017900     EJECT                                                                
018000 01  SORT-POST08.                                                         
018100*02  AREA8 -COPY W461S008    -L.                                          
018200     SKIP2                                                                
018300 01  SORT-POST10.                                                         
018400*02  AREA10 -COPY W461S010    -L.                                         
018500     SKIP2                                                                
018600 01  SORT-POST11.                                                         
018700*02  AREA11 -COPY W461S011    -L.                                         
018800     SKIP2                                                                
018900 01  SORT-POST12.                                                         
019000*02  AREA12 -COPY W461S012    -L.                                         
019100     SKIP2                                                                
019200 01  SORT-POST13.                                                         
019300*02  AREA13 -COPY W461S013    -L.                                         
019400     EJECT                                                                
019500 01  SORT-POST15.                                                         
019600*02  AREA15 -COPY W461S015    -L.                                         
019700     SKIP2                                                                
019800 01  SORT-POST16.                                                         
019900*02  AREA16 -COPY W461S016    -L.                                         
020000     SKIP2                                                                
020100 01  SORT-POST20.                                                         
020200*02  AREA20 -COPY W461S020    -L.                                         
020300     EJECT                                                                
020400 WORKING-STORAGE SECTION.                                                 
020500     SKIP2                                                                
020510                                                                          
020600*    -- CHECKED BY WY2000                                                 
021100 77  IDPGM                       PIC X(8)    VALUE 'W4611000'.            
021300     SKIP2                                                                
021400*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
021500                                                                          
021600 77  JA                          PIC X(1)    VALUE 'J'.                   
021700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
021800     SKIP2                                                                
021900*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
022000                                                                          
022100 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
022200 77  W46110-EOF                  PIC X(1)    VALUE 'N'.                   
022300*                                                                         
023400     SKIP2                                                                
023410*- - - - - - - - - - - - - -  SPAR-FÄLT                                   
023420                                                                          
023430 01  SPAR-IDDISTR                PIC S9(5)   COMP-3  VALUE ZERO.          
023431 01  SPAR-IDKUNDNR               PIC S9(7)   COMP-3  VALUE ZERO.          
023432 01  SPAR-IDORDNR                PIC S9(7)   COMP-3  VALUE ZERO.          
023450*                                                                         
023460     SKIP2                                                                
023500 01  DYNAMISKA-SUBPROGRAM.                                                
023600   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
023700   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
023710   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
023720   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
023900     SKIP3                                                                
024000*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
024100                                                                          
024200 01  RETURKODER.                                                          
024300   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
024400   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
024500   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
024600     EJECT                                                                
024700*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
024800                                                                          
024900*01  -COPY W0005       -PRE  POSTSUM-.                                    
025000     EJECT                                                                
025010 01  NYCKLAR-TILL-DLI.                                                    
025020   03  W-WDQ2CSEQ-X.                                                      
025030     05  W-IDDISTR               PIC S9(5)                COMP-3.         
025040     05  W-IDKUNDNR              PIC S9(7)                COMP-3.         
025050     05  W-IDKUNDRF.                                                      
025060      07 W-IDORDNR               PIC  9(7).                               
025070      07 FILLER                  PIC  X(3).                               
025080     SKIP3                                                                
025095*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
025096*                                                                         
025097 01  IMS-WS.                                                              
025098   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
025099     SKIP3                                                                
025100*                            *** STATUSKOD FRÅN IMS                       
025101   03  STATUS-WS                 PIC XX.                                  
025102     88  SEGMENT-FINNS                       VALUE '  '.                  
025103     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025104     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025105     SKIP3                                                                
025106   03  GODK-STATUSKODER.                                                  
025107     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025108     SKIP3                                                                
025109   03  SSA1                      PIC X(64).                               
025110     EJECT                                                                
025111*01  -COPY W0003                                                          
025112     EJECT                                                                
025113 01  DLI-IO-AREA.                                                         
025116*  03  WLORQI01 -COPY WDQ201                                              
025117     EJECT                                                                
025120 LINKAGE SECTION.                                                         
025121     SKIP2                                                                
025130*01      -COPY W0008     -PRE ORQI-                                       
025140      05 FILLER          PIC X.                                           
025300     EJECT                                                                
025400 PROCEDURE DIVISION  USING ORQI-PCB.                                      
025500     ENTRY 'DLITCBL' USING ORQI-PCB.                                      
025600     SKIP2                                                                
025700     PERFORM A-INIT                                                       
025800                                                                          
025900     MOVE -150000 TO SORT-CORE-SIZE                                       
026000     SORT SORTFIL ASCENDING                                               
026100                  SORT-BIP-SOR0-IDDISTR                                   
026200                  SORT-BIP-SOR0-IDKUNDNR                                  
026300                  SORT-BIP-SOR0-IDRONR                                    
026400                  SORT-DECADE                                             
026410                  SORT-SMALL-DATE                                         
026420                  COLLATING SEQUENCE Y2000                                
026500          INPUT  PROCEDURE B-INPUT-PROCEDURE                              
026600          OUTPUT PROCEDURE C-OUTPUT-PROCEDURE                             
026700     SKIP2                                                                
026800     IF SORT-RETURN > ZERO                                                
026900       DISPLAY '***  W4611000  - FEL VID SORTERING'                       
027000       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
027100     ELSE                                                                 
027200       PERFORM Z-FINIT                                                    
027300       MOVE ZERO TO RETURN-CODE                                           
027400       GOBACK                                                             
027500                                                                          
027600     END-IF                                                               
027700     .                                                                    
027800     EJECT                                                                
027900 A-INIT SECTION.                                                          
028000     SKIP2                                                                
028100     OPEN OUTPUT W46111-UT                                                
028200     OPEN  INPUT W46110-IN                                                
028300     SKIP2                                                                
028400     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
028500     MOVE    ZERO      TO SPAR-IDDISTR                                    
028600     MOVE    ZERO      TO SPAR-IDKUNDNR                                   
028700     MOVE    ZERO      TO SPAR-IDORDNR                                    
029100     .                                                                    
029200     EJECT                                                                
029300 B-INPUT-PROCEDURE SECTION.                                               
029400     SKIP2                                                                
029500     PERFORM S01-LAS-W46110-IN                                            
029600     PERFORM UNTIL W46110-EOF = JA                                        
029800       IF      IN-FHUV-SOR0-IDRONR NOT = ZERO                             
029900         PERFORM  BA-SORTERA-POSTER                                       
030000       ELSE                                                               
030100         PERFORM  BB-SKRIV-POSTER                                         
030200       END-IF                                                             
030300       PERFORM S01-LAS-W46110-IN                                          
030400*                                                                         
030500     END-PERFORM                                                          
030600     .                                                                    
030700     EJECT                                                                
030800 BA-SORTERA-POSTER SECTION.                                               
030900*                                                                         
031000     EVALUATE IN-FHUV-IDPTYP                                              
031100     WHEN '001'                                                           
031200       RELEASE SORT-POST01  FROM  IN-POST01                               
031300     WHEN '002'                                                           
031400       RELEASE SORT-POST02  FROM  IN-POST02                               
031500     WHEN '003'                                                           
031600       RELEASE SORT-POST03  FROM  IN-POST03                               
031700     WHEN '004'                                                           
031800       RELEASE SORT-POST04  FROM  IN-POST04                               
031900     WHEN '005'                                                           
032000       RELEASE SORT-POST05  FROM  IN-POST05                               
032100     WHEN '006'                                                           
032200       RELEASE SORT-POST06  FROM  IN-POST06                               
032300     WHEN '007'                                                           
032400       RELEASE SORT-POST07  FROM  IN-POST07                               
032500     WHEN '008'                                                           
032600       RELEASE SORT-POST08  FROM  IN-POST08                               
032700     WHEN '010'                                                           
032800       RELEASE SORT-POST10  FROM  IN-POST10                               
032900     WHEN '011'                                                           
033000       RELEASE SORT-POST11  FROM  IN-POST11                               
033100     WHEN '012'                                                           
033200       RELEASE SORT-POST12  FROM  IN-POST12                               
033300     WHEN '013'                                                           
033400       RELEASE SORT-POST13  FROM  IN-POST13                               
033500     WHEN '015'                                                           
033600       RELEASE SORT-POST15  FROM  IN-POST15                               
033700     WHEN '016'                                                           
033800       RELEASE SORT-POST16  FROM  IN-POST16                               
033900     WHEN '020'                                                           
034000       RELEASE SORT-POST20  FROM  IN-POST20                               
034100     WHEN OTHER                                                           
034200       DISPLAY 'FEL POSTTYP ' IN-POST01                                   
034300     END-EVALUATE                                                         
034400     .                                                                    
034500     EJECT                                                                
034600 BB-SKRIV-POSTER SECTION.                                                 
034700*                                                                         
034800     EVALUATE IN-FHUV-IDPTYP                                              
034900     WHEN '001'                                                           
035000       PERFORM BBA-BIPACK                                                 
035100     WHEN '002'                                                           
035200       PERFORM BBB-ORDERBEKR-HUVUD                                        
035300     WHEN '003'                                                           
035400       PERFORM BBC-ENTYDIG                                                
035500     WHEN '004'                                                           
035600       PERFORM BBD-EJENTYDIG                                              
035700     WHEN '005'                                                           
035800       PERFORM BBE-ERS-TEXT                                               
035900     WHEN '006'                                                           
036000       PERFORM BBF-KVANTANP                                               
036100     WHEN '007'                                                           
036200       PERFORM BBG-LAG-AVB                                                
036300     WHEN '008'                                                           
036400       PERFORM BBH-STOPPAD-RAD                                            
036500     WHEN '010'                                                           
036600       PERFORM BBI-FAKTURA-HUVUD                                          
036700     WHEN '011'                                                           
036800       PERFORM BBK-REFERENS                                               
036900     WHEN '012'                                                           
037000       PERFORM BBL-KOLLI                                                  
037100     WHEN '013'                                                           
037200       PERFORM BBM-RADPOST                                                
037300     WHEN '015'                                                           
037400       PERFORM BBN-BYPASS                                                 
037500     WHEN '016'                                                           
037600       PERFORM BBO-AVSTPOST                                               
037700     WHEN '020'                                                           
037800       PERFORM BBP-ON-ORDERPOST                                           
037900     WHEN OTHER                                                           
038000       DISPLAY 'FEL POSTTYP ' IN-FHUV-IDPTYP                              
038100     END-EVALUATE                                                         
038200     .                                                                    
038300     EJECT                                                                
038400 BBA-BIPACK SECTION.                                                      
038500     WRITE   UT-POST01       FROM  IN-POST01                              
038600     MOVE    '001'           TO POSTSUM-TRANSTYP                          
038700     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
038800     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
038900     CALL    POSTSUM         USING POSTSUM-PARM                           
039000     .                                                                    
039100     SKIP3                                                                
039200 BBB-ORDERBEKR-HUVUD SECTION.                                             
039300     WRITE   UT-POST02       FROM IN-POST02                               
039400     MOVE    '002'           TO POSTSUM-TRANSTYP                          
039500     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
039600     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
039700     CALL    POSTSUM         USING POSTSUM-PARM                           
039800     .                                                                    
039900     SKIP3                                                                
040000 BBC-ENTYDIG SECTION.                                                     
040100     WRITE   UT-POST03       FROM IN-POST03                               
040200     MOVE    '003'           TO POSTSUM-TRANSTYP                          
040300     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
040400     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
040500     CALL    POSTSUM         USING POSTSUM-PARM                           
040600     .                                                                    
040700     SKIP3                                                                
040800 BBD-EJENTYDIG SECTION.                                                   
040900     WRITE   UT-POST04       FROM IN-POST04                               
041000     MOVE    '004'           TO POSTSUM-TRANSTYP                          
041100     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
041200     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
041300     CALL    POSTSUM         USING POSTSUM-PARM                           
041400     .                                                                    
041500     EJECT                                                                
041600 BBE-ERS-TEXT SECTION.                                                    
041700     WRITE   UT-POST05       FROM IN-POST05                               
041800     MOVE    '005'           TO POSTSUM-TRANSTYP                          
041900     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
042000     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
042100     CALL    POSTSUM         USING POSTSUM-PARM                           
042200     .                                                                    
042300     SKIP3                                                                
042400 BBF-KVANTANP SECTION.                                                    
042500     WRITE   UT-POST06       FROM IN-POST06                               
042600     MOVE    '006'           TO POSTSUM-TRANSTYP                          
042700     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
042800     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
042900     CALL    POSTSUM         USING POSTSUM-PARM                           
043000     .                                                                    
043100     SKIP3                                                                
043200 BBG-LAG-AVB SECTION.                                                     
043300     WRITE   UT-POST07       FROM IN-POST07                               
043400     MOVE    '007'           TO POSTSUM-TRANSTYP                          
043500     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
043600     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
043700     CALL    POSTSUM         USING POSTSUM-PARM                           
043800     .                                                                    
043900     SKIP3                                                                
044000 BBH-STOPPAD-RAD SECTION.                                                 
044100     WRITE   UT-POST08       FROM IN-POST08                               
044200     MOVE    '008'           TO POSTSUM-TRANSTYP                          
044300     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
044400     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
044500     CALL    POSTSUM         USING POSTSUM-PARM                           
044600     .                                                                    
044700     EJECT                                                                
044800 BBI-FAKTURA-HUVUD SECTION.                                               
044900     MOVE    IN-POST10       TO UT-POST10                                 
045000     WRITE   UT-POST10                                                    
045100     MOVE    '010'           TO POSTSUM-TRANSTYP                          
045200     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
045300     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
045400     CALL    POSTSUM         USING POSTSUM-PARM                           
045500     .                                                                    
045600     SKIP3                                                                
045700 BBK-REFERENS SECTION.                                                    
045800     WRITE   UT-POST11       FROM IN-POST11                               
045900     MOVE    '011'           TO POSTSUM-TRANSTYP                          
046000     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
046100     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
046200     CALL    POSTSUM         USING POSTSUM-PARM                           
046300     .                                                                    
046400     SKIP3                                                                
046500 BBL-KOLLI SECTION.                                                       
046600     WRITE   UT-POST12       FROM IN-POST12                               
046700     MOVE    '012'           TO POSTSUM-TRANSTYP                          
046800     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
046900     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
047000     CALL    POSTSUM         USING POSTSUM-PARM                           
047100     .                                                                    
047200     SKIP3                                                                
047300 BBM-RADPOST SECTION.                                                     
047400     WRITE   UT-POST13       FROM IN-POST13                               
047500     MOVE    '013'           TO POSTSUM-TRANSTYP                          
047600     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
047700     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
047800     CALL    POSTSUM         USING POSTSUM-PARM                           
047900     .                                                                    
048000     EJECT                                                                
048100 BBN-BYPASS SECTION.                                                      
048200     WRITE   UT-POST15       FROM  IN-POST15                              
048300     MOVE    '015'           TO POSTSUM-TRANSTYP                          
048400     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
048500     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
048600     CALL    POSTSUM         USING POSTSUM-PARM                           
048700     .                                                                    
048800     SKIP3                                                                
048900 BBO-AVSTPOST SECTION.                                                    
049000     WRITE   UT-POST16       FROM IN-POST16                               
049100     MOVE    '016'           TO POSTSUM-TRANSTYP                          
049200     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
049300     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
049400     CALL    POSTSUM         USING POSTSUM-PARM                           
049500     .                                                                    
049600     EJECT                                                                
049700 BBP-ON-ORDERPOST SECTION.                                                
049800     WRITE   UT-POST20       FROM IN-POST20                               
049900     MOVE    '020'           TO POSTSUM-TRANSTYP                          
050000     MOVE    'W46111'        TO POSTSUM-FDNAMN                            
050100     MOVE    'W46110D2'      TO POSTSUM-DDNAMN2                           
050200     CALL    POSTSUM         USING POSTSUM-PARM                           
050300     .                                                                    
050400     EJECT                                                                
050500 C-OUTPUT-PROCEDURE SECTION.                                              
050600     SKIP2                                                                
050700     MOVE 'W46111'            TO POSTSUM-FDNAMN                           
050800     MOVE 'W46110D2'          TO POSTSUM-DDNAMN2                          
050900     SKIP2                                                                
051000     PERFORM S03-RETURN-SORTFIL                                           
051100     PERFORM UNTIL SORTFIL-EOF = JA                                       
051300       PERFORM  CB-GEMENSAM-BEARB                                         
051400*                                                                         
051500       PERFORM  CA-SKRIV-W46111                                           
051600       PERFORM S03-RETURN-SORTFIL                                         
051700     END-PERFORM                                                          
051800     .                                                                    
051900     EJECT                                                                
052000 CA-SKRIV-W46111 SECTION.                                                 
052100*                                                                         
052200     EVALUATE SORT-BIP-IDPTYP                                             
052300     WHEN '001'                                                           
052400       PERFORM CAA-BIPACK                                                 
052500     WHEN '002'                                                           
052600       PERFORM CAB-ORDERBEKR-HUVUD                                        
052700     WHEN '003'                                                           
052800       PERFORM CAC-ENTYDIG                                                
052900     WHEN '004'                                                           
053000       PERFORM CAD-EJENTYDIG                                              
053100     WHEN '005'                                                           
053200       PERFORM CAE-ERS-TEXT                                               
053300     WHEN '006'                                                           
053400       PERFORM CAF-KVANTANP                                               
053500     WHEN '007'                                                           
053600       PERFORM CAG-LAG-AVB                                                
053700     WHEN '008'                                                           
053800       PERFORM CAH-STOPPAD-RAD                                            
053900     WHEN '010'                                                           
054000       PERFORM CAI-FAKTURA-HUVUD                                          
054100     WHEN '011'                                                           
054200       PERFORM CAK-REFERENS                                               
054300     WHEN '012'                                                           
054400       PERFORM CAL-KOLLI                                                  
054500     WHEN '013'                                                           
054600       PERFORM CAM-RADPOST                                                
054700     WHEN '015'                                                           
054800       PERFORM CAN-BYPASS                                                 
054900     WHEN '016'                                                           
055000       PERFORM CAO-AVSTPOST                                               
055100     WHEN '020'                                                           
055200       PERFORM CAP-ON-ORDERPOST                                           
055300     END-EVALUATE                                                         
055400     .                                                                    
055500     EJECT                                                                
055600 CB-GEMENSAM-BEARB SECTION.                                               
055700     SKIP2                                                                
055800     IF      SORT-BIP-SOR0-IDDISTR = SPAR-IDDISTR AND                     
055900     SORT-BIP-SOR0-IDKUNDNR = SPAR-IDKUNDNR AND                           
056000     SORT-BIP-SOR0-IDRONR  = SPAR-IDORDNR                                 
056100*           TOM SATS                                                      
056200       CONTINUE                                                           
056300     ELSE                                                                 
056500       MOVE    SORT-BIP-SOR0-IDDISTR     TO W-IDDISTR                     
056510                                            SPAR-IDDISTR                  
056600       MOVE    SORT-BIP-SOR0-IDKUNDNR    TO W-IDKUNDNR                    
056610                                            SPAR-IDKUNDNR                 
056620       MOVE    SPACE                     TO W-IDKUNDRF                    
056700       MOVE    SORT-BIP-SOR0-IDRONR      TO W-IDORDNR                     
056710                                            SPAR-IDORDNR                  
056800                                                                          
056900       PERFORM IMS-GET-REFERENSER                                         
057000                                                                          
057100       IF      SEGMENT-SAKNAS                                             
057200         MOVE     +4       TO OHUV-KDORDKL                                
057300         MOVE     SPACE    TO OHUV-BEKUNDRF                               
057400         MOVE     SPACE    TO OHUV-BEVARREF                               
057500       END-IF                                                             
057600     END-IF                                                               
057700     .                                                                    
057800     EJECT                                                                
057900 CAA-BIPACK SECTION.                                                      
058000     MOVE    SORT-POST01     TO UT-POST01                                 
058100     WRITE   UT-POST01                                                    
058200     MOVE    '001'           TO POSTSUM-TRANSTYP                          
058300     CALL    POSTSUM         USING POSTSUM-PARM                           
058400     .                                                                    
058500     SKIP3                                                                
058600 CAB-ORDERBEKR-HUVUD SECTION.                                             
058700     MOVE    SORT-POST02     TO UT-POST02                                 
058800     MOVE    OHUV-KDORDKL    TO UT-OBHUV-KDORDKL                          
058900     MOVE    OHUV-BEVARREF   TO UT-OBHUV-BEVARREF                         
059000     MOVE    OHUV-BEKUNDRF   TO UT-OBHUV-BEVOLREF                         
059100     WRITE   UT-POST02                                                    
059200     MOVE    '002'           TO POSTSUM-TRANSTYP                          
059300     CALL    POSTSUM         USING POSTSUM-PARM                           
059400     .                                                                    
059500     SKIP3                                                                
059600 CAC-ENTYDIG SECTION.                                                     
059700     MOVE    SORT-POST03     TO UT-POST03                                 
059800     WRITE   UT-POST03                                                    
059900     MOVE    '003'           TO POSTSUM-TRANSTYP                          
060000     CALL    POSTSUM         USING POSTSUM-PARM                           
060100     .                                                                    
060200     SKIP3                                                                
060300 CAD-EJENTYDIG SECTION.                                                   
060400     MOVE    SORT-POST04     TO UT-POST04                                 
060500     WRITE   UT-POST04                                                    
060600     MOVE    '004'           TO POSTSUM-TRANSTYP                          
060700     CALL    POSTSUM         USING POSTSUM-PARM                           
060800     .                                                                    
060900     EJECT                                                                
061000 CAE-ERS-TEXT SECTION.                                                    
061100     MOVE    SORT-POST05     TO UT-POST05                                 
061200     WRITE   UT-POST05                                                    
061300     MOVE    '005'           TO POSTSUM-TRANSTYP                          
061400     CALL    POSTSUM         USING POSTSUM-PARM                           
061500     .                                                                    
061600     SKIP3                                                                
061700 CAF-KVANTANP SECTION.                                                    
061800     MOVE    SORT-POST06     TO UT-POST06                                 
061900     WRITE   UT-POST06                                                    
062000     MOVE    '006'           TO POSTSUM-TRANSTYP                          
062100     CALL    POSTSUM         USING POSTSUM-PARM                           
062200     .                                                                    
062300     SKIP3                                                                
062400 CAG-LAG-AVB SECTION.                                                     
062500     MOVE    SORT-POST07     TO UT-POST07                                 
062600     WRITE   UT-POST07                                                    
062700     MOVE    '007'           TO POSTSUM-TRANSTYP                          
062800     CALL    POSTSUM         USING POSTSUM-PARM                           
062900     .                                                                    
063000     SKIP3                                                                
063100 CAH-STOPPAD-RAD SECTION.                                                 
063200     MOVE    SORT-POST08     TO UT-POST08                                 
063300     WRITE   UT-POST08                                                    
063400     MOVE    '008'           TO POSTSUM-TRANSTYP                          
063500     CALL    POSTSUM         USING POSTSUM-PARM                           
063600     .                                                                    
063700     EJECT                                                                
063800 CAI-FAKTURA-HUVUD SECTION.                                               
063900     MOVE    SORT-POST10     TO UT-POST10                                 
064000     WRITE   UT-POST10                                                    
064100     MOVE    '010'           TO POSTSUM-TRANSTYP                          
064200     CALL    POSTSUM         USING POSTSUM-PARM                           
064300     .                                                                    
064400     SKIP3                                                                
064500 CAK-REFERENS SECTION.                                                    
064600     MOVE    SORT-POST11     TO UT-POST11                                 
064700     MOVE    OHUV-BEVARREF   TO UT-FREF-BEVARREF                          
064800     WRITE   UT-POST11                                                    
064900     MOVE    '011'           TO POSTSUM-TRANSTYP                          
065000     CALL    POSTSUM         USING POSTSUM-PARM                           
065100     .                                                                    
065200     SKIP3                                                                
065300 CAL-KOLLI SECTION.                                                       
065400     MOVE    SORT-POST12     TO UT-POST12                                 
065500     WRITE   UT-POST12                                                    
065600     MOVE    '012'           TO POSTSUM-TRANSTYP                          
065700     CALL    POSTSUM         USING POSTSUM-PARM                           
065800     .                                                                    
065900     SKIP3                                                                
066000 CAM-RADPOST SECTION.                                                     
066100     MOVE    SORT-POST13     TO UT-POST13                                 
066200     MOVE    OHUV-KDORDKL    TO UT-FRAD-KDORDKL-URS                       
066300     WRITE   UT-POST13                                                    
066400     MOVE    '013'           TO POSTSUM-TRANSTYP                          
066500     CALL    POSTSUM         USING POSTSUM-PARM                           
066600     .                                                                    
066700     EJECT                                                                
066800 CAN-BYPASS SECTION.                                                      
066900     MOVE    SORT-POST15     TO UT-POST15                                 
067000     WRITE   UT-POST15                                                    
067100     MOVE    '015'           TO POSTSUM-TRANSTYP                          
067200     CALL    POSTSUM         USING POSTSUM-PARM                           
067300     .                                                                    
067400     SKIP3                                                                
067500 CAO-AVSTPOST SECTION.                                                    
067600     MOVE    SORT-POST16     TO UT-POST16                                 
067700     WRITE   UT-POST16                                                    
067800     MOVE    '016'           TO POSTSUM-TRANSTYP                          
067900     CALL    POSTSUM         USING POSTSUM-PARM                           
068000     .                                                                    
068100     EJECT                                                                
068200 CAP-ON-ORDERPOST SECTION.                                                
068300     MOVE    SORT-POST20     TO UT-POST20                                 
068400     WRITE   UT-POST20                                                    
068500     MOVE    '020'           TO POSTSUM-TRANSTYP                          
068600     CALL    POSTSUM         USING POSTSUM-PARM                           
068700     .                                                                    
068800     EJECT                                                                
068900 S01-LAS-W46110-IN SECTION.                                               
069000     SKIP2                                                                
069100     READ  W46110-IN                                                      
069200                      AT END MOVE JA TO W46110-EOF                        
069300     END-READ                                                             
069400                                                                          
069500     IF W46110-EOF = NEJ                                                  
069600                                                                          
069700       MOVE 'W46110'            TO POSTSUM-FDNAMN                         
069800       MOVE 'W46110D1'          TO POSTSUM-DDNAMN2                        
069900       MOVE IN-FHUV-IDPTYP      TO POSTSUM-TRANSTYP                       
070000       CALL POSTSUM   USING POSTSUM-PARM                                  
070100                                                                          
070200     END-IF                                                               
070300     .                                                                    
070400     SKIP3                                                                
070500                                                                          
070600 S03-RETURN-SORTFIL SECTION.                                              
070700     SKIP2                                                                
070800     RETURN SORTFIL                                                       
070900                      AT END MOVE JA TO SORTFIL-EOF                       
071000     END-RETURN                                                           
071100     .                                                                    
071200                                                                          
071300     SKIP3                                                                
071400 Z-FINIT SECTION.                                                         
071500     SKIP2                                                                
071600     CLOSE W46110-IN   W46111-UT                                          
071700     SKIP2                                                                
071800*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
071900*                                    SKRIVNA POSTER                       
072000                                                                          
072100     MOVE 'S' TO POSTSUM-OPKOD                                            
072200     CALL POSTSUM USING POSTSUM-PARM                                      
072300     .                                                                    
072310     EJECT                                                                
072400* IMS SEKTIONER                                                           
072500     SKIP3                                                                
072600 IMS-GET-REFERENSER SECTION.                                              
072700     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
072800            DELIMITED BY SIZE INTO SSA1                                   
072900     MOVE '  GE' TO GODK-STATUSKODER                                      
073000     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-AREA SSA1                      
073100     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
073200     PERFORM IMS-STATUSKONTROLL                                           
073300     .                                                                    
073400     SKIP3                                                                
073500     EJECT                                                                
073600 IMS-STATUSKONTROLL SECTION.                                              
073700     SET STATUS-IX TO 1                                                   
073800     SEARCH GODK-STATUS AT END CALL FELLOG                                
073900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
074000     CONTINUE                                                             
074100     END-SEARCH                                                           
074200     .                                                                    

000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4610500.                                                 
001000*AUTHOR.        N. N.                                                     
001100*DATE-WRITTEN.  NOV 1984.                                                 
001200                                                                          
001700                                                                          
001800*    FUNKTION:                                                            
001900                                                                          
002000*        ORDERBEKRÄFTELSE TRANSARNA SORTERAS I ORDNING FÖR VIPS           
002100                                                                          
002200*        EN SORTAREA BYGGES TEMPORÄRT                                     
002300                                                                          
002400*        EFTER SORTEN TAS DUBLETTHUVUD BORT,TRANSARNA NUMRERAS            
002500*        OCH SKRIVS PÅ W46107 FIL.                                        
002600                                                                          
002700*        FÖR ORDERBEKR. POSTER DÄR RONR EJ = NOLL LÄGGS RONR I            
002800*        SORTDELEN PÅ W46107 FIL                                          
002900                                                                          
003000*    ABENDKODER:                                                          
003100                                                                          
003200*        U0016    - OM RETURKOD FRÅN SORT                                 
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     SKIP2                                                                
004000*- - - - - - - - - - - - INFIL:                                           
004100*                        - -  FIL ORDERBEKRÄFTELSER                       
004200     SELECT W46100-BEKR                  ASSIGN TO UT-S-W46105D1.         
004300     SKIP2                                                                
004400*- - - - - - - - - - - - UTFIL:                                           
004500*                        - -  W46107 NUMRERAD W46105 FIL                  
004600     SELECT W46105-UT                    ASSIGN TO UT-S-W46105D2.         
004700     SKIP2                                                                
004800*- - - - - - - - - - - - SORTFIL:                                         
004900     SELECT SORTFIL                      ASSIGN TO UT-S-W46105DS.         
005000     EJECT                                                                
005100 DATA DIVISION.                                                           
005200     SKIP2                                                                
005300 FILE SECTION.                                                            
005400     SKIP3                                                                
005500 FD  W46100-BEKR                                                          
005600     RECORDING      V                                                     
005700     BLOCK CONTAINS 0.                                                    
005800     SKIP2                                                                
005900*01  POST  -COPY W461002    -L.                                           
006000*01  POST1 -COPY W461003    -L.                                           
006100*01  POST2 -COPY W461004    -L.                                           
006200*01  POST3 -COPY W461005    -L.                                           
006300*01  POST4 -COPY W461006    -L.                                           
006400*01  POST5 -COPY W461007    -L.                                           
006500*01  POST6 -COPY W461008    -L.                                           
006600     SKIP3                                                                
006700 FD  W46105-UT                                                            
006800     RECORDING      V                                                     
006900     BLOCK CONTAINS 0.                                                    
007000     SKIP2                                                                
007100*01  POST  -COPY W461S002   -PRE UT-  -L.                                 
007200*01  POST1 -COPY W461S003   -PRE UT- -L.                                  
007300*01  POST2 -COPY W461S004   -PRE UT- -L.                                  
007400*01  POST3 -COPY W461S005   -PRE UT- -L.                                  
007500*01  POST4 -COPY W461S006   -PRE UT- -L.                                  
007600*01  POST5 -COPY W461S007   -PRE UT- -L.                                  
007700*01  POST6 -COPY W461S008   -PRE UT- -L.                                  
007800     SKIP2                                                                
007900 SD  SORTFIL                                                              
008000                .                                                         
008100 01  SORT-POST-HUVUD.                                                     
008200   02  SORT-TEMP-SORT.                                                    
008300     03  SORT-IDDISTR            PIC S9(5)   COMP-3.                      
008400     03  SORT-IDKUNDNR           PIC S9(7)   COMP-3.                      
008500     03  SORT-IDORDNR            PIC S9(7)   COMP-3.                      
008600     03  SORT-DEL-AV-LIST        PIC S9(1)   COMP-3.                      
008700     03  SORT-IDARTNR            PIC S9(9)   COMP-3.                      
008800     03  SORT-IDRONR             PIC S9(7)   COMP-3.                      
008900     03  SORT-ORSAKSKOD          PIC S9(7)   COMP-3.                      
009000     03  SORT-IDLOPNR            PIC S9(7)   COMP-3.                      
009100     03  SORT-KORTNR             PIC S9(7)   COMP-3.                      
009200*                                PIC X(33)                                
009300*                                                                         
009400 02  SORT-AREA-HUVUD.                                                     
009500*03  POST0 -COPY W461002    -PRE SO-.                                     
009600 01  SORT-POST-ENTYDIG.                                                   
009700   02  FILLER                    PIC X(33).                               
009800 02  SORT-AREA-ENTYDIG.                                                   
009900*03  POST1 -COPY W461003    -PRE SO-.                                     
010000 01  SORT-POST-EJENTYDIG.                                                 
010100   02  FILLER                    PIC X(33).                               
010200 02  SORT-AREA-EJENTYDIG.                                                 
010300*03  POST2 -COPY W461004    -PRE SO-.                                     
010400 01  SORT-POST-ERS-TEXT.                                                  
010500   02  FILLER                    PIC X(33).                               
010600 02  SORT-AREA-ERS-TEXT.                                                  
010700*03  POST3 -COPY W461005    -PRE SO-.                                     
010800 01  SORT-POST-KVANTANP.                                                  
010900   02  FILLER                    PIC X(33).                               
011000 02  SORT-AREA-KVANTANP.                                                  
011100*03  POST4 -COPY W461006    -PRE SO-.                                     
011200 01  SORT-POST-LAG-AVB.                                                   
011300   02  FILLER                    PIC X(33).                               
011400 02  SORT-AREA-LAG-AVB.                                                   
011500*03  POST5 -COPY W461007    -PRE SO-.                                     
011600 01  SORT-POST-STOPPAD.                                                   
011700   02  FILLER                    PIC X(33).                               
011800 02  SORT-AREA-STOPPAD.                                                   
011900*03  POST6 -COPY W461008    -PRE SO-.                                     
012000     EJECT                                                                
012100 WORKING-STORAGE SECTION.                                                 
012101                                                                          
012110*    -- CHECKED BY WY2000                                                 
012200 77  IDPGM                       PIC X(8)    VALUE 'W4610500'.            
013000     SKIP2                                                                
013100*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
013200                                                                          
013300 77  JA                          PIC X(1)    VALUE 'J'.                   
013400 77  NEJ                         PIC X(1)    VALUE 'N'.                   
013500 77  WS-HUVUD-SAKNAS             PIC X(1)    VALUE 'J'.                   
013600     SKIP2                                                                
013700*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
013800                                                                          
013900 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
014000 77  W46100-EOF                  PIC X(1)    VALUE 'N'.                   
014100*                                                                         
014200     SKIP2                                                                
014300*- - - - - - - - - - - - - -  SUBMODULER                                  
014400                                                                          
014500*- - - - - - - - - - - - - -  SPAR AREAOR                                 
014600 01  FILLER                      PIC X(8)    VALUE 'AREAOR'.              
014700*                                                                         
014800 77  WS-LOPNR                    PIC S9(9)   COMP-3 VALUE ZERO.           
014900*- - - - - - - - - - - - - -                                              
015000 01  WS-GAMMALT-ID.                                                       
015100     03  G-IDORDNR               PIC S9(7)  COMP-3   VALUE ZERO.          
015200     03  G-IDDISTR               PIC S9(5)  COMP-3   VALUE ZERO.          
015300     03  G-IDKUNDNR              PIC S9(7)  COMP-3   VALUE ZERO.          
015400*- - - - - - - - - - - - - -                                              
015500 01  WS-HUV-AREA.                                                         
015600     03  WS-HUV-IDORDNR          PIC S9(7)  COMP-3   VALUE ZERO.          
015700     03  WS-HUV-IDDISTR          PIC S9(5)  COMP-3   VALUE ZERO.          
015800     03  WS-HUV-IDKUNDNR         PIC S9(7)  COMP-3   VALUE ZERO.          
015900     EJECT                                                                
016000*- - - - - - - - - - - - - - -                                            
016100                                                                          
016200 01  W-SORTAREA.                                                          
016300     03  W-IDDISTR               PIC S9(5)   COMP-3  VALUE ZERO.          
016400     03  W-IDKUNDNR              PIC S9(7)   COMP-3  VALUE ZERO.          
016500     03  W-IDORDNR               PIC S9(7)   COMP-3  VALUE ZERO.          
016600     03  W-KDLIDEL               PIC S9(1)   COMP-3  VALUE ZERO.          
016700     03  W-IDARTNR               PIC S9(9)   COMP-3  VALUE ZERO.          
016800     03  W-IDRONR                PIC S9(7)   COMP-3  VALUE ZERO.          
016900     03  W-KDRESTR               PIC S9(7)   COMP-3  VALUE ZERO.          
017000     03  W-IDLOPNRE              PIC S9(7)   COMP-3  VALUE ZERO.          
017100     03  W-IDKORTNR              PIC S9(7)   COMP-3  VALUE ZERO.          
017200*- - - - - - - - - - - - - -                                              
017300*                                     IN AREA                             
017400 01  FILLER                      PIC X(8)   VALUE 'IN-AREA'.              
017500*                                                                         
017600 01  IN-AREA                  PIC X(200).                                 
017700*01  POST  -COPY W461002    -PRE IN- -RED IN-AREA.                        
017800*01  POST1 -COPY W461003    -PRE IN- -RED IN-AREA.                        
017900*01  POST2 -COPY W461004    -PRE IN- -RED IN-AREA.                        
018000*01  POST3 -COPY W461005    -PRE IN- -RED IN-AREA.                        
018100*01  POST4 -COPY W461006    -PRE IN- -RED IN-AREA.                        
018200*01  POST5 -COPY W461007    -PRE IN- -RED IN-AREA.                        
018300*01  POST6 -COPY W461008    -PRE IN- -RED IN-AREA.                        
018400     SKIP3                                                                
018500                                                                          
018600     EJECT                                                                
018700 01  DYNAMISKA-SUBPROGRAM.                                                
018800   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
018900   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
019000   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
019100     SKIP3                                                                
019200*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
019300                                                                          
019400 01  RETURKODER.                                                          
019500   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
019600   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
019700   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
019800     EJECT                                                                
019900*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
020000                                                                          
020100*01  -COPY W0005       -PRE  POSTSUM-.                                    
020200     EJECT                                                                
020300 01  FILLER                   PIC X(8)       VALUE 'X HUVUD '.            
020400 01  WS-EXTRA-HUVUD-POST.                                                 
020500*02  AREA0 -COPY W461SOR0    -PRE  X-.                                    
020600*02  AREA1 -COPY W461002     -PRE  X-.                                    
020700     EJECT                                                                
020800 01  FILLER                   PIC X(8)       VALUE 'OUTPUT  '.            
020900 01  OUTPUT-AREA              PIC X(500).                                 
021000 01  AREA-HUVUD        REDEFINES OUTPUT-AREA.                             
021100*01  AREA-HUVUD.                                                          
021200*02  AREA0 -COPY W461SOR0    -PRE  H-.                                    
021300*02  AREA1 -COPY W461002     -PRE  H-.                                    
021400     EJECT                                                                
021500 01  AREA-ENTYDIG      REDEFINES OUTPUT-AREA.                             
021600*01  AREA-ENTYDIG.                                                        
021700*02  AREA0 -COPY W461SOR0    -PRE  E-.                                    
021800*02  AREA1 -COPY W461003     -PRE  E-.                                    
021900     EJECT                                                                
022000 01  AREA-EJENTYDIG    REDEFINES OUTPUT-AREA.                             
022100*01  AREA-EJENTYDIG.                                                      
022200*02  AREA0 -COPY W461SOR0    -PRE  J-.                                    
022300*02  AREA1 -COPY W461004     -PRE  J-.                                    
022400     EJECT                                                                
022500 01  AREA-ERS-TEXT     REDEFINES OUTPUT-AREA.                             
022600*01  AREA-ERS-TEXT.                                                       
022700*02  AREA0 -COPY W461SOR0    -PRE  T-.                                    
022800*02  AREA1 -COPY W461005     -PRE  T-.                                    
022900     EJECT                                                                
023000*01  AREA-KVANTANP.                                                       
023100 01  AREA-KVANTANP     REDEFINES OUTPUT-AREA.                             
023200*02  AREA0 -COPY W461SOR0    -PRE  K-.                                    
023300*02  AREA1 -COPY W461006     -PRE  K-.                                    
023400     EJECT                                                                
023500 01  AREA-LAG-AVB      REDEFINES OUTPUT-AREA.                             
023600*01  AREA-LAG-AVB.                                                        
023700*02  AREA0 -COPY W461SOR0    -PRE  L-.                                    
023800*02  AREA1 -COPY W461007     -PRE  L-.                                    
023900     EJECT                                                                
024000 01  AREA-STOPPAD      REDEFINES OUTPUT-AREA.                             
024100*01  AREA-STOPPAD.                                                        
024200*02  AREA0 -COPY W461SOR0    -PRE  S-.                                    
024300*02  AREA1 -COPY W461008     -PRE  S-.                                    
024400     EJECT                                                                
024500 PROCEDURE DIVISION.                                                      
024600     SKIP2                                                                
024700     PERFORM A-INIT                                                       
024800                                                                          
024900     SORT SORTFIL ASCENDING                                               
025000                  SORT-TEMP-SORT                                          
025100          INPUT  PROCEDURE B-INPUT-PROCEDURE                              
025200          OUTPUT PROCEDURE C-OUTPUT-PROCEDURE                             
025300     SKIP2                                                                
025400     IF SORT-RETURN > ZERO                                                
025500       DISPLAY '***  W4610500  - FEL VID SORTERING'                       
025600       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
025700     ELSE                                                                 
025800       PERFORM Z-FINIT                                                    
025900       MOVE ZERO TO RETURN-CODE                                           
026000       GOBACK                                                             
026100                                                                          
026200     END-IF                                                               
026300     CONTINUE.                                                            
026400     EJECT                                                                
026500 A-INIT SECTION.                                                          
026600     SKIP2                                                                
026700     OPEN OUTPUT W46105-UT                                                
026800     OPEN  INPUT W46100-BEKR                                              
026900     SKIP2                                                                
027000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027100     CONTINUE.                                                            
027200     EJECT                                                                
027300 B-INPUT-PROCEDURE SECTION.                                               
027400     SKIP2                                                                
027500*                            SKAPA TEMPORÄR SORTAREA                      
027600*                                                                         
027700     PERFORM S01-LAS-W46100                                               
027800     PERFORM UNTIL                                                        
027900      NOT ( W46100-EOF = NEJ )                                            
028000       EVALUATE IN-OBHUV-IDPTYP                                           
028100       WHEN '002'                                                         
028200         PERFORM BA-HUVUD                                                 
028300       WHEN '003'                                                         
028400         PERFORM BB-ENTYDIG                                               
028500       WHEN '004'                                                         
028600         PERFORM BC-EJ-ENTYDIG                                            
028700       WHEN '005'                                                         
028800         PERFORM BD-ERS-TEXT                                              
028900       WHEN '006'                                                         
029000         PERFORM BE-KVANTANP                                              
029100       WHEN '007'                                                         
029200         PERFORM BF-LAG-AVB                                               
029300       WHEN '008'                                                         
029400         PERFORM BG-STOPPAD-RAD                                           
029500       WHEN OTHER                                                         
029600         PERFORM BH-FEL-POSTTYP                                           
029700       END-EVALUATE                                                       
029800       PERFORM S01-LAS-W46100                                             
029900                                                                          
030000     END-PERFORM                                                          
030100     CONTINUE.                                                            
030200     EJECT                                                                
030300 BA-HUVUD SECTION.                                                        
030400*                                     POSTTYP 002                         
030500*                                                                         
030600     MOVE    IN-OBHUV-IDDISTR       TO W-IDDISTR                          
030700     MOVE    IN-OBHUV-IDKUNDNR      TO W-IDKUNDNR                         
030800     MOVE    IN-OBHUV-IDORDNR       TO W-IDORDNR                          
030900     MOVE    ZERO                   TO W-KDLIDEL                          
031000     MOVE    ZERO                   TO W-IDARTNR                          
031100     MOVE    ZERO                   TO W-IDRONR                           
031200     MOVE    ZERO                   TO W-KDRESTR                          
031300     MOVE    ZERO                   TO W-IDLOPNRE                         
031400     MOVE    ZERO                   TO W-IDKORTNR                         
031500     MOVE    W-SORTAREA             TO SORT-TEMP-SORT                     
031600     MOVE    IN-OBHUV-W461002       TO SORT-AREA-HUVUD                    
031700*                                                                         
031800     RELEASE SORT-POST-HUVUD                                              
031900     CONTINUE.                                                            
032000     SKIP3                                                                
032100 BB-ENTYDIG SECTION.                                                      
032200*                                     POSTTYP 003                         
032300*                                                                         
032400     MOVE    IN-OBEN-IDDISTR        TO W-IDDISTR                          
032500     MOVE    IN-OBEN-IDKUNDNR       TO W-IDKUNDNR                         
032600     MOVE    IN-OBEN-IDORDNR        TO W-IDORDNR                          
032700     MOVE    IN-OBEN-KDLIDEL        TO W-KDLIDEL                          
032800     MOVE    IN-OBEN-IDARTNR        TO W-IDARTNR                          
032900     MOVE    IN-OBEN-IDRONR         TO W-IDRONR                           
033000     MOVE    ZERO                   TO W-KDRESTR                          
033100     MOVE    IN-OBEN-IDLOPNRE       TO W-IDLOPNRE                         
033200     MOVE    IN-OBEN-IDKORTNR       TO W-IDKORTNR                         
033300     MOVE    W-SORTAREA             TO SORT-TEMP-SORT                     
033400     MOVE    IN-OBEN-W461003        TO SORT-AREA-ENTYDIG                  
033500*                                                                         
033600     RELEASE SORT-POST-ENTYDIG                                            
033700     CONTINUE.                                                            
033800     EJECT                                                                
033900 BC-EJ-ENTYDIG SECTION.                                                   
034000*                                     POSTTYP 004                         
034100*                                                                         
034200     MOVE    IN-OBEEN-IDDISTR       TO W-IDDISTR                          
034300     MOVE    IN-OBEEN-IDKUNDNR      TO W-IDKUNDNR                         
034400     MOVE    IN-OBEEN-IDORDNR       TO W-IDORDNR                          
034500     MOVE    IN-OBEEN-KDLIDEL       TO W-KDLIDEL                          
034600     MOVE    IN-OBEEN-IDARTNR       TO W-IDARTNR                          
034700     MOVE    IN-OBEEN-IDRONR        TO W-IDRONR                           
034800     MOVE    ZERO                   TO W-KDRESTR                          
034900     MOVE    IN-OBEEN-IDLOPNRE      TO W-IDLOPNRE                         
035000     MOVE    IN-OBEEN-IDKORTNR      TO W-IDKORTNR                         
035100     MOVE    W-SORTAREA             TO SORT-TEMP-SORT                     
035200     MOVE    IN-OBEEN-W461004       TO SORT-AREA-EJENTYDIG                
035300*                                                                         
035400     RELEASE SORT-POST-EJENTYDIG                                          
035500     CONTINUE.                                                            
035600     SKIP3                                                                
035700 BD-ERS-TEXT SECTION.                                                     
035800*                                     POSTTYP 005                         
035900*                                                                         
036000     MOVE    IN-OBTEXT-IDDISTR      TO W-IDDISTR                          
036100     MOVE    IN-OBTEXT-IDKUNDNR     TO W-IDKUNDNR                         
036200     MOVE    IN-OBTEXT-IDORDNR      TO W-IDORDNR                          
036300     MOVE    IN-OBTEXT-KDLIDEL      TO W-KDLIDEL                          
036400     MOVE    IN-OBTEXT-IDARTNR      TO W-IDARTNR                          
036500     MOVE    IN-OBTEXT-IDRONR       TO W-IDRONR                           
036600     MOVE    ZERO                   TO W-KDRESTR                          
036700     MOVE    IN-OBTEXT-IDLOPNRE     TO W-IDLOPNRE                         
036800     MOVE    IN-OBTEXT-IDKORTNR     TO W-IDKORTNR                         
036900     MOVE    W-SORTAREA             TO SORT-TEMP-SORT                     
037000     MOVE    IN-OBTEXT-W461005      TO SORT-AREA-ERS-TEXT                 
037100*                                                                         
037200     RELEASE SORT-POST-ERS-TEXT                                           
037300     CONTINUE.                                                            
037400     EJECT                                                                
037500 BE-KVANTANP SECTION.                                                     
037600*                                     POSTTYP 006                         
037700*                                                                         
037800     MOVE    IN-OBKVAN-IDDISTR      TO W-IDDISTR                          
037900     MOVE    IN-OBKVAN-IDKUNDNR     TO W-IDKUNDNR                         
038000     MOVE    IN-OBKVAN-IDORDNR      TO W-IDORDNR                          
038100     MOVE    IN-OBKVAN-KDLIDEL      TO W-KDLIDEL                          
038200     MOVE    IN-OBKVAN-IDARTNR      TO W-IDARTNR                          
038300     MOVE    IN-OBKVAN-IDRONR       TO W-IDRONR                           
038400     MOVE    IN-OBKVAN-KDRESTR      TO W-KDRESTR                          
038500     MOVE    ZERO                   TO W-IDLOPNRE                         
038600     MOVE    ZERO                   TO W-IDKORTNR                         
038700     MOVE    W-SORTAREA             TO SORT-TEMP-SORT                     
038800     MOVE    IN-OBKVAN-W461006      TO SORT-AREA-KVANTANP                 
038900*                                                                         
039000     RELEASE SORT-POST-KVANTANP                                           
039100     CONTINUE.                                                            
039200     SKIP3                                                                
039300 BF-LAG-AVB SECTION.                                                      
039400*                                     POSTTYP 007                         
039500*                                                                         
039600     MOVE    IN-OBLAG-IDDISTR       TO W-IDDISTR                          
039700     MOVE    IN-OBLAG-IDKUNDNR      TO W-IDKUNDNR                         
039800     MOVE    IN-OBLAG-IDORDNR       TO W-IDORDNR                          
039900     MOVE    IN-OBLAG-KDLIDEL       TO W-KDLIDEL                          
040000     MOVE    IN-OBLAG-IDARTNR       TO W-IDARTNR                          
040100     MOVE    IN-OBLAG-IDRONR        TO W-IDRONR                           
040200     MOVE    IN-OBLAG-KDRESTR       TO W-KDRESTR                          
040300     MOVE    ZERO                   TO W-IDLOPNRE                         
040400     MOVE    ZERO                   TO W-IDKORTNR                         
040500     MOVE    W-SORTAREA             TO SORT-TEMP-SORT                     
040600     MOVE    IN-OBLAG-W461007       TO SORT-AREA-LAG-AVB                  
040700*                                                                         
040800     RELEASE SORT-POST-LAG-AVB                                            
040900     CONTINUE.                                                            
041000     EJECT                                                                
041100 BG-STOPPAD-RAD SECTION.                                                  
041200*                                     POSTTYP 008                         
041300*                                                                         
041400     MOVE    IN-OBSTOP-IDDISTR      TO W-IDDISTR                          
041500     MOVE    IN-OBSTOP-IDKUNDNR     TO W-IDKUNDNR                         
041600     MOVE    IN-OBSTOP-IDORDNR      TO W-IDORDNR                          
041700     MOVE    IN-OBSTOP-KDLIDEL      TO W-KDLIDEL                          
041800     MOVE    IN-OBSTOP-IDARTNR      TO W-IDARTNR                          
041900     MOVE    IN-OBSTOP-IDRONR       TO W-IDRONR                           
042000     MOVE    IN-OBSTOP-KDRESTR      TO W-KDRESTR                          
042100     MOVE    ZERO                   TO W-IDLOPNRE                         
042200     MOVE    ZERO                   TO W-IDKORTNR                         
042300     MOVE    W-SORTAREA             TO SORT-TEMP-SORT                     
042400     MOVE    IN-OBSTOP-W461008      TO SORT-AREA-STOPPAD                  
042500*                                                                         
042600     RELEASE SORT-POST-STOPPAD                                            
042700     CONTINUE.                                                            
042800     SKIP3                                                                
042900 BH-FEL-POSTTYP SECTION.                                                  
043000     DISPLAY 'FEL POSTTYP'                                                
043100     DISPLAY 'POSTTYP ' IN-OBEN-IDPTYP                                    
043200     DISPLAY 'DISTRIKT ' IN-OBEN-IDDISTR                                  
043300     DISPLAY 'KUND ' IN-OBEN-IDKUNDNR                                     
043400     DISPLAY 'ORDER ' IN-OBEN-IDORDNR                                     
043500     DISPLAY 'ARTIKEL ' IN-OBEN-IDARTNR                                   
043600     CONTINUE.                                                            
043700     EJECT                                                                
043800 C-OUTPUT-PROCEDURE SECTION.                                              
043900     SKIP2                                                                
044000     MOVE 'W46107'                   TO POSTSUM-FDNAMN                    
044100     MOVE 'W46105D2'                 TO POSTSUM-DDNAMN2                   
044200     SKIP2                                                                
044300     PERFORM S03-RETURN-SORTFIL                                           
044400     IF      SORTFIL-EOF = NEJ                                            
044500*                           OBS  ANVÄND DEN STÖRSTA AREAN                 
044600       MOVE    SORT-AREA-ENTYDIG       TO E-AREA1                         
044700       MOVE    SORT-TEMP-SORT          TO W-SORTAREA                      
044800       MOVE    W-IDORDNR               TO G-IDORDNR                       
044900       MOVE    W-IDDISTR               TO G-IDDISTR                       
045000       MOVE    W-IDKUNDNR              TO G-IDKUNDNR                      
045100       SKIP2                                                              
045200       PERFORM S03-RETURN-SORTFIL                                         
045300       SKIP2                                                              
045400       PERFORM UNTIL                                                      
045500        NOT ( SORTFIL-EOF = NEJ )                                         
045600         PERFORM UNTIL                                                    
045700          NOT ( SORTFIL-EOF = NEJ AND SORT-IDORDNR = G-IDORDNR            
045800            AND SORT-IDDISTR = G-IDDISTR AND SORT-IDKUNDNR =              
045900            G-IDKUNDNR )                                                  
046000           PERFORM UNTIL                                                  
046100            NOT ( SORTFIL-EOF = NEJ AND SO-OBHUV-IDPTYP = '002'           
046200              AND SORT-IDORDNR = G-IDORDNR AND SORT-IDDISTR =             
046300              G-IDDISTR AND SORT-IDKUNDNR = G-IDKUNDNR )                  
046400             PERFORM S03-RETURN-SORTFIL                                   
046500           END-PERFORM                                                    
046600           PERFORM CA-SKRIV-W46107                                        
046700                                                                          
046800           MOVE  SORT-AREA-ENTYDIG    TO E-AREA1                          
046900           MOVE  SORT-TEMP-SORT       TO  W-SORTAREA                      
047000                                                                          
047100           IF      SORTFIL-EOF = NEJ                                      
047200             PERFORM S03-RETURN-SORTFIL                                   
047300           END-IF                                                         
047400         END-PERFORM                                                      
047500         IF      SORTFIL-EOF = NEJ                                        
047600           PERFORM CA-SKRIV-W46107                                        
047700*                                                                         
047800           MOVE    SORT-AREA-ENTYDIG       TO E-AREA1                     
047900           MOVE    SORT-TEMP-SORT          TO W-SORTAREA                  
048000           MOVE    W-IDORDNR               TO G-IDORDNR                   
048100           MOVE    W-IDDISTR               TO G-IDDISTR                   
048200           MOVE    W-IDKUNDNR              TO G-IDKUNDNR                  
048300           MOVE    JA                      TO WS-HUVUD-SAKNAS             
048400           PERFORM S03-RETURN-SORTFIL                                     
048500         END-IF                                                           
048600       END-PERFORM                                                        
048700       PERFORM CA-SKRIV-W46107                                            
048800     END-IF                                                               
048900     CONTINUE.                                                            
049000     EJECT                                                                
049100 CA-SKRIV-W46107 SECTION.                                                 
049200*                                                                         
049300     PERFORM  CAK-EV-EXTRA-HUVUD                                          
049400*                                                                         
049500     EVALUATE H-OBHUV-IDPTYP                                              
049600     WHEN '002'                                                           
049700       PERFORM CAA-HUVUD                                                  
049800     WHEN '003'                                                           
049900       PERFORM CAB-ENTYDIG                                                
050000     WHEN '004'                                                           
050100       PERFORM CAC-EJ-ENTYDIG                                             
050200     WHEN '005'                                                           
050300       PERFORM CAD-ERS-TEXT                                               
050400     WHEN '006'                                                           
050500       PERFORM CAE-KVANTANP                                               
050600     WHEN '007'                                                           
050700       PERFORM CAF-LAG-AVB                                                
050800     WHEN '008'                                                           
050900       PERFORM CAG-STOPPAD-RAD                                            
051000     WHEN OTHER                                                           
051100       PERFORM CAH-FEL-POSTTYP                                            
051200     END-EVALUATE                                                         
051300     CONTINUE.                                                            
051400     EJECT                                                                
051500 CAA-HUVUD SECTION.                                                       
051600     MOVE    H-OBHUV-IDDISTR       TO     H-SOR0-IDDISTR                  
051700     MOVE    H-OBHUV-IDKUNDNR      TO     H-SOR0-IDKUNDNR                 
051800     MOVE    ZERO                  TO     H-SOR0-IDRONR                   
051900     MOVE    ZERO                  TO     H-SOR0-TIRODAT                  
052000     MOVE    SPACE                 TO     H-OBHUV-BEVARREF                
052100     MOVE    SPACE                 TO     H-OBHUV-BEVOLREF                
052200     MOVE    '002'                 TO     H-SOR0-IDPTYP                   
052300     ADD     +1                    TO     WS-LOPNR                        
052400     MOVE    WS-LOPNR              TO     H-SOR0-IDLOPNR                  
052500     WRITE   UT-POST               FROM   AREA-HUVUD                      
052600     MOVE    H-OBHUV-IDPTYP        TO     POSTSUM-TRANSTYP                
052700     CALL    POSTSUM               USING  POSTSUM-PARM                    
052800     CONTINUE.                                                            
052900     SKIP3                                                                
053000 CAB-ENTYDIG SECTION.                                                     
053100     MOVE    E-OBEN-IDDISTR        TO     E-SOR0-IDDISTR                  
053200     MOVE    E-OBEN-IDKUNDNR       TO     E-SOR0-IDKUNDNR                 
053300     MOVE    E-OBEN-IDRONR         TO     E-SOR0-IDRONR                   
053400     MOVE    E-OBEN-TIRODAT        TO     E-SOR0-TIRODAT                  
053500     MOVE    '002'                 TO     E-SOR0-IDPTYP                   
053600     ADD     +1                    TO     WS-LOPNR                        
053700     MOVE    WS-LOPNR              TO     E-SOR0-IDLOPNR                  
053800     WRITE   UT-POST1              FROM   AREA-ENTYDIG                    
053900     MOVE    E-OBEN-IDPTYP         TO     POSTSUM-TRANSTYP                
054000     CALL    POSTSUM               USING  POSTSUM-PARM                    
054100     CONTINUE.                                                            
054200     EJECT                                                                
054300 CAC-EJ-ENTYDIG SECTION.                                                  
054400     MOVE    J-OBEEN-IDDISTR       TO     J-SOR0-IDDISTR                  
054500     MOVE    J-OBEEN-IDKUNDNR      TO     J-SOR0-IDKUNDNR                 
054600     MOVE    J-OBEEN-IDRONR        TO     J-SOR0-IDRONR                   
054700     MOVE    J-OBEEN-TIRODAT       TO     J-SOR0-TIRODAT                  
054800     MOVE    '002'                 TO     J-SOR0-IDPTYP                   
054900     ADD     +1                    TO     WS-LOPNR                        
055000     MOVE    WS-LOPNR              TO     J-SOR0-IDLOPNR                  
055100     WRITE   UT-POST2              FROM   AREA-EJENTYDIG                  
055200     MOVE    J-OBEEN-IDPTYP        TO     POSTSUM-TRANSTYP                
055300     CALL    POSTSUM               USING  POSTSUM-PARM                    
055400     CONTINUE.                                                            
055500     SKIP3                                                                
055600 CAD-ERS-TEXT SECTION.                                                    
055700     MOVE    T-OBTEXT-IDDISTR      TO     T-SOR0-IDDISTR                  
055800     MOVE    T-OBTEXT-IDKUNDNR     TO     T-SOR0-IDKUNDNR                 
055900     MOVE    T-OBTEXT-IDRONR       TO     T-SOR0-IDRONR                   
056000     MOVE    T-OBTEXT-TIRODAT      TO     T-SOR0-TIRODAT                  
056100     MOVE    '002'                 TO     T-SOR0-IDPTYP                   
056200     ADD     +1                    TO     WS-LOPNR                        
056300     MOVE    WS-LOPNR              TO     T-SOR0-IDLOPNR                  
056400     WRITE   UT-POST3              FROM   AREA-ERS-TEXT                   
056500     MOVE    T-OBTEXT-IDPTYP       TO     POSTSUM-TRANSTYP                
056600     CALL    POSTSUM               USING  POSTSUM-PARM                    
056700     CONTINUE.                                                            
056800     EJECT                                                                
056900 CAE-KVANTANP SECTION.                                                    
057000     MOVE    K-OBKVAN-IDDISTR      TO     K-SOR0-IDDISTR                  
057100     MOVE    K-OBKVAN-IDKUNDNR     TO     K-SOR0-IDKUNDNR                 
057200     MOVE    K-OBKVAN-IDRONR       TO     K-SOR0-IDRONR                   
057300     MOVE    K-OBKVAN-TIRODAT      TO     K-SOR0-TIRODAT                  
057400     MOVE    '002'                 TO     K-SOR0-IDPTYP                   
057500     ADD     +1                    TO     WS-LOPNR                        
057600     MOVE    WS-LOPNR              TO     K-SOR0-IDLOPNR                  
057700     WRITE   UT-POST4              FROM   AREA-KVANTANP                   
057800     MOVE    K-OBKVAN-IDPTYP       TO     POSTSUM-TRANSTYP                
057900     CALL    POSTSUM               USING  POSTSUM-PARM                    
058000     CONTINUE.                                                            
058100     SKIP3                                                                
058200 CAF-LAG-AVB SECTION.                                                     
058300     MOVE    L-OBLAG-IDDISTR       TO     L-SOR0-IDDISTR                  
058400     MOVE    L-OBLAG-IDKUNDNR      TO     L-SOR0-IDKUNDNR                 
058500     MOVE    L-OBLAG-IDRONR        TO     L-SOR0-IDRONR                   
058600     MOVE    L-OBLAG-TIRODAT       TO     L-SOR0-TIRODAT                  
058700     MOVE    '002'                 TO     L-SOR0-IDPTYP                   
058800     ADD     +1                    TO     WS-LOPNR                        
058900     MOVE    WS-LOPNR              TO     L-SOR0-IDLOPNR                  
059000     WRITE   UT-POST5              FROM   AREA-LAG-AVB                    
059100     MOVE    L-OBLAG-IDPTYP        TO     POSTSUM-TRANSTYP                
059200     CALL    POSTSUM               USING  POSTSUM-PARM                    
059300     CONTINUE.                                                            
059400     EJECT                                                                
059500 CAG-STOPPAD-RAD SECTION.                                                 
059600     MOVE    S-OBSTOP-IDDISTR      TO     S-SOR0-IDDISTR                  
059700     MOVE    S-OBSTOP-IDKUNDNR     TO     S-SOR0-IDKUNDNR                 
059800     MOVE    S-OBSTOP-IDRONR       TO     S-SOR0-IDRONR                   
059900     MOVE    S-OBSTOP-TIRODAT      TO     S-SOR0-TIRODAT                  
060000     MOVE    '002'                 TO     S-SOR0-IDPTYP                   
060100     ADD     +1                    TO     WS-LOPNR                        
060200     MOVE    WS-LOPNR              TO     S-SOR0-IDLOPNR                  
060300     WRITE   UT-POST6              FROM   AREA-STOPPAD                    
060400     MOVE    S-OBSTOP-IDPTYP       TO     POSTSUM-TRANSTYP                
060500     CALL    POSTSUM               USING  POSTSUM-PARM                    
060600     CONTINUE.                                                            
060700     SKIP3                                                                
060800 CAH-FEL-POSTTYP SECTION.                                                 
060900     DISPLAY 'FEL POSTTYP (CAH-SECTION)'                                  
061000     DISPLAY 'POSTTYP ' E-OBEN-IDPTYP                                     
061100     DISPLAY 'DISTRIKT ' E-OBEN-IDDISTR                                   
061200     DISPLAY 'KUND ' E-OBEN-IDKUNDNR                                      
061300     DISPLAY 'ORDER ' E-OBEN-IDORDNR                                      
061400     DISPLAY 'ARTIKEL ' E-OBEN-IDARTNR                                    
061500     CONTINUE.                                                            
061600     EJECT                                                                
061700 CAK-EV-EXTRA-HUVUD SECTION.                                              
061800*                                                                         
061900*                          EV SKALL ETT ORDERHUVUD SKAPAS                 
062000     IF    H-OBHUV-IDPTYP = '002'                                         
062100       MOVE    W-IDDISTR            TO WS-HUV-IDDISTR                     
062200       MOVE    W-IDKUNDNR           TO WS-HUV-IDKUNDNR                    
062300       MOVE    W-IDORDNR            TO WS-HUV-IDORDNR                     
062400     ELSE                                                                 
062500       IF      W-IDDISTR            =  WS-HUV-IDDISTR  AND                
062600       W-IDKUNDNR           =  WS-HUV-IDKUNDNR AND                        
062700       W-IDORDNR            =  WS-HUV-IDORDNR                             
062800*                 TOM SATS                                                
062900         CONTINUE                                                         
063000       ELSE                                                               
063100         EVALUATE H-OBHUV-IDPTYP                                          
063200         WHEN '003'                                                       
063300           PERFORM CAKA-ERS-ENT                                           
063400         WHEN '004'                                                       
063500           PERFORM CAKB-ERS-TEXT                                          
063600         WHEN '005'                                                       
063700           PERFORM CAKC-ERS-EJENT                                         
063800         WHEN '006'                                                       
063900           PERFORM CAKD-KVANT                                             
064000         WHEN '007'                                                       
064100           PERFORM CAKE-LAG-AVB                                           
064200         WHEN '008'                                                       
064300           PERFORM CAKF-STOPPAD-RAD                                       
064400         WHEN OTHER                                                       
064500           PERFORM CAKG-FEL-POSTTYP                                       
064600         END-EVALUATE                                                     
064700         MOVE    W-IDDISTR            TO WS-HUV-IDDISTR                   
064800         MOVE    W-IDKUNDNR           TO WS-HUV-IDKUNDNR                  
064900         MOVE    W-IDORDNR            TO WS-HUV-IDORDNR                   
065000         MOVE    '002'                 TO     X-SOR0-IDPTYP               
065100         ADD     +1                    TO     WS-LOPNR                    
065200         MOVE    WS-LOPNR              TO     X-SOR0-IDLOPNR              
065300         MOVE    '002'                 TO     X-OBHUV-IDPTYP              
065400         MOVE    SPACE                 TO     X-OBHUV-BEVARREF            
065500         MOVE    SPACE                 TO     X-OBHUV-BEVOLREF            
065600         MOVE    ZERO                  TO     X-OBHUV-KDORDKL             
065700         WRITE   UT-POST1              FROM   WS-EXTRA-HUVUD-POST         
065800         MOVE    '002'                 TO     POSTSUM-TRANSTYP            
065900         CALL    POSTSUM               USING  POSTSUM-PARM                
066000       END-IF                                                             
066100     END-IF                                                               
066200     CONTINUE.                                                            
066300     EJECT                                                                
066400     SKIP3                                                                
066500 CAKA-ERS-ENT SECTION.                                                    
066600*                                                                         
066700* HUVUD SKALL SKAPAS FÖR DIVERSEORDER SOM ORDERBEKR VID DORELEASE         
066800*                                                                         
066900     MOVE    E-OBEN-IDDISTR        TO     X-SOR0-IDDISTR                  
067000     MOVE    E-OBEN-IDKUNDNR       TO     X-SOR0-IDKUNDNR                 
067100     MOVE    E-OBEN-IDORDNR        TO     X-SOR0-IDRONR                   
067200     MOVE    E-OBEN-TIRODAT        TO     X-SOR0-TIRODAT                  
067300     MOVE    E-OBEN-IDDISTR        TO     X-OBHUV-IDDISTR                 
067400     MOVE    E-OBEN-IDKUNDNR       TO     X-OBHUV-IDKUNDNR                
067500     MOVE    E-OBEN-IDORDNR        TO     X-OBHUV-IDORDNR                 
067600     MOVE    E-OBEN-KDFRAKT        TO     X-OBHUV-KDFRAKT                 
067700     MOVE    E-OBEN-BEVOLREF       TO     X-OBHUV-BEVOLREF                
067800     MOVE    E-OBEN-TIRODAT        TO     X-OBHUV-TIORDREG                
067900     MOVE    E-OBEN-KDFAKTYP       TO     X-OBHUV-KDFAKTYP                
068000     CONTINUE.                                                            
068100     EJECT                                                                
068200 CAKB-ERS-TEXT SECTION.                                                   
068300*                                                                         
068400* HUVUD SKALL SKAPAS FÖR DIVERSEORDER SOM ORDERBEKR VID DORELEASE         
068500*                                                                         
068600     MOVE    T-OBTEXT-IDDISTR      TO     X-SOR0-IDDISTR                  
068700     MOVE    T-OBTEXT-IDKUNDNR     TO     X-SOR0-IDKUNDNR                 
068800     MOVE    T-OBTEXT-IDORDNR      TO     X-SOR0-IDRONR                   
068900     MOVE    T-OBTEXT-TIRODAT      TO     X-SOR0-TIRODAT                  
069000     MOVE    T-OBTEXT-IDDISTR      TO     X-OBHUV-IDDISTR                 
069100     MOVE    T-OBTEXT-IDKUNDNR     TO     X-OBHUV-IDKUNDNR                
069200     MOVE    T-OBTEXT-IDORDNR      TO     X-OBHUV-IDORDNR                 
069300     MOVE    T-OBTEXT-KDFRAKT      TO     X-OBHUV-KDFRAKT                 
069400     MOVE    T-OBTEXT-BEVOLREF     TO     X-OBHUV-BEVOLREF                
069500     MOVE    T-OBTEXT-TIRODAT      TO     X-OBHUV-TIORDREG                
069600     MOVE    T-OBTEXT-KDFAKTYP     TO     X-OBHUV-KDFAKTYP                
069700     CONTINUE.                                                            
069800     EJECT                                                                
069900 CAKC-ERS-EJENT SECTION.                                                  
070000*                                                                         
070100* HUVUD SKALL SKAPAS FÖR DIVERSEORDER SOM ORDERBEKR VID DORELEASE         
070200*                                                                         
070300     MOVE    J-OBEEN-IDDISTR       TO     X-SOR0-IDDISTR                  
070400     MOVE    J-OBEEN-IDKUNDNR      TO     X-SOR0-IDKUNDNR                 
070500     MOVE    J-OBEEN-IDORDNR       TO     X-SOR0-IDRONR                   
070600     MOVE    J-OBEEN-TIRODAT       TO     X-SOR0-TIRODAT                  
070700     MOVE    J-OBEEN-IDDISTR       TO     X-OBHUV-IDDISTR                 
070800     MOVE    J-OBEEN-IDKUNDNR      TO     X-OBHUV-IDKUNDNR                
070900     MOVE    J-OBEEN-IDORDNR       TO     X-OBHUV-IDORDNR                 
071000     MOVE    J-OBEEN-KDFRAKT       TO     X-OBHUV-KDFRAKT                 
071100     MOVE    J-OBEEN-BEVOLREF      TO     X-OBHUV-BEVOLREF                
071200     MOVE    J-OBEEN-TIRODAT       TO     X-OBHUV-TIORDREG                
071300     MOVE    J-OBEEN-KDFAKTYP      TO     X-OBHUV-KDFAKTYP                
071400     CONTINUE.                                                            
071500     EJECT                                                                
071600 CAKD-KVANT SECTION.                                                      
071700*                                                                         
071800* HUVUD SKALL SKAPAS FÖR DIVERSEORDER SOM ORDERBEKR VID DORELEASE         
071900*                                                                         
072000     MOVE    K-OBKVAN-IDDISTR      TO     X-SOR0-IDDISTR                  
072100     MOVE    K-OBKVAN-IDKUNDNR     TO     X-SOR0-IDKUNDNR                 
072200     MOVE    K-OBKVAN-IDORDNR      TO     X-SOR0-IDRONR                   
072300     MOVE    K-OBKVAN-TIRODAT      TO     X-SOR0-TIRODAT                  
072400     MOVE    K-OBKVAN-IDDISTR      TO     X-OBHUV-IDDISTR                 
072500     MOVE    K-OBKVAN-IDKUNDNR     TO     X-OBHUV-IDKUNDNR                
072600     MOVE    K-OBKVAN-IDORDNR      TO     X-OBHUV-IDORDNR                 
072700     MOVE    K-OBKVAN-KDFRAKT      TO     X-OBHUV-KDFRAKT                 
072800     MOVE    K-OBKVAN-BEVOLREF     TO     X-OBHUV-BEVOLREF                
072900     MOVE    K-OBKVAN-TIRODAT      TO     X-OBHUV-TIORDREG                
073000     MOVE    K-OBKVAN-KDFAKTYP     TO     X-OBHUV-KDFAKTYP                
073100     CONTINUE.                                                            
073200     EJECT                                                                
073300 CAKE-LAG-AVB SECTION.                                                    
073400* HUVUD SKAPAS FÖR RADER FRÅN PACKNINGEN                                  
073500* OCH FÖR DIVERSEORDER SOM ORDERBEKR VID DO RELEASE                       
073600*                                                                         
073700     MOVE    L-OBLAG-IDDISTR       TO     X-SOR0-IDDISTR                  
073800     MOVE    L-OBLAG-IDKUNDNR      TO     X-SOR0-IDKUNDNR                 
073900     MOVE    L-OBLAG-IDORDNR       TO     X-SOR0-IDRONR                   
074000     MOVE    L-OBLAG-TIORDREG      TO     X-SOR0-TIRODAT                  
074100     MOVE    L-OBLAG-IDDISTR       TO     X-OBHUV-IDDISTR                 
074200     MOVE    L-OBLAG-IDKUNDNR      TO     X-OBHUV-IDKUNDNR                
074300     MOVE    L-OBLAG-IDORDNR       TO     X-OBHUV-IDORDNR                 
074400     MOVE    L-OBLAG-KDFRAKT       TO     X-OBHUV-KDFRAKT                 
074500     MOVE    L-OBLAG-BEVOLREF      TO     X-OBHUV-BEVOLREF                
074600     MOVE    L-OBLAG-TIORDREG      TO     X-OBHUV-TIORDREG                
074700     MOVE    L-OBLAG-KDFAKTYP      TO     X-OBHUV-KDFAKTYP                
074800     CONTINUE.                                                            
074900     EJECT                                                                
075000 CAKF-STOPPAD-RAD SECTION.                                                
075100* HUVUD SKAPAS FÖR  ANNULL RO                                             
075200* OCH FÖR DIVERSEORDER SOM ORDERBEKR VID DO RELEASE                       
075300*                                                                         
075400     MOVE    S-OBSTOP-IDDISTR      TO     X-SOR0-IDDISTR                  
075500     MOVE    S-OBSTOP-IDKUNDNR     TO     X-SOR0-IDKUNDNR                 
075600     MOVE    S-OBSTOP-IDORDNR      TO     X-SOR0-IDRONR                   
075700     MOVE    S-OBSTOP-TIORDREG     TO     X-SOR0-TIRODAT                  
075800     MOVE    S-OBSTOP-IDDISTR      TO     X-OBHUV-IDDISTR                 
075900     MOVE    S-OBSTOP-IDKUNDNR     TO     X-OBHUV-IDKUNDNR                
076000     MOVE    S-OBSTOP-IDORDNR      TO     X-OBHUV-IDORDNR                 
076100     MOVE    S-OBSTOP-KDFRAKT      TO     X-OBHUV-KDFRAKT                 
076200     MOVE    S-OBSTOP-BEVOLREF     TO     X-OBHUV-BEVOLREF                
076300     MOVE    S-OBSTOP-TIORDREG     TO     X-OBHUV-TIORDREG                
076400     MOVE    S-OBSTOP-KDFAKTYP     TO     X-OBHUV-KDFAKTYP                
076500     CONTINUE.                                                            
076600     SKIP3                                                                
076700 CAKG-FEL-POSTTYP SECTION.                                                
076800     DISPLAY 'FEL POSTTYP (CAKG-SECTION)'                                 
076900     DISPLAY 'POSTTYP ' E-OBEN-IDPTYP                                     
077000     DISPLAY 'DISTRIKT ' E-OBEN-IDDISTR                                   
077100     DISPLAY 'KUND ' E-OBEN-IDKUNDNR                                      
077200     DISPLAY 'ORDER ' E-OBEN-IDORDNR                                      
077300     DISPLAY 'ARTIKEL ' E-OBEN-IDARTNR                                    
077400     CONTINUE.                                                            
077500     EJECT                                                                
077600 S01-LAS-W46100 SECTION.                                                  
077700     SKIP2                                                                
077800     READ  W46100-BEKR    INTO IN-AREA                                    
077900*     READ  W46100-BEKR                                                   
078000                      AT END MOVE JA TO W46100-EOF                        
078100     END-READ                                                             
078200                                                                          
078300     IF W46100-EOF = NEJ                                                  
078400                                                                          
078500       MOVE 'W46100'            TO POSTSUM-FDNAMN                         
078600       MOVE 'W46105D1'          TO POSTSUM-DDNAMN2                        
078700       MOVE IN-OBHUV-IDPTYP     TO POSTSUM-TRANSTYP                       
078800       CALL POSTSUM   USING POSTSUM-PARM                                  
078900                                                                          
079000     END-IF                                                               
079100     CONTINUE.                                                            
079200     SKIP3                                                                
079300                                                                          
079400 S03-RETURN-SORTFIL SECTION.                                              
079500     SKIP2                                                                
079600     RETURN SORTFIL                                                       
079700                      AT END MOVE JA TO SORTFIL-EOF                       
079800     END-RETURN                                                           
079900     CONTINUE.                                                            
080000                                                                          
080100     EJECT                                                                
080200     SKIP3                                                                
080300 Z-FINIT SECTION.                                                         
080400     SKIP2                                                                
080500     CLOSE W46100-BEKR W46105-UT                                          
080600     SKIP2                                                                
080700*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
080800*                                    SKRIVNA POSTER                       
080900                                                                          
081000     MOVE 'S' TO POSTSUM-OPKOD                                            
081100     CALL POSTSUM USING POSTSUM-PARM                                      
081200     CONTINUE.                                                            

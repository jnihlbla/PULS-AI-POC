000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4798300.                                                
000400*AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500*DATE-WRITTEN.   92/03/06.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*      LÄSER NER WDE4 OCH SELEKTERAR UT LITE DATA TILL TVÅ FILER          
001100*      MHA SB.                                                            
001200*                                                                         
001300*      PROGRAMMET LÄSER WDE4                                              
001400*                                                                         
001500*                                                                         
001600     EJECT                                                                
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- UTFIL                                                      
002500     SELECT W47922                    ASSIGN TO W47983D1.                 
002600     SELECT W47923                    ASSIGN TO W47983D2.                 
002700     SELECT W47923B                   ASSIGN TO W47983D3.                 
002800     SELECT W47983                    ASSIGN TO W47983D4.                 
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W47922                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  POST -COPY W47922 -PRE  U22-  -L.                                    
003900     EJECT                                                                
004000 FD  W47923                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300     SKIP2                                                                
004400*01  POST -COPY W47923 -PRE  U23-  -L.                                    
004500     EJECT                                                                
004600 FD  W47923B                                                              
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900     SKIP2                                                                
005000*01  POST -COPY W47923B -PRE  U23B-  -L.                                  
005100     EJECT                                                                
005200 FD  W47983                                                               
005300     RECORDING       V                                                    
005400     BLOCK CONTAINS  0.                                                   
005500     SKIP2                                                                
005600 01  U83-POST                    PIC X(39).                               
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900     SKIP2                                                                
006000                                                                          
006100*    -- CHECKED BY WY2000                                                 
006200 77  IDPGM                       PIC X(8)    VALUE 'W4798300'.            
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500 01  WS-KORD-TIBEGPAC            PIC S9(7)   COMP-3 VALUE +0.             
006600 77  WS-IDDC                     PIC X(2).                                
006700     88 CDC                                  VALUE '11'.                  
006800                                                                          
006900 01  WS-FIRST-TIME               PIC X       VALUE 'N'.                   
007000 01  WS-PREV-SEGM                PIC X(8)    VALUE SPACE.                 
007100 01  U83-HEADER                  PIC X(39)   VALUE                        
007200                'DISTRICT;CUSTOMER;ORDER;PRODNR;PLK LIST'.                
007300 01  U83-AREA.                                                            
007400     03 U83-IDDISTR              PIC 9(5).                                
007500     03 FILLER                   PIC X       VALUE ';'.                   
007600     03 U83-IDKUNDNR             PIC 9(7).                                
007700     03 FILLER                   PIC X       VALUE ';'.                   
007800     03 U83-IDKUNDRF             PIC X(10).                               
007900     03 FILLER                   PIC X       VALUE ';'.                   
008000     03 U83-IDPRODNR             PIC 9(7).                                
008100     03 FILLER                   PIC X       VALUE ';'.                   
008200     03 U83-IDPLKLST             PIC 9(3).                                
008300     03 FILLER                   PIC X(3)    VALUE SPACE.                 
008400                                                                          
008500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008600 01  FILLER REDEFINES DAGENS-DATUM.                                       
008700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009000     EJECT                                                                
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200*                                                                         
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
009700     SKIP2                                                                
009800 01  FELTEXT.                                                             
009900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL POSTSUM                                          
010300*                                                                         
010400*01  -COPY W0005   -PRE  POSTSUM-                                         
010500     EJECT                                                                
010600*    ------- PARAMETRAR TILL DATUMKORT                                    
010700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W47979'.              
010800     SKIP2                                                                
010900 01  DATUMKORT-ID            PIC X(6)   VALUE '000001'.                   
011000                                                                          
011100*    -COPY WDATKORT                                                       
011200     EJECT                                                                
011300 01  UT-AREA-START               PIC X(24)   VALUE                        
011400                                 'UT-AREA-START  '.                       
011500     SKIP2                                                                
011600                                                                          
011700*01  AREA -COPY W47922    -PRE U22-                                       
011800     EJECT                                                                
011900*01  AREA -COPY W47923    -PRE U23-                                       
012000     EJECT                                                                
012100*01  AREA -COPY W47923B   -PRE U23B-                                      
012200     EJECT                                                                
012300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012400*                                                                         
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012700     SKIP3                                                                
012800*    --- STATUS-KOD FRÅN IMS                                              
012801 01  NYCKLAR-TILL-DLI.                                                    
012810     03  W-WDQ301-KEY-X.                                                  
012820         05  W-WDQ301-IDORDER    PIC S9(7)   VALUE ZERO  COMP-3.          
012830         05  W-WDQ301-IDDC       PIC X(2).                                
012840         05  W-WDQ301-IDPRODNR   PIC S9(7)   VALUE ZERO  COMP-3.          
012850         05  W-WDQ301-IDPLKLST   PIC S9(3)   VALUE ZERO  COMP-3.          
012851                                                                          
012852   03    W-WDQ201-X.                                                      
012853     05    W-201-IDORDER         PIC S9(7) COMP-3.                        
012854*                                                                         
012860     03  W-WDQ211-KEY-X.                                                  
012870         05  W-211-IDDC          PIC X(2).                                
012880         05  W-211-IDLEVNR       PIC X(5)   VALUE SPACE.                  
012890*                                                                         
012900 01  STATUS-WS                   PIC XX.                                  
013000     88  SEGMENT-FINNS                       VALUE '  '.                  
013100     88  BASEN-SLUT                          VALUE 'GB'.                  
013200     SKIP2                                                                
013300 01  GODK-STATUSKODER.                                                    
013400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013500     SKIP3                                                                
013600 01  SSA1                        PIC X(64).                               
013700 01  SSA2                        PIC X(64).                               
013800     EJECT                                                                
013900*    --- IMS FUNKTIONSKODER                                               
014000*01  -COPY W0003                                                          
014100     EJECT                                                                
014200*    ---  DLI INPUT-OUTPUT AREA                                           
014210 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDQ301'.        
014220 01  DLI-IO-WDQ301.                                                       
014230*    03 -COPY WDQ301                                                      
014240     SKIP2                                                                
014250 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDQ211'.        
014260 01  DLI-IO-WDQ211.                                                       
014270*    03 -COPY WDQ211                                                      
014280     SKIP2                                                                
014300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
014400     SKIP3                                                                
014500 01  DLI-IO-AREA.                                                         
014600     03  IO-AREA                 PIC X(500)  VALUE SPACE.                 
014700     03  IO-E401    REDEFINES IO-AREA.                                    
014800*        05  -COPY WDE401                                                 
014900     03  IO-E411    REDEFINES IO-AREA.                                    
015000*        05  -COPY WDE411                                                 
015100     03  IO-E421    REDEFINES IO-AREA.                                    
015200*        05  -COPY WDE421                                                 
015300                                                                          
015400     EJECT                                                                
015500 LINKAGE SECTION.                                                         
015600                                                                          
015700     EJECT                                                                
015800*01  -COPY W0008  -PRE WDE4-                                              
015900     05  FILLER                  PIC X.                                   
015910*01  -COPY W0008  -PRE WDQ3-                                              
015920     05  FILLER                  PIC X.                                   
015930*01  -COPY W0008  -PRE WDQ2-                                              
015940     05  FILLER                  PIC X.                                   
016000     EJECT                                                                
016100 PROCEDURE DIVISION  USING WDE4-PCB WDQ3-PCB WDQ2-PCB.                    
016200                                                                          
016300     PERFORM A-INIT                                                       
016400     PERFORM IMS-GET-WDE4                                                 
016500     PERFORM UNTIL BASEN-SLUT                                             
016600                                                                          
016700        EVALUATE WDE4-SEG-NAME-FB                                         
016800           WHEN 'WDE401  '                                                
016810             MOVE KORD-IDDC     TO W-WDQ301-IDDC                          
016811             MOVE KORD-IDORDER  TO W-WDQ301-IDORDER                       
016820             MOVE KORD-IDPRODNR TO W-WDQ301-IDPRODNR                      
016830             MOVE KORD-IDPLKLST TO W-WDQ301-IDPLKLST                      
016840             PERFORM IMS-GU-WDQ301                                        
016850                                                                          
016860             IF SEGMENT-FINNS                                             
016870               MOVE ODEL-DARFS (3:6)   TO U22-TIBEGPAC                    
016880                                          U23B-TIBEGPAC                   
016881               MOVE ODEL-DARFS (9:4)   TO U23B-TIBEGTID                   
016882                                                                          
016890             ELSE                                                         
016891               MOVE ZERO        TO U22-TIBEGPAC                           
016892                                   U23B-TIBEGPAC                          
016893                                   U23B-TIBEGTID                          
016894             END-IF                                                       
016900             PERFORM B-FLYTTA-WDE401-INFO                                 
017000             MOVE KORD-TIBEGPAC TO WS-KORD-TIBEGPAC                       
017010             MOVE KORD-IDORDER  TO W-201-IDORDER                          
017020             MOVE KORD-IDDC     TO W-211-IDDC                             
017100           WHEN 'WDE411  '                                                
017101              MOVE ORAD-IDLEVNR  TO W-211-IDLEVNR                         
017110              IF ORAD-FLDIRLEV = JA                                       
017113**               *DDGS ORDER.                                             
017141                 PERFORM IMS-GU-WDQ211                                    
017142                 IF SEGMENT-FINNS                                         
017146                    MOVE DIRL-TISKEPPN-DDC TO U22-TIBEGPAC                
017147                                              U23B-TIBEGPAC               
017148                    MOVE ZERO              TO U23B-TIBEGTID               
017149                 END-IF                                                   
017150              END-IF                                                      
017200              PERFORM D-FLYTTA-WDE411-INFO                                
017300              PERFORM S11-SKRIV-W47922                                    
017400              IF ORAD-KVBEART > ORAD-KVLEVART                             
017500             AND ORAD-KDRADSTA = +4                                       
017600             AND ORAD-FLFYSAVV = JA                                       
017700             AND WS-KORD-TIBEGPAC = DAGENS-DATUM                          
017800                 PERFORM S12-SKRIV-W47923                                 
017900              END-IF                                                      
018000           WHEN 'WDE421  '                                                
018100              PERFORM E-FLYTTA-WDE421-INFO                                
018200              PERFORM S13-SKRIV-W47923B                                   
018300        END-EVALUATE                                                      
018400                                                                          
018500        MOVE WDE4-SEG-NAME-FB    TO WS-PREV-SEGM                          
018600        PERFORM IMS-GET-WDE4                                              
018700     END-PERFORM                                                          
018800                                                                          
018900     PERFORM Z-FINIT                                                      
019000                                                                          
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 A-INIT SECTION.                                                          
019600                                                                          
019700     OPEN OUTPUT W47922                                                   
019800                 W47923                                                   
019900                 W47923B                                                  
020000                 W47983                                                   
020100                                                                          
020200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020300                                                                          
020400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
020500                                                                          
020600     MOVE D-AAR                TO  DAGENS-DATUM-AAR                       
020700     MOVE D-MAANAD             TO  DAGENS-DATUM-MAANAD                    
020800     MOVE D-DAG                TO  DAGENS-DATUM-DAG                       
020900                                                                          
021000     MOVE SPACE                TO U23B-IDPTYP                             
021100     MOVE '0000'               TO U23B-IDPRC                              
021200     MOVE ZERO                 TO U23B-ADLAGOMR-DLB                       
021210                                  U23B-ADGANG-DLB                         
021220                                  U23B-ADPLATS-DLB                        
021300                                  U23B-DARFS                              
021400                                  U23B-DASUPREF                           
021500                                  U23B-IDFAKT                             
021600                                  U23B-IDFKNGRP                           
021700                                  U23B-IDKOLLI                            
021800                                  U23B-IDORDNR5                           
021900                                  U23B-IDPSN-DC                           
022000                                  U23B-IDSHIFT                            
022100                                  U23B-IDSHIPM                            
022200                                  U23B-KDFARLIG                           
022300                                  U23B-KVLEVART2                          
022400                                  U23B-KVQPACK-0                          
022500                                  U23B-KVQPACK-1                          
022600                                  U23B-KVQPACK-2                          
022700                                  U23B-KVQPACK-3                          
022800                                  U23B-KVQPACK-4                          
022900                                  U23B-KVULOAD                            
023000                                  U23B-PRARTSTD                           
023100                                  U23B-TIBEGTID                           
023200                                  U23B-TISUPTID                           
023300                                  U23B-TIFAKT                             
023400                                  U23B-TIFAKTID                           
023500                                  U23B-TIHHMM                             
023600                                  U23B-TILASTN                            
023700                                  U23B-TILASTID                           
023800                                  U23B-TIPACKN                            
023900                                  U23B-TIPACTID                           
024000                                  U23B-TIREGTID                           
024100                                  U23B-TIREPDAT                           
024200                                  U23B-TISKEPPN                           
024300                                  U23B-TISKPTID                           
024400                                  U23B-TIUTSKR                            
024500                                  U23B-TIUTSTID                           
024600     MOVE SPACE                TO U23B-FLLDCKND                           
024700                                  U23B-IDLBBET                            
024800                                  U23B-IDLEVNR                            
024900                                  U23B-IDTRP                              
025000                                  U23B-IDUSER-PACK                        
025100                                  U23B-KDARTURS                           
025200                                  U23B-KDKOLLI                            
025300                                  U23B-KDPRCGRP                           
025400                                  U23B-KDSORT                             
025500                                  U23B-KDVIA                              
025510                                  U23B-IDVIN                              
025600     MOVE '00000000'           TO U23B-DATRPAVD                           
025700     .                                                                    
025800     EJECT                                                                
025900 B-FLYTTA-WDE401-INFO SECTION.                                            
026000                                                                          
026100     IF WS-PREV-SEGM = WDE4-SEG-NAME-FB                                   
026200       PERFORM BA-CREATE-REPORT                                           
026300     END-IF                                                               
026400                                                                          
026500     MOVE KORD-IDDISTR                    TO U22-IDDISTR                  
026600                                             U23-IDDISTR                  
026700                                             U23B-IDDISTR                 
026800                                             U83-IDDISTR                  
026900     MOVE KORD-IDKUNDNR                   TO U22-IDKUNDNR                 
027000                                             U23-IDKUNDNR                 
027100                                             U23B-IDKUNDNR                
027200                                             U83-IDKUNDNR                 
027300     MOVE KORD-IDKUNDRF                   TO U22-IDKUNDRF                 
027400                                             U83-IDKUNDRF                 
027500     MOVE KORD-IDPRODNR                   TO U23-IDPRODNR                 
027600                                             U23B-IDPRODNR                
027700                                             U83-IDPRODNR                 
027800     MOVE KORD-IDORDER                    TO U22-IDORDER                  
027900                                             U23-IDORDER                  
028000                                             U23B-IDORDER                 
028100     MOVE KORD-IDDC                       TO U22-IDDC                     
028200                                             U23-IDDC                     
028300                                             U23B-IDDC                    
028400                                             WS-IDDC                      
028500     MOVE KORD-KVORDRAD                   TO U22-KVORDRAD                 
028600     MOVE KORD-KVORDRAD-PACK              TO U22-KVORDRAD-PACK            
028700     MOVE KORD-TIBEGPAC                   TO U23-TIBEGPAC                 
029000     MOVE KORD-TIORDREG                   TO U22-TIORDREG                 
029100                                             U23B-TIORDREG                
029200     MOVE KORD-IDPLKLST                   TO U22-IDPLKLST                 
029300                                             U23B-IDPLKLST                
029400                                             U83-IDPLKLST                 
029500     MOVE KORD-IDUSER                     TO U23-IDUSER                   
029600                                             U23B-IDUSER                  
029700     MOVE KORD-KDFAKTYP                   TO U22-KDFAKTYP                 
029800                                             U23B-KDFAKTYP                
029900     MOVE KORD-KDFRAKT                    TO U22-KDFRAKT                  
030000                                             U23B-KDFRAKT                 
030100     MOVE KORD-KDORDKL                    TO U22-KDORDKL                  
030200                                             U23B-KDORDKL                 
030300     MOVE KORD-TIUTSKR                    TO U22-TIUTSKR                  
030400                                                                          
030500     IF KORD-KVORDRAD-LEVPL = ZERO                                        
030600       MOVE NEJ                           TO U22-FLDIRLEV                 
030700     ELSE                                                                 
030800       MOVE JA                            TO U22-FLDIRLEV                 
030900     END-IF                                                               
031000     .                                                                    
031100     EJECT                                                                
031200 BA-CREATE-REPORT SECTION.                                                
031300                                                                          
031400     IF WS-FIRST-TIME = NEJ                                               
031500       MOVE JA                            TO WS-FIRST-TIME                
031600       WRITE U83-POST                   FROM U83-HEADER                   
031700     END-IF                                                               
031800     PERFORM S14-SKRIV-W47983                                             
031900                                                                          
032000     .                                                                    
032100     EJECT                                                                
032200 D-FLYTTA-WDE411-INFO SECTION.                                            
032300     MOVE ORAD-IDPURAD                    TO U22-IDPURAD                  
032400     MOVE ORAD-IDARTNR                    TO U22-IDARTNR                  
032500                                             U23-IDARTNR                  
032600                                             U23B-IDARTNR                 
032700     MOVE ORAD-ADLAGOMR                   TO U22-ADLAGOMR                 
032800                                             U23-ADLAGOMR                 
032900                                             U23B-ADLAGOMR-VERKLIG        
033000     MOVE ORAD-FLRESTN                    TO U22-FLRESTN                  
033100     MOVE ORAD-FLFYSAVV                   TO U23-FLFYSAVV                 
033200     MOVE ORAD-IDLEVNR                    TO U22-IDLEVNR                  
033300     MOVE ORAD-IDKUNDRF-RO                TO U22-IDKUNDRF-RO              
033400                                             U23B-IDKUNDRF-RO             
033500     MOVE ORAD-KDRADSTA                   TO U22-KDRADSTA                 
033600                                             U23-KDRADSTA                 
033700                                             U23B-KDRADSTA                
033800     MOVE ORAD-KVAVBART                   TO U22-KVAVBART                 
033900                                             U23-KVAVBART                 
034000     MOVE ORAD-KVBEART                    TO U22-KVBEART                  
034100                                             U23-KVBEART                  
034200                                             U23B-KVBEART                 
034300     MOVE ORAD-KVLEVART                   TO U22-KVLEVART                 
034400                                             U23-KVLEVART                 
034500                                             U23B-KVLEVART                
034600     MOVE ORAD-PRARTNTO                   TO U22-PRARTNTO                 
034700                                             U23B-PRARTNTO                
034800     MOVE ORAD-PRARTNTO-LOC               TO U22-PRARTNTO-LOC             
034900                                             U23B-PRARTNTO-LOC            
035000     MOVE ORAD-PRARTNTO-LOCPREL           TO U22-PRARTNTO-LOCPREL         
035100     MOVE ORAD-KDVALISO                   TO U22-KDVALISO                 
035200     MOVE ORAD-TIRODAT                    TO U22-TIRODAT                  
035300     MOVE ORAD-VKARTNTO                   TO U22-VKARTNTO                 
035400                                             U23B-VKARTNTO                
035500     MOVE ORAD-VLARTNTO                   TO U22-VLARTNTO                 
035600                                             U23B-VLARTNTO                
035700     MOVE ORAD-FLDIRLEV                   TO U23B-FLDIRLEV                
035800     MOVE ORAD-REKSIFFR                   TO U22-REKSIFFR                 
035900                                                                          
036000     IF ORAD-FLTILLK = JA                                                 
036100       MOVE '1'                           TO U22-IDARTNR-ERS              
036200     ELSE                                                                 
036300       MOVE  ZERO                         TO U22-IDARTNR-ERS              
036400     END-IF                                                               
036500                                                                          
036600     MOVE ORAD-ADLEVPL                    TO U22-ADLEVPL                  
036700     MOVE ORAD-BERADREF                   TO U22-BERADREF                 
036800     MOVE ORAD-IDPRODNR                   TO U22-IDPRODNR                 
036900     MOVE ORAD-KDFRAKT                    TO U22-KDFRAKT                  
037000     MOVE ORAD-KDORDTYP                   TO U22-KDORDTYP                 
037100     MOVE ORAD-KDPRODSL                   TO U22-KDPRODSL                 
037200                                             U23B-KDPRODSL                
037300     MOVE ORAD-IDKONTO                    TO U22-IDKONTO                  
037400     MOVE ORAD-IDKST                      TO U22-IDKST                    
037500     MOVE ORAD-TIUTSKR                    TO U22-TIUTSKR                  
037600     MOVE ORAD-KDKVBRYT                   TO U22-KDKVBRYT                 
037700     MOVE LOW-VALUE                       TO U22-LOW-VALUE1               
037800                                             U22-LOW-VALUE2               
037900     MOVE ZERO                            TO U23B-IDPLOCK                 
038000     MOVE ORAD-IDSYSTEM                   TO U23B-IDSYSTEM                
038100     MOVE ORAD-IDPSN                      TO U23B-IDPSN                   
038500     MOVE ORAD-IDVIN                      TO U23B-IDVIN                   
038900     .                                                                    
039000     EJECT                                                                
039100 E-FLYTTA-WDE421-INFO SECTION.                                            
039200                                                                          
039300     MOVE KKOLLI-IDKOLLI                  TO U23B-IDKOLLI                 
039400     MOVE KKOLLI-KVLEVART                 TO U23B-KVLEVART2               
039500                                                                          
039600     MOVE '0000'                          TO U23B-IDPRC                   
039700                                                                          
039800     MOVE ZERO                            TO U23B-IDSHIFT                 
039900                                             U23B-PRARTSTD                
040000                                             U23B-TIPACKN                 
040100     .                                                                    
040200     EJECT                                                                
040300 Z-FINIT SECTION.                                                         
040400                                                                          
040500     CLOSE W47922                                                         
040600           W47923                                                         
040700           W47923B                                                        
040800           W47983                                                         
040900     SKIP2                                                                
041000     MOVE 'S' TO POSTSUM-OPKOD                                            
041100     CALL POSTSUM USING POSTSUM-PARM                                      
041200     .                                                                    
041300     EJECT                                                                
041400 S11-SKRIV-W47922 SECTION.                                                
041500                                                                          
041600     WRITE U22-POST FROM U22-AREA                                         
041700                                                                          
041800     MOVE 'U22'      TO POSTSUM-TRANSTYP                                  
041900     MOVE 'W47922'   TO POSTSUM-FDNAMN                                    
042000     MOVE 'W47983D1' TO POSTSUM-DDNAMN2                                   
042100     CALL POSTSUM USING POSTSUM-PARM                                      
042200     .                                                                    
042300     EJECT                                                                
042400 S12-SKRIV-W47923 SECTION.                                                
042500                                                                          
042600     IF CDC                                                               
042700        CONTINUE                                                          
042800     ELSE                                                                 
042900        WRITE U23-POST FROM U23-AREA                                      
043000                                                                          
043100        MOVE 'U23'   TO POSTSUM-TRANSTYP                                  
043200        MOVE 'W47923' TO POSTSUM-FDNAMN                                   
043300        MOVE 'W47983D2' TO POSTSUM-DDNAMN2                                
043400        CALL POSTSUM USING POSTSUM-PARM                                   
043500     END-IF                                                               
043600     .                                                                    
043700     EJECT                                                                
043800 S13-SKRIV-W47923B SECTION.                                               
043900                                                                          
044000     WRITE U23B-POST FROM U23B-AREA                                       
044100                                                                          
044200     MOVE 'U23'      TO POSTSUM-TRANSTYP                                  
044300     MOVE 'W47923B'  TO POSTSUM-FDNAMN                                    
044400     MOVE 'W47983D3' TO POSTSUM-DDNAMN2                                   
044500     CALL POSTSUM USING POSTSUM-PARM                                      
044600     .                                                                    
044700     EJECT                                                                
044800 S14-SKRIV-W47983 SECTION.                                                
044900                                                                          
045000     WRITE U83-POST  FROM U83-AREA                                        
045100                                                                          
045200     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
045300     MOVE 'W47983'   TO POSTSUM-FDNAMN                                    
045400     MOVE 'W47983D4' TO POSTSUM-DDNAMN2                                   
045500     CALL POSTSUM USING POSTSUM-PARM                                      
045600     .                                                                    
045700     EJECT                                                                
045800* --- IMS SEKTIONER ---                                                   
045900     SKIP3                                                                
046000     EJECT                                                                
046100 IMS-GET-WDE4   SECTION.                                                  
046200                                                                          
046300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
046400     CALL CBLTDLI USING GN WDE4-PCB DLI-IO-AREA                           
046500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
046600     PERFORM IMS-STATUSKONTROLL                                           
046700     .                                                                    
046800     EJECT                                                                
046810 IMS-GU-WDQ301   SECTION.                                                 
046820     STRING 'WDQ301  (WDQ301KY =' W-WDQ301-KEY-X ')'                      
046830            DELIMITED BY SIZE INTO SSA1                                   
046840     MOVE '  GE' TO GODK-STATUSKODER                                      
046850     CALL CBLTDLI USING GU    WDQ3-PCB DLI-IO-WDQ301 SSA1                 
046860     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
046870     PERFORM IMS-STATUSKONTROLL                                           
046880     .                                                                    
046890     SKIP3                                                                
046909 IMS-GU-WDQ211    SECTION.                                                
046913     STRING 'WDQ201  (IDORDER  =' W-WDQ201-X ')'                          
046914            DELIMITED BY SIZE INTO SSA1                                   
046915     STRING 'WDQ211  (WDQ211KY =' W-WDQ211-KEY-X ')'                      
046916            DELIMITED BY SIZE INTO SSA2                                   
046917     MOVE '  GE'                TO GODK-STATUSKODER                       
046918     CALL CBLTDLI USING GU   WDQ2-PCB DLI-IO-WDQ211 SSA1 SSA2             
046920     MOVE WDQ2-STATUS-CODE      TO STATUS-WS                              
046921     PERFORM IMS-STATUSKONTROLL                                           
046922     .                                                                    
046923                                                                          
046924                                                                          
046930 IMS-STATUSKONTROLL SECTION.                                              
047000                                                                          
047100     SET STATUS-IX TO 1                                                   
047200     SEARCH GODK-STATUS                                                   
047300       AT END                                                             
047400         MOVE 'FEL STATUSKOD FRÅN IMS' TO FELTEXT-STR                     
047500         DISPLAY FELTEXT                                                  
047600         CALL FELLOG                                                      
047700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
047800         CONTINUE                                                         
047900     END-SEARCH                                                           
048000     .                                                                    

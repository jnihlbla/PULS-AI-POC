000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4140100.                                                
000400*AUTHOR.         GUNNAR LARSSON IDK.                                      
000500*DATE-WRITTEN.   92/03/20.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER NER LOGGTRANSAR FRÅN ORDERAVSLUT TILL SEKV.FILER.          
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLFILA (WDR6) MED SB.                      
001300*                                                                         
001400*        OBS! I PROGRAMMET REFERERAS TILL LOGISKT DBD WLFILA              
001500*             (PREFIX FILA-).                                             
001600*             I PSB:ET REFERERAS DOCK TILL FYSISKT DBD WDR6.              
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- ORDERHUVUDEN                                               
003100     SELECT W41401                     ASSIGN TO W41401D1.                
003200     SKIP2                                                                
003300*          --- ORDERRADER, ORDERBEKR, TILLÄGG-TPO & ANNULL.VORKÖ          
003400     SELECT W41402                     ASSIGN TO W41401D2.                
003500     SKIP2                                                                
003600*          --- WDR6 TRANSAR SOM SKALL RENSAS                              
003700     SELECT W41404                     ASSIGN TO W41401D3.                
003800     SKIP2                                                                
003900*          --- TPO-RADER                                                  
004000     SELECT W41405                     ASSIGN TO W41401D4.                
004100     SKIP2                                                                
004200*          --- WDB6-ÄNDRINGS LOGGAR                                       
004300     SELECT W41410                     ASSIGN TO W41401D5.                
004400     EJECT                                                                
004500*          --- ARTNR 100 IDENTIFIERINGS LOGGAR                            
004600     SELECT W41411                     ASSIGN TO W41401D6.                
004700     EJECT                                                                
004710*          --- BUMPERS                                                    
004720     SELECT W41412                     ASSIGN TO W41401D7.                
004730     EJECT                                                                
004740*          --- TEMPORARY CLASS 1 LOGG                                     
004750     SELECT W41418                     ASSIGN TO W41401D8.                
004751*          --- TEMPORARY LOGG DIFF NDCA AND XDCA MDULES                   
004752     SELECT W41419                     ASSIGN TO W41401D9.                
004760     EJECT                                                                
004800 DATA DIVISION.                                                           
004900     SKIP3                                                                
005000 FILE SECTION.                                                            
005100     SKIP3                                                                
005200 FD  W41401                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500     SKIP2                                                                
005600*01  POST -COPY W414001 -PRE  W41401-  -L.                                
005700     SKIP3                                                                
005800 FD  W41402                                                               
005900     RECORDING       V                                                    
006000     BLOCK CONTAINS  0.                                                   
006100     SKIP2                                                                
006200*01  ORAD-POST -COPY W414S002 -PRE  W41402-  -L.                          
006300     SKIP2                                                                
006400*01  OBKR-POST -COPY W414S003 -PRE  W41402-  -L.                          
006500     SKIP3                                                                
006600*01  TILLTPO-POST -COPY W414S005 -PRE  W41402-  -L.                       
006700     SKIP3                                                                
006800*01  ANNVOR-POST -COPY W414S011 -PRE  W41402-  -L.                        
006900     SKIP3                                                                
007000 FD  W41404                                                               
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300     SKIP2                                                                
007400*01  POST -COPY W414004 -PRE  W41404-  -L.                                
007500 FD  W41405                                                               
007600     RECORDING       V                                                    
007700     BLOCK CONTAINS  0.                                                   
007800     SKIP2                                                                
007900*01  REGTPO-POST  -COPY W414006 -PRE  W41405-  -L.                        
008000     SKIP3                                                                
008100 FD  W41410                                                               
008200     RECORDING       F                                                    
008300     BLOCK CONTAINS  0.                                                   
008400     SKIP2                                                                
008500*01  POST -COPY WDR601 -PRE  W41410-  -L.                                 
008600     EJECT                                                                
008700 FD  W41411                                                               
008800     RECORDING       F                                                    
008900     BLOCK CONTAINS  0.                                                   
009000     SKIP2                                                                
009100*01  POST -COPY W414100A -PRE  W41411-  -L.                               
009200     SKIP3                                                                
009210 FD  W41412                                                               
009220     RECORDING       F                                                    
009230     BLOCK CONTAINS  0.                                                   
009240     SKIP2                                                                
009250*01  POST -COPY W4141201 -PRE  W41412-  -L.                               
009260     SKIP3                                                                
009270 FD  W41418                                                               
009280     RECORDING       F                                                    
009290     BLOCK CONTAINS  0.                                                   
009291     SKIP2                                                                
009292*01  POST -COPY W414LOGG -PRE  W41418-  -L.                               
009293     SKIP3                                                                
009294 FD  W41419                                                               
009295     RECORDING       F                                                    
009296     BLOCK CONTAINS  0.                                                   
009297     SKIP2                                                                
009298*01  POST -COPY W414XDCD -PRE  W41419-  -L.                               
009299     SKIP3                                                                
009300 WORKING-STORAGE SECTION.                                                 
009400     SKIP2                                                                
009500                                                                          
009600*    -- CHECKED BY WY2000                                                 
009700 77  IDPGM                       PIC X(8)    VALUE 'W4140100'.            
009800 77  JA                          PIC X       VALUE 'J'.                   
009900 77  NEJ                         PIC X       VALUE 'N'.                   
010000     EJECT                                                                
010100 01  DYNAMISKA-SUBPROGRAM.                                                
010200*                                                                         
010300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010700     SKIP2                                                                
010800*    --- PARAMETRAR TILL ABEND                                            
010900                                                                          
011000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011200     SKIP2                                                                
011300 01  FELTEXT.                                                             
011400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL POSTSUM                                          
011800*                                                                         
011900*01  -COPY W0005   -PRE  POSTSUM-                                         
012000     EJECT                                                                
012100 01  W41401-AREA-START           PIC X(24)   VALUE                        
012200                                 'W41401-AREA-START  '.                   
012300     SKIP2                                                                
012400*01  AREA   -COPY W414001      -PRE W41401-                               
012500     EJECT                                                                
012600                                                                          
012700 01  W41402-ORAD-AREA-START      PIC X(24)   VALUE                        
012800                                 'W41402-ORAD-AREA-START'.                
012900     SKIP2                                                                
013000*01  ORAD-AREA -COPY W414S002     -PRE W41402-                            
013100     EJECT                                                                
013200                                                                          
013300 01  W41402-OBKR-AREA-START      PIC X(24)   VALUE                        
013400                                 'W41402-OBKR-AREA-START'.                
013500     SKIP2                                                                
013600*01  OBKR-AREA -COPY W414S003     -PRE W41402-                            
013700     EJECT                                                                
013800                                                                          
013900 01  W41402-TILLTPO-AREA-START   PIC X(24)   VALUE                        
014000                                 'W41402-TILLTPO-AREA'.                   
014100     SKIP2                                                                
014200*01  TILLTPO-AREA -COPY W414S005  -PRE W41402-                            
014300     EJECT                                                                
014400                                                                          
014500 01  W41402-ANNVOR-AREA-START    PIC X(24)   VALUE                        
014600                                 'W41402-ANNVOR-AREA'.                    
014700     SKIP2                                                                
014800*01  ANNVOR-AREA -COPY W414S011   -PRE W41402-                            
014900     EJECT                                                                
015000                                                                          
015100 01  W41404-AREA-START           PIC X(24)   VALUE                        
015200                                 'W41404-AREA-START  '.                   
015300     SKIP2                                                                
015400*01  AREA -COPY W414004     -PRE W41404-                                  
015500     EJECT                                                                
015600                                                                          
015700 01  W41405-REGTPO-AREA-START    PIC X(24)   VALUE                        
015800                                 'W41405-REGTPO-AREA'.                    
015900     SKIP2                                                                
016000*01  REGTPO-AREA  -COPY W414006  -PRE W41405-                             
016100     EJECT                                                                
016200                                                                          
016300 01  W41410-AREA-START           PIC X(24)   VALUE                        
016400                                 'W41410-AREA-START  '.                   
016500     SKIP2                                                                
016600*01  AREA -COPY WDR601      -PRE W41410-                                  
016700     EJECT                                                                
016800 01  W41411-AREA-START           PIC X(24)   VALUE                        
016810                                 'W41411-AREA-START  '.                   
016820     SKIP2                                                                
016830*01  AREA   -COPY W414100A     -PRE W41411-                               
016840     EJECT                                                                
016841 01  W41412-AREA-START           PIC X(24)   VALUE                        
016842                                 'W41412-AREA-START  '.                   
016843     SKIP2                                                                
016844*01  AREA   -COPY W4141201     -PRE W41412-                               
016845     EJECT                                                                
016846 01  W41418-AREA-START           PIC X(24)   VALUE                        
016847                                 'W41418-AREA-START  '.                   
016848     SKIP2                                                                
016849*01  AREA   -COPY W414LOGG     -PRE W41418-                               
016850                                                                          
016851     SKIP2                                                                
016852 01  W41419-AREA-START           PIC X(24)   VALUE                        
016853                                 'W41419-AREA-START  '.                   
016854     SKIP2                                                                
016855*01  AREA   -COPY W414XDCD     -PRE W41419-                               
016856     EJECT                                                                
016860                                                                          
016900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017000*                                                                         
017100     SKIP2                                                                
017200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017300     SKIP3                                                                
017400*    --- STATUS-KOD FRÅN IMS                                              
017500 01  STATUS-WS                   PIC XX.                                  
017600     88  SEGMENT-FINNS                       VALUE '  '.                  
017700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017900     SKIP2                                                                
018000 01  GODK-STATUSKODER.                                                    
018100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018200     EJECT                                                                
018300*    --- IMS FUNKTIONSKODER                                               
018400*01  -COPY W0003                                                          
018500     EJECT                                                                
018600*    ---  DLI INPUT-OUTPUT AREA                                           
018700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018800     SKIP3                                                                
018900 01  DLI-IO-AREA.                                                         
019000     SKIP3                                                                
019100*    03  WLFILA01 -COPY WDR601                                            
019200     EJECT                                                                
019300*        07  -COPY W414200A -RED FIL-WDR601-DATA                          
019400     EJECT                                                                
019500*        07  -COPY W414201A -RED FIL-WDR601-DATA                          
019600     EJECT                                                                
019700*        07  -COPY W414202A -RED FIL-WDR601-DATA                          
019800     EJECT                                                                
019900*        07  -COPY W414203A -RED FIL-WDR601-DATA                          
020000     EJECT                                                                
020100*        07  -COPY W414204A -RED FIL-WDR601-DATA                          
020200     EJECT                                                                
020300*        07  -COPY W414205A -RED FIL-WDR601-DATA                          
020400     EJECT                                                                
020500*        07  -COPY W414DCSA -RED FIL-WDR601-DATA                          
020600     EJECT                                                                
020610*        07  -COPY W414100A -RED FIL-WDR601-DATA                          
020620     EJECT                                                                
020630*        07  -COPY W414BUMA -RED FIL-WDR601-DATA                          
020640     EJECT                                                                
020650*        07  -COPY W414LOGG -RED FIL-WDR601-DATA                          
020651     EJECT                                                                
020652*        07  -COPY W414XDCA -RED FIL-WDR601-DATA                          
020660     EJECT                                                                
020700 LINKAGE SECTION.                                                         
020800     SKIP2                                                                
020900*01  -COPY W0008  -PRE FILA-                                              
021000     05  FILLER                  PIC X.                                   
021100     EJECT                                                                
021200 PROCEDURE DIVISION  USING FILA-PCB.                                      
021300     ENTRY 'DLITCBL' USING FILA-PCB.                                      
021400                                                                          
021500     SKIP2                                                                
021600     PERFORM A-INIT                                                       
021700                                                                          
021800     PERFORM IMS-GN-FILA-FIL                                              
021900     PERFORM UNTIL (NOT SEGMENT-FINNS)                                    
022000                                                                          
022100       IF  FIL-CT-IDSYSTEM = 'W414'                                       
022200         EVALUATE FIL-CT-IDPTYP                                           
022300           WHEN '200'                                                     
022400             PERFORM B-SKAPA-OHUV-POST                                    
022500             PERFORM E-SKAPA-RENS-POST                                    
022600           WHEN '201'                                                     
022700             PERFORM C-SKAPA-ORAD-POST                                    
022800             PERFORM E-SKAPA-RENS-POST                                    
022900           WHEN '202'                                                     
023000             PERFORM D-SKAPA-OBKR-POST                                    
023100             PERFORM E-SKAPA-RENS-POST                                    
023200           WHEN '203'                                                     
023300             PERFORM F-SKAPA-TPO-POST                                     
023400             PERFORM E-SKAPA-RENS-POST                                    
023500           WHEN '204'                                                     
023600             PERFORM G-SKAPA-TPO-POST                                     
023700             PERFORM E-SKAPA-RENS-POST                                    
023800           WHEN '205'                                                     
023900             PERFORM H-SKAPA-VOR-POST                                     
024000             PERFORM E-SKAPA-RENS-POST                                    
024100           WHEN 'DCS'                                                     
024200             PERFORM I-SKAPA-B6LOGG-POST                                  
024300             PERFORM E-SKAPA-RENS-POST                                    
024310           WHEN '100'                                                     
024320             PERFORM J-SKAPA-IDENT-POST                                   
024330             PERFORM E-SKAPA-RENS-POST                                    
024340           WHEN 'BUM'                                                     
024350             PERFORM K-SKAPA-BUMP-POST                                    
024360             PERFORM E-SKAPA-RENS-POST                                    
024370           WHEN 'LOG'                                                     
024380             PERFORM L-SKAPA-LOGG-POST                                    
024390             PERFORM E-SKAPA-RENS-POST                                    
024391           WHEN 'XDC'                                                     
024392             PERFORM M-SKAPA-XDCA-POST                                    
024393             PERFORM E-SKAPA-RENS-POST                                    
024400           WHEN OTHER                                                     
024500             CONTINUE                                                     
024600         END-EVALUATE                                                     
024700       ELSE                                                               
024800         CONTINUE                                                         
024900       END-IF                                                             
025000                                                                          
025100       PERFORM IMS-GN-FILA-FIL                                            
025200     END-PERFORM                                                          
025300                                                                          
025400     PERFORM Z-FINIT                                                      
025500                                                                          
025600     MOVE ZERO TO RETURN-CODE                                             
025700     GOBACK                                                               
025800     .                                                                    
025900     EJECT                                                                
026000 A-INIT SECTION.                                                          
026100                                                                          
026200     OPEN OUTPUT W41401                                                   
026300                 W41402                                                   
026400                 W41404                                                   
026500                 W41405                                                   
026600                 W41410                                                   
026610                 W41411                                                   
026620                 W41412                                                   
026630                 W41418                                                   
026640                 W41419                                                   
026700                                                                          
026800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026900     .                                                                    
027000     EJECT                                                                
027100 B-SKAPA-OHUV-POST SECTION.                                               
027200                                                                          
027300     MOVE FIL-CT-IDPTYP          TO W41401-OHUV-IDPTYP                    
027400     MOVE 200-BEKUNDRF           TO W41401-OHUV-BEKUNDRF                  
027500     MOVE 200-BEVARREF           TO W41401-OHUV-BEVARREF                  
027600     MOVE 200-FLRESTN            TO W41401-OHUV-FLRESTN                   
027700     MOVE 200-IDDISTR            TO W41401-OHUV-IDDISTR                   
027800     MOVE 200-IDKONTO            TO W41401-OHUV-IDKONTO                   
027900     MOVE 200-IDKST              TO W41401-OHUV-IDKST                     
028000     MOVE 200-IDKUNDNR           TO W41401-OHUV-IDKUNDNR                  
028100     MOVE 200-IDKUNDRF           TO W41401-OHUV-IDKUNDRF                  
028200     MOVE 200-IDORDER            TO W41401-OHUV-IDORDER                   
028300     MOVE 200-IDSKYLT            TO W41401-OHUV-IDSKYLT                   
028400     MOVE 200-IDDC               TO W41401-OHUV-IDDC                      
028500     MOVE 200-KDFAKTYP           TO W41401-OHUV-KDFAKTYP                  
028600     MOVE 200-KDFRAKT            TO W41401-OHUV-KDFRAKT                   
028700     MOVE 200-KDORDKL            TO W41401-OHUV-KDORDKL                   
028800     MOVE 200-KDROPACK           TO W41401-OHUV-KDROPACK                  
028900     MOVE 200-KDTULLVE           TO W41401-OHUV-KDTULLVE                  
029000     MOVE 200-TIREGDAT           TO W41401-OHUV-TIREGDAT-ORDER            
029100     MOVE 200-TIRFS              TO W41401-OHUV-TIRFS                     
029200     MOVE 200-TIAAMMDD           TO W41401-OHUV-TIAAMMDD                  
029300     MOVE 200-TIHHMM             TO W41401-OHUV-TIHHMM                    
029400     MOVE FIL-TIREGDAT           TO W41401-OHUV-TIREGDAT                  
029500     MOVE FIL-TIKLOCK            TO W41401-OHUV-TIKLOCK                   
029600     PERFORM S11-SKRIV-W41401                                             
029700     .                                                                    
029800     EJECT                                                                
029900 C-SKAPA-ORAD-POST SECTION.                                               
030000                                                                          
030100     MOVE FIL-CT-IDPTYP          TO W41402-ORAD-IDPTYP                    
030200     MOVE 201-IDARTNR            TO W41402-ORAD-IDARTNR                   
030300                                    W41402-ORAD-IDARTNR-S                 
030400     MOVE ZERO                   TO W41402-ORAD-IDARTNR-TILLK-S           
030500     MOVE 201-BERADREF           TO W41402-ORAD-BERADREF                  
030600     MOVE 201-BEVARREF           TO W41402-ORAD-BEVARREF                  
030700     MOVE 201-BEVOLREF           TO W41402-ORAD-BEVOLREF                  
030800     MOVE 201-FLINVEST           TO W41402-ORAD-FLINVEST                  
030900     MOVE 201-FLOVRLEV           TO W41402-ORAD-FLOVRLEV                  
031000     MOVE 201-FLPRTILL           TO W41402-ORAD-FLPRTILL                  
031100     MOVE 201-FLTILLK            TO W41402-ORAD-FLTILLK                   
031200     MOVE SPACE                  TO W41402-ORAD-FLREFILL                  
031300     MOVE 201-IDDISTR            TO W41402-ORAD-IDDISTR                   
031400     MOVE ZERO                   TO W41402-ORAD-IDFKNGRP                  
031500     MOVE 201-IDKAMPRF           TO W41402-ORAD-IDKAMPRF                  
031600     MOVE 201-IDKONTO            TO W41402-ORAD-IDKONTO                   
031700     MOVE 201-IDKST              TO W41402-ORAD-IDKST                     
031800     MOVE 201-IDKUNDNR           TO W41402-ORAD-IDKUNDNR                  
031900     MOVE 201-IDKUNDRF           TO W41402-ORAD-IDKUNDRF                  
032000     MOVE 201-IDKUNDRF-RO        TO W41402-ORAD-IDKUNDRF-RO               
032100     MOVE 201-IDLOPNR            TO W41402-ORAD-IDLOPNR                   
032200     MOVE 201-IDORDER            TO W41402-ORAD-IDORDER                   
032300     MOVE 201-IDSYSTEM           TO W41402-ORAD-IDSYSTEM                  
032400     MOVE 201-IDSYSTEM-OHUV      TO W41402-ORAD-IDSYSTEM-OHUV             
032500     MOVE 201-IDUSER             TO W41402-ORAD-IDUSER                    
032600     MOVE 201-IDDC               TO W41402-ORAD-IDDC                      
032700     MOVE 201-IDDC-CLEAR         TO W41402-ORAD-IDDC-CLEAR                
032800     MOVE 201-KDDSP              TO W41402-ORAD-KDDSP                     
032900     MOVE 201-KDFAKTYP           TO W41402-ORAD-KDFAKTYP                  
033000     MOVE 201-KDFRAKT            TO W41402-ORAD-KDFRAKT                   
033100     MOVE 201-KDKVBRYT           TO W41402-ORAD-KDKVBRYT                  
033200     MOVE 201-KDORDING           TO W41402-ORAD-KDORDING                  
033300     MOVE 201-KDORDKL            TO W41402-ORAD-KDORDKL                   
033400     MOVE 201-KDPRTYP            TO W41402-ORAD-KDPRTYP                   
033500     MOVE ZERO                   TO W41402-ORAD-KDPRODSL                  
033600     MOVE ZERO                   TO W41402-ORAD-KDTIPPR                   
033700     MOVE 201-KDTPOTYP           TO W41402-ORAD-KDTPOTYP                  
033800     MOVE SPACE                  TO W41402-ORAD-KDUART                    
033900     MOVE 201-KDVRINFO           TO W41402-ORAD-KDVRINFO                  
034000     MOVE 201-KVBEART            TO W41402-ORAD-KVBEART                   
034100     MOVE 201-KVBEART-Q          TO W41402-ORAD-KVBEART-Q                 
034200     MOVE ZERO                   TO W41402-ORAD-PRARTBTO-EXP              
034300     MOVE 201-PRARTNTO           TO W41402-ORAD-PRARTNTO                  
034400     MOVE ZERO                   TO W41402-ORAD-PRARTSJK                  
034500     MOVE 201-PRBPRIS            TO W41402-ORAD-PRBPRIS                   
034600     MOVE ZERO                   TO W41402-ORAD-REDIRLEV                  
034700     MOVE 201-REKSIFFR           TO W41402-ORAD-REKSIFFR                  
034800     MOVE 201-TIREGDAT           TO W41402-ORAD-TIREGDAT-ORDER            
034900     MOVE 201-TIRODAT            TO W41402-ORAD-TIRODAT                   
035000     MOVE FIL-TIREGDAT           TO W41402-ORAD-TIREGDAT                  
035100     MOVE FIL-TIKLOCK            TO W41402-ORAD-TIKLOCK                   
035200     PERFORM S12-SKRIV-W41402-ORAD                                        
035300     .                                                                    
035400     EJECT                                                                
035500 D-SKAPA-OBKR-POST SECTION.                                               
035600                                                                          
035700     MOVE FIL-CT-IDPTYP          TO W41402-OBKR-IDPTYP                    
035800     MOVE 202-IDARTNR            TO W41402-OBKR-IDARTNR                   
035900                                    W41402-OBKR-IDARTNR-S                 
036000     MOVE 202-BEERS              TO W41402-OBKR-BEERS                     
036100     MOVE 202-BERADREF           TO W41402-OBKR-BERADREF                  
036200     MOVE 202-BEVARREF           TO W41402-OBKR-BEVARREF                  
036300     MOVE 202-BEVOLREF           TO W41402-OBKR-BEVOLREF                  
036400     MOVE 202-DIERS-KVOT         TO W41402-OBKR-DIERS-KVOT                
036500     MOVE 202-FLINVEST           TO W41402-OBKR-FLINVEST                  
036600     MOVE 202-FLOVRLEV           TO W41402-OBKR-FLOVRLEV                  
036700     MOVE 202-FLPRTILL           TO W41402-OBKR-FLPRTILL                  
036800     MOVE 202-FLTILLK            TO W41402-OBKR-FLTILLK                   
036900     MOVE 202-IDARTNR-TILLK TO W41402-OBKR-IDARTNR-TILLK                  
037000                                    W41402-OBKR-IDARTNR-TILLK-S           
037100     MOVE 202-IDDISTR            TO W41402-OBKR-IDDISTR                   
037200     MOVE ZERO                   TO W41402-OBKR-IDFKNGRP                  
037300     MOVE 202-IDKONTO            TO W41402-OBKR-IDKONTO                   
037400     MOVE 202-IDKST              TO W41402-OBKR-IDKST                     
037500     MOVE 202-IDKUNDNR           TO W41402-OBKR-IDKUNDNR                  
037600     MOVE 202-IDKUNDRF           TO W41402-OBKR-IDKUNDRF                  
037700     MOVE 202-IDKUNDRF-RO        TO W41402-OBKR-IDKUNDRF-RO               
037800     MOVE 202-IDLOPNR            TO W41402-OBKR-IDLOPNR                   
037900     MOVE 202-IDORDER            TO W41402-OBKR-IDORDER                   
038000     MOVE 202-IDSEKVNR           TO W41402-OBKR-IDSEKVNR                  
038100     MOVE 202-IDSYSTEM           TO W41402-OBKR-IDSYSTEM                  
038200     MOVE 202-IDDC               TO W41402-OBKR-IDDC                      
038300     MOVE 202-KDDSP              TO W41402-OBKR-KDDSP                     
038400     MOVE 202-KDERS              TO W41402-OBKR-KDERS                     
038500     MOVE 202-KDFAKTYP           TO W41402-OBKR-KDFAKTYP                  
038600     MOVE 202-KDFRAKT            TO W41402-OBKR-KDFRAKT                   
038700     MOVE 202-KDKVBRYT           TO W41402-OBKR-KDKVBRYT                  
038800     MOVE 202-KDORDBEK           TO W41402-OBKR-KDORDBEK                  
038900     MOVE 202-KDORDKL            TO W41402-OBKR-KDORDKL                   
039000     MOVE ZERO                   TO W41402-OBKR-KDPRODSL                  
039100     MOVE 202-KDPRTYP            TO W41402-OBKR-KDPRTYP                   
039200     MOVE ZERO                   TO W41402-OBKR-KDTIPPR                   
039300     MOVE 202-KDTPOTYP           TO W41402-OBKR-KDTPOTYP                  
039400     MOVE SPACE                  TO W41402-OBKR-KDUART                    
039500     MOVE 202-KDVRINFO           TO W41402-OBKR-KDVRINFO                  
039600     MOVE 202-KVBEART            TO W41402-OBKR-KVBEART                   
039700     MOVE 202-KVBEART-Q          TO W41402-OBKR-KVBEART-Q                 
039800     MOVE 202-KVBEART-TILLK TO W41402-OBKR-KVBEART-TILLK                  
039900     MOVE 202-KVPREAVB           TO W41402-OBKR-KVPREAVB                  
040000     MOVE 202-KVPRERO            TO W41402-OBKR-KVPRERO                   
040100     MOVE 202-KVQPACK-1          TO W41402-OBKR-KVQPACK-1                 
040200     MOVE ZERO                   TO W41402-OBKR-PRARTBTO-EXP              
040300     MOVE 202-PRARTNTO           TO W41402-OBKR-PRARTNTO                  
040400     MOVE ZERO                   TO W41402-OBKR-PRARTSJK                  
040500     MOVE 202-REKSIFFR           TO W41402-OBKR-REKSIFFR                  
040600     MOVE 202-REKSIFFR-TILLK TO W41402-OBKR-REKSIFFR-TILLK                
040700     MOVE 202-TIDISPIN           TO W41402-OBKR-TIDISPIN                  
040800     MOVE 202-TIORDREG           TO W41402-OBKR-TIORDREG                  
040900     MOVE 202-TIREGDAT           TO W41402-OBKR-TIREGDAT-OBKR             
041000     MOVE 202-TITPO              TO W41402-OBKR-TITPO                     
041010     IF 202-TIREPDAT IS NUMERIC                                           
041020       MOVE 202-TIREPDAT         TO W41402-OBKR-TIREPDAT                  
041030     ELSE                                                                 
041040       MOVE +0                   TO W41402-OBKR-TIREPDAT                  
041050     END-IF                                                               
041100     MOVE FIL-TIREGDAT           TO W41402-OBKR-TIREGDAT                  
041200     MOVE FIL-TIKLOCK            TO W41402-OBKR-TIKLOCK                   
041300     PERFORM S13-SKRIV-W41402-OBKR                                        
041400     .                                                                    
041500     EJECT                                                                
041600 E-SKAPA-RENS-POST SECTION.                                               
041700                                                                          
041800     MOVE FIL-IDPGM              TO W41404-RENS-IDPGM                     
041900     MOVE FIL-TIREGDAT           TO W41404-RENS-TIREGDAT                  
042000     MOVE FIL-TIKLOCK            TO W41404-RENS-TIKLOCK                   
042100     MOVE FIL-IDSEKVNR           TO W41404-RENS-IDSEKVNR                  
042200     MOVE FIL-CT-IDSYSTEM        TO W41404-RENS-CT-IDSYSTEM               
042300     MOVE FIL-CT-IDPTYP          TO W41404-RENS-CT-IDPTYP                 
042400     MOVE FIL-CT-IDVTYP          TO W41404-RENS-CT-IDVTYP                 
042500     PERFORM S14-SKRIV-W41404                                             
042600     .                                                                    
042700     EJECT                                                                
042800 F-SKAPA-TPO-POST SECTION.                                                
042900                                                                          
043000     MOVE FIL-CT-IDPTYP          TO W41402-TILLTPO-IDPTYP                 
043100     MOVE 203-IDARTNR            TO W41402-TILLTPO-IDARTNR                
043200                                    W41402-TILLTPO-IDARTNR-S              
043300     MOVE ZERO                   TO W41402-TILLTPO-IDARTNR-TILLK-S        
043400     MOVE 203-BERADREF           TO W41402-TILLTPO-BERADREF               
043500     MOVE 203-BEVOLREF           TO W41402-TILLTPO-BEVOLREF               
043600     MOVE 203-FLTILLK            TO W41402-TILLTPO-FLTILLK                
043700     MOVE 203-IDDISTR            TO W41402-TILLTPO-IDDISTR                
043800     MOVE 203-IDKONTO            TO W41402-TILLTPO-IDKONTO                
043900     MOVE 203-IDKST              TO W41402-TILLTPO-IDKST                  
044000     MOVE 203-IDKUNDNR           TO W41402-TILLTPO-IDKUNDNR               
044100     MOVE 203-IDKUNDRF           TO W41402-TILLTPO-IDKUNDRF               
044200     MOVE 203-IDSYSTEM           TO W41402-TILLTPO-IDSYSTEM               
044300     MOVE 203-KDDSP              TO W41402-TILLTPO-KDDSP                  
044400     MOVE 203-KDFAKTYP           TO W41402-TILLTPO-KDFAKTYP               
044500     MOVE 203-KDFRAKT            TO W41402-TILLTPO-KDFRAKT                
044600     MOVE 203-KDKVBRYT           TO W41402-TILLTPO-KDKVBRYT               
044700     MOVE 203-KDORDBEK           TO W41402-TILLTPO-KDORDBEK               
044800     MOVE 203-KDORDING           TO W41402-TILLTPO-KDORDING               
044900     MOVE 203-KDORDKL            TO W41402-TILLTPO-KDORDKL                
045000     MOVE 203-KDTPOTYP           TO W41402-TILLTPO-KDTPOTYP               
045100     MOVE 203-KDVRINFO           TO W41402-TILLTPO-KDVRINFO               
045200     MOVE 203-KVBEART-Q          TO W41402-TILLTPO-KVBEART-Q              
045300     MOVE 203-PRARTNTO           TO W41402-TILLTPO-PRARTNTO               
045400     MOVE 203-REKSIFFR           TO W41402-TILLTPO-REKSIFFR               
045500     MOVE 203-TIDISPIN           TO W41402-TILLTPO-TIDISPIN               
045600     MOVE 203-TIREGDAT           TO W41402-TILLTPO-TIREGDAT-TPO           
045700     MOVE 203-TITPO              TO W41402-TILLTPO-TITPO                  
045800     MOVE 203-KVBEART            TO W41402-TILLTPO-KVBEART                
045900     MOVE 203-KVQPACK-1          TO W41402-TILLTPO-KVQPACK-1              
046000     MOVE 203-BEVARREF           TO W41402-TILLTPO-BEVARREF               
046100     MOVE 203-KDPRTYP            TO W41402-TILLTPO-KDPRTYP                
046200     MOVE 203-FLPRTILL           TO W41402-TILLTPO-FLPRTILL               
046300     MOVE 203-FLINVEST           TO W41402-TILLTPO-FLINVEST               
046400     MOVE 203-KDPRODSL           TO W41402-TILLTPO-KDPRODSL               
046500     MOVE ZERO                   TO W41402-TILLTPO-PRARTBTO-EXP           
046600     MOVE ZERO                   TO W41402-TILLTPO-IDFKNGRP               
046700     MOVE FIL-TIREGDAT           TO W41402-TILLTPO-TIREGDAT               
046800     MOVE FIL-TIKLOCK            TO W41402-TILLTPO-TIKLOCK                
046900     PERFORM S15-SKRIV-W41402-TILLTPO                                     
047000     .                                                                    
047100     EJECT                                                                
047200 G-SKAPA-TPO-POST SECTION.                                                
047300                                                                          
047400     MOVE FIL-CT-IDPTYP          TO W41405-REGTPO-IDPTYP                  
047500     MOVE 204-IDARTNR            TO W41405-REGTPO-IDARTNR                 
047600     MOVE 204-BERADREF           TO W41405-REGTPO-BERADREF                
047700     MOVE 204-IDDISTR            TO W41405-REGTPO-IDDISTR                 
047800     MOVE 204-IDKONTO            TO W41405-REGTPO-IDKONTO                 
047900     MOVE 204-IDKST              TO W41405-REGTPO-IDKST                   
048000     MOVE 204-IDKUNDNR           TO W41405-REGTPO-IDKUNDNR                
048100     MOVE 204-IDKUNDRF           TO W41405-REGTPO-IDKUNDRF                
048200     MOVE 204-IDSYSTEM           TO W41405-REGTPO-IDSYSTEM                
048300     MOVE 204-KDFRAKT            TO W41405-REGTPO-KDFRAKT                 
048400     MOVE 204-KDKVBRYT           TO W41405-REGTPO-KDKVBRYT                
048500     MOVE 204-KDORDKL            TO W41405-REGTPO-KDORDKL                 
048600     MOVE 204-KDTPOTYP           TO W41405-REGTPO-KDTPOTYP                
048700     MOVE 204-KDUART             TO W41405-REGTPO-KDUART                  
048800     MOVE 204-KDVRINFO           TO W41405-REGTPO-KDVRINFO                
048900     MOVE 204-KVBEART-Q          TO W41405-REGTPO-KVBEART-Q               
049000     MOVE 204-PRARTNTO           TO W41405-REGTPO-PRARTNTO                
049100     MOVE 204-REKSIFFR           TO W41405-REGTPO-REKSIFFR                
049200     MOVE 204-TIREGDAT           TO W41405-REGTPO-TIREGDAT-TPO            
049300     MOVE 204-TITPO              TO W41405-REGTPO-TITPO                   
049400     MOVE FIL-TIREGDAT           TO W41405-REGTPO-TIREGDAT                
049500     MOVE FIL-TIKLOCK            TO W41405-REGTPO-TIKLOCK                 
049600     PERFORM S16-SKRIV-W41405-REGTPO                                      
049700     .                                                                    
049800     EJECT                                                                
049900 H-SKAPA-VOR-POST SECTION.                                                
050000                                                                          
050100     MOVE FIL-CT-IDPTYP          TO W41402-ANNVOR-IDPTYP                  
050200     MOVE 205-IDARTNR            TO W41402-ANNVOR-IDARTNR                 
050300                                    W41402-ANNVOR-IDARTNR-S               
050400     MOVE ZERO                   TO W41402-ANNVOR-IDARTNR-TILLK-S         
050500     MOVE 205-IDDC               TO W41402-ANNVOR-IDDC                    
050600     MOVE 205-IDDISTR            TO W41402-ANNVOR-IDDISTR                 
050700     MOVE 205-IDKUNDNR           TO W41402-ANNVOR-IDKUNDNR                
050800     MOVE ZERO                   TO W41402-ANNVOR-KDPRODSL                
050900     MOVE 205-FLDIRLEV           TO W41402-ANNVOR-FLDIRLEV                
051000     COMPUTE W41402-ANNVOR-KVANNANT                                       
051100                                 = 205-KVBEART-Q - 205-KVAVBART           
051200     MOVE 205-KVAVBART           TO W41402-ANNVOR-KVAVBART                
051300     MOVE 205-KVBEART-Q          TO W41402-ANNVOR-KVBEART-Q               
051400     PERFORM S17-SKRIV-W41402-ANNVOR                                      
051500     .                                                                    
051600     EJECT                                                                
051700 I-SKAPA-B6LOGG-POST SECTION.                                             
051800                                                                          
051900     MOVE DLI-IO-AREA            TO W41410-AREA                           
052000     PERFORM S18-SKRIV-W41410                                             
052100     .                                                                    
052200     EJECT                                                                
052210 J-SKAPA-IDENT-POST SECTION.                                              
052220                                                                          
052295     MOVE 100-IDDC-REC           TO W41411-100-IDDC-REC                   
052296     MOVE 100-IDDC-SEND          TO W41411-100-IDDC-SEND                  
052297     MOVE 100-IDARTNR            TO W41411-100-IDARTNR                    
052298     MOVE 100-KVANTAL            TO W41411-100-KVANTAL                    
052299     MOVE 'PARTNO 100'           TO W41411-100-AVVIKELSETYP               
052300     MOVE 100-BEART              TO W41411-100-BEART                      
052301     MOVE 100-ADLAGOMR           TO W41411-100-ADLAGOMR                   
052302     MOVE 100-ADGANG             TO W41411-100-ADGANG                     
052303     MOVE 100-ADPLATS            TO W41411-100-ADPLATS                    
052307     PERFORM S19-SKRIV-W41411                                             
052308     .                                                                    
052309     EJECT                                                                
052310 K-SKAPA-BUMP-POST SECTION.                                               
052311                                                                          
052312     MOVE FIL-TIREGDAT           TO W41412-BUMU-TIREGDAT                  
052313     MOVE BUM-IDLEVNR            TO W41412-BUMU-IDLEVNR                   
052314     MOVE BUM-IDDISTR            TO W41412-BUMU-IDDISTR                   
052315     MOVE BUM-IDKUNDNR           TO W41412-BUMU-IDKUNDNR                  
052316     MOVE BUM-IDKUNDRF           TO W41412-BUMU-IDKUNDRF                  
052317     MOVE BUM-IDARTNR            TO W41412-BUMU-IDARTNR                   
052318     MOVE BUM-KVBEART            TO W41412-BUMU-KVBEART                   
052319     MOVE BUM-IDDC               TO W41412-BUMU-IDDC                      
052320     MOVE BUM-IDDC-STEER         TO W41412-BUMU-IDDC-STEER                
052321     PERFORM S20-SKRIV-W41412                                             
052322     .                                                                    
052323     EJECT                                                                
052324 L-SKAPA-LOGG-POST SECTION.                                               
052325                                                                          
052326     MOVE LOG-TIREGDAT           TO W41418-LOG-TIREGDAT                   
052327     MOVE LOG-TIREGTID           TO W41418-LOG-TIREGTID                   
052328     MOVE LOG-IDARTNR            TO W41418-LOG-IDARTNR                    
052329     MOVE LOG-KVBEART-Q          TO W41418-LOG-KVBEART-Q                  
052330     MOVE LOG-IDDC               TO W41418-LOG-IDDC                       
052331     MOVE LOG-KVLS               TO W41418-LOG-KVLS                       
052332     MOVE LOG-KVDISP             TO W41418-LOG-KVDISP                     
052333     MOVE LOG-KVAKS-SDC          TO W41418-LOG-KVAKS-SDC                  
052334     MOVE LOG-KVAKS-PAV          TO W41418-LOG-KVAKS-PAV                  
052335     MOVE LOG-KVOKS-BULK         TO W41418-LOG-KVOKS-BULK                 
052336     MOVE LOG-KVOKS-DAG          TO W41418-LOG-KVOKS-DAG                  
052337     MOVE LOG-TIBUFF             TO W41418-LOG-TIBUFF                     
052338     MOVE LOG-KVBEART-BUFF       TO W41418-LOG-KVBEART-BUFF               
052339     MOVE LOG-KVLS-AVAILABLE     TO W41418-LOG-KVLS-AVAILABLE             
052340     MOVE LOG-KVLS-ON-HAND       TO W41418-LOG-KVLS-ON-HAND               
052341     MOVE LOG-IDDC-REF           TO W41418-LOG-IDDC-REF                   
052342     MOVE LOG-KVDISP-REF         TO W41418-LOG-KVDISP-REF                 
052343     MOVE LOG-KVOKS-BULK-REF     TO W41418-LOG-KVOKS-BULK-REF             
052344     MOVE LOG-KVOKS-DAG-REF      TO W41418-LOG-KVOKS-DAG-REF              
052345     MOVE LOG-FLLEVOK            TO W41418-LOG-FLLEVOK                    
052384     PERFORM S21-SKRIV-W41418                                             
052385     .                                                                    
052386     EJECT                                                                
052387 M-SKAPA-XDCA-POST SECTION.                                               
052388                                                                          
052389     MOVE FIL-TIREGDAT           TO W41419-DIFF-TIREGDAT                  
052390     MOVE FIL-TIKLOCK            TO W41419-DIFF-TIKLOCK                   
052391     MOVE DIFF-IDDISTR           TO W41419-DIFF-IDDISTR                   
052392     MOVE DIFF-IDKUNDNR          TO W41419-DIFF-IDKUNDNR                  
052393     MOVE DIFF-IDORDNR5          TO W41419-DIFF-IDORDNR5                  
052394     MOVE DIFF-KDORDKL           TO W41419-DIFF-KDORDKL                   
052395     MOVE DIFF-IDARTNR           TO W41419-DIFF-IDARTNR                   
052396     MOVE DIFF-KVBEART-Q         TO W41419-DIFF-KVBEART-Q                 
052397     MOVE DIFF-IDDC              TO W41419-DIFF-IDDC                      
052398     MOVE DIFF-IDSYSTEM          TO W41419-DIFF-IDSYSTEM                  
052399     MOVE DIFF-FLSVAR            TO W41419-DIFF-FLSVAR                    
052400     MOVE DIFF-KVOKS-DAG-NDCA    TO W41419-DIFF-KVOKS-DAG-NDCA            
052401     MOVE DIFF-KVOKS-DAG-XDCA    TO W41419-DIFF-KVOKS-DAG-XDCA            
052402     MOVE DIFF-KVOKS-BULK-NDCA   TO W41419-DIFF-KVOKS-BULK-NDCA           
052403     MOVE DIFF-KVOKS-BULK-XDCA   TO W41419-DIFF-KVOKS-BULK-XDCA           
052404     MOVE DIFF-KDORDBEK-NDCA     TO W41419-DIFF-KDORDBEK-NDCA             
052405     MOVE DIFF-KVPREAVB-NDCA     TO W41419-DIFF-KVPREAVB-NDCA             
052406     MOVE DIFF-ADLAGOMR-NDCA     TO W41419-DIFF-ADLAGOMR-NDCA             
052407     MOVE DIFF-ADGANG-NDCA       TO W41419-DIFF-ADGANG-NDCA               
052408     MOVE DIFF-ADPLATS-NDCA      TO W41419-DIFF-ADPLATS-NDCA              
052409     MOVE DIFF-IDDC-NDCA         TO W41419-DIFF-IDDC-NDCA                 
052410     MOVE DIFF-IDDC-RO-NDCA      TO W41419-DIFF-IDDC-RO-NDCA              
052411     MOVE DIFF-KDARTURS-NDCA     TO W41419-DIFF-KDARTURS-NDCA             
052412     MOVE DIFF-KDOI-NDCA         TO W41419-DIFF-KDOI-NDCA                 
052413     MOVE DIFF-KVPRERO-NDCA      TO W41419-DIFF-KVPRERO-NDCA              
052414     MOVE DIFF-VKART-NDCA        TO W41419-DIFF-VKART-NDCA                
052415     MOVE DIFF-VKART-NTO-NDCA    TO W41419-DIFF-VKART-NTO-NDCA            
052416     MOVE DIFF-VLARTNTO-NDCA     TO W41419-DIFF-VLARTNTO-NDCA             
052417     MOVE DIFF-OI-CLEAR-GRP-NDCA TO W41419-DIFF-OI-CLEAR-GRP-NDCA         
052418     MOVE DIFF-KDORDBEK-XDCA     TO W41419-DIFF-KDORDBEK-XDCA             
052419     MOVE DIFF-KVPREAVB-XDCA     TO W41419-DIFF-KVPREAVB-XDCA             
052420     MOVE DIFF-ADLAGOMR-XDCA     TO W41419-DIFF-ADLAGOMR-XDCA             
052421     MOVE DIFF-ADGANG-XDCA       TO W41419-DIFF-ADGANG-XDCA               
052422     MOVE DIFF-ADPLATS-XDCA      TO W41419-DIFF-ADPLATS-XDCA              
052423     MOVE DIFF-IDDC-XDCA         TO W41419-DIFF-IDDC-XDCA                 
052424     MOVE DIFF-IDDC-RO-XDCA      TO W41419-DIFF-IDDC-RO-XDCA              
052425     MOVE DIFF-KDARTURS-XDCA     TO W41419-DIFF-KDARTURS-XDCA             
052426     MOVE DIFF-KDOI-XDCA         TO W41419-DIFF-KDOI-XDCA                 
052427     MOVE DIFF-KVPRERO-XDCA      TO W41419-DIFF-KVPRERO-XDCA              
052428     MOVE DIFF-VKART-XDCA        TO W41419-DIFF-VKART-XDCA                
052429     MOVE DIFF-VKART-NTO-XDCA    TO W41419-DIFF-VKART-NTO-XDCA            
052430     MOVE DIFF-VLARTNTO-XDCA     TO W41419-DIFF-VLARTNTO-XDCA             
052431     MOVE DIFF-OI-CLEAR-GRP-XDCA TO W41419-DIFF-OI-CLEAR-GRP-XDCA         
052432     PERFORM S22-SKRIV-W41419                                             
052433     .                                                                    
052434     EJECT                                                                
052435 Z-FINIT SECTION.                                                         
052440     CLOSE W41401                                                         
052500           W41402                                                         
052600           W41404                                                         
052700           W41405                                                         
052800           W41410                                                         
052810           W41411                                                         
052811           W41412                                                         
052820           W41418                                                         
052830           W41419                                                         
052900     SKIP2                                                                
053000     MOVE 'S' TO POSTSUM-OPKOD                                            
053100     CALL POSTSUM USING POSTSUM-PARM                                      
053200     .                                                                    
053300     EJECT                                                                
053400 S11-SKRIV-W41401 SECTION.                                                
053500     SKIP2                                                                
053600     WRITE W41401-POST FROM W41401-AREA                                   
053700                                                                          
053800     MOVE W41401-OHUV-IDPTYP TO POSTSUM-TRANSTYP                          
053900     MOVE 'W41401' TO POSTSUM-FDNAMN                                      
054000     MOVE 'W41401D1' TO POSTSUM-DDNAMN2                                   
054100     CALL POSTSUM USING POSTSUM-PARM                                      
054200     .                                                                    
054300     EJECT                                                                
054400 S12-SKRIV-W41402-ORAD SECTION.                                           
054500     SKIP2                                                                
054600     WRITE W41402-ORAD-POST FROM W41402-ORAD-AREA                         
054700                                                                          
054800     MOVE W41402-ORAD-IDPTYP TO POSTSUM-TRANSTYP                          
054900     MOVE 'W41402' TO POSTSUM-FDNAMN                                      
055000     MOVE 'W41401D2' TO POSTSUM-DDNAMN2                                   
055100     CALL POSTSUM USING POSTSUM-PARM                                      
055200     .                                                                    
055300     EJECT                                                                
055400 S13-SKRIV-W41402-OBKR SECTION.                                           
055500     SKIP2                                                                
055600     WRITE W41402-OBKR-POST FROM W41402-OBKR-AREA                         
055700                                                                          
055800     MOVE W41402-OBKR-IDPTYP TO POSTSUM-TRANSTYP                          
055900     MOVE 'W41402' TO POSTSUM-FDNAMN                                      
056000     MOVE 'W41401D2' TO POSTSUM-DDNAMN2                                   
056100     CALL POSTSUM USING POSTSUM-PARM                                      
056200     .                                                                    
056300     EJECT                                                                
056400 S14-SKRIV-W41404 SECTION.                                                
056500     SKIP2                                                                
056600     WRITE W41404-POST FROM W41404-AREA                                   
056700                                                                          
056800     MOVE 'RENS'        TO POSTSUM-TRANSTYP                               
056900     MOVE 'W41404' TO POSTSUM-FDNAMN                                      
057000     MOVE 'W41401D3' TO POSTSUM-DDNAMN2                                   
057100     CALL POSTSUM USING POSTSUM-PARM                                      
057200     .                                                                    
057300     EJECT                                                                
057400 S15-SKRIV-W41402-TILLTPO SECTION.                                        
057500     SKIP2                                                                
057600     WRITE W41402-TILLTPO-POST FROM W41402-TILLTPO-AREA                   
057700                                                                          
057800     MOVE W41402-TILLTPO-IDPTYP TO POSTSUM-TRANSTYP                       
057900     MOVE 'W41402' TO POSTSUM-FDNAMN                                      
058000     MOVE 'W41401D2' TO POSTSUM-DDNAMN2                                   
058100     CALL POSTSUM USING POSTSUM-PARM                                      
058200     .                                                                    
058300     EJECT                                                                
058400 S16-SKRIV-W41405-REGTPO SECTION.                                         
058500     SKIP2                                                                
058600     WRITE W41405-REGTPO-POST FROM W41405-REGTPO-AREA                     
058700                                                                          
058800     MOVE W41405-REGTPO-IDPTYP TO POSTSUM-TRANSTYP                        
058900     MOVE 'W41405' TO POSTSUM-FDNAMN                                      
059000     MOVE 'W41401D4' TO POSTSUM-DDNAMN2                                   
059100     CALL POSTSUM USING POSTSUM-PARM                                      
059200     .                                                                    
059300     EJECT                                                                
059400 S17-SKRIV-W41402-ANNVOR SECTION.                                         
059500     SKIP2                                                                
059600     WRITE W41402-ANNVOR-POST FROM W41402-ANNVOR-AREA                     
059700                                                                          
059800     MOVE W41402-ANNVOR-IDPTYP TO POSTSUM-TRANSTYP                        
059900     MOVE 'W41402' TO POSTSUM-FDNAMN                                      
060000     MOVE 'W41401D2' TO POSTSUM-DDNAMN2                                   
060100     CALL POSTSUM USING POSTSUM-PARM                                      
060200     .                                                                    
060300     EJECT                                                                
060400 S18-SKRIV-W41410 SECTION.                                                
060500     SKIP2                                                                
060600     WRITE W41410-POST FROM W41410-AREA                                   
060700                                                                          
060800     MOVE 'DCSL'        TO POSTSUM-TRANSTYP                               
060900     MOVE 'W41410' TO POSTSUM-FDNAMN                                      
061000     MOVE 'W41401D5' TO POSTSUM-DDNAMN2                                   
061100     CALL POSTSUM USING POSTSUM-PARM                                      
061200     .                                                                    
061300     EJECT                                                                
061310 S19-SKRIV-W41411 SECTION.                                                
061320     SKIP2                                                                
061330     WRITE W41411-POST FROM W41411-AREA                                   
061340                                                                          
061350     MOVE '100'        TO POSTSUM-TRANSTYP                                
061360     MOVE 'W41411' TO POSTSUM-FDNAMN                                      
061370     MOVE 'W41401D6' TO POSTSUM-DDNAMN2                                   
061380     CALL POSTSUM USING POSTSUM-PARM                                      
061390     .                                                                    
061391     EJECT                                                                
061392 S20-SKRIV-W41412 SECTION.                                                
061393     SKIP2                                                                
061394     WRITE W41412-POST FROM W41412-AREA                                   
061395                                                                          
061396     MOVE 'BUM'        TO POSTSUM-TRANSTYP                                
061397     MOVE 'W41412' TO POSTSUM-FDNAMN                                      
061398     MOVE 'W41401D7' TO POSTSUM-DDNAMN2                                   
061399     CALL POSTSUM USING POSTSUM-PARM                                      
061400     .                                                                    
061401     EJECT                                                                
061402 S21-SKRIV-W41418 SECTION.                                                
061403     SKIP2                                                                
061404     WRITE W41418-POST FROM W41418-AREA                                   
061405                                                                          
061406     MOVE 'LOG'        TO POSTSUM-TRANSTYP                                
061407     MOVE 'W41412' TO POSTSUM-FDNAMN                                      
061408     MOVE 'W41401D8' TO POSTSUM-DDNAMN2                                   
061409     CALL POSTSUM USING POSTSUM-PARM                                      
061410     .                                                                    
061411     EJECT                                                                
061412 S22-SKRIV-W41419 SECTION.                                                
061413     SKIP2                                                                
061414     WRITE W41419-POST FROM W41419-AREA                                   
061415                                                                          
061416     MOVE 'LOG'        TO POSTSUM-TRANSTYP                                
061417     MOVE 'W41412' TO POSTSUM-FDNAMN                                      
061418     MOVE 'W41401D9' TO POSTSUM-DDNAMN2                                   
061419     CALL POSTSUM USING POSTSUM-PARM                                      
061420     .                                                                    
061421     EJECT                                                                
061430* --- IMS SEKTIONER ---                                                   
061500     SKIP3                                                                
061600 IMS-GN-FILA-FIL SECTION.                                                 
061700     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
061800     CALL CBLTDLI USING GN FILA-PCB DLI-IO-AREA                           
061900     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
062000     PERFORM IMS-STATUSKONTROLL                                           
062100     .                                                                    
062200     SKIP3                                                                
062300 IMS-STATUSKONTROLL SECTION.                                              
062400     SKIP2                                                                
062500     SET STATUS-IX TO 1                                                   
062600     SEARCH GODK-STATUS                                                   
062700       AT END                                                             
062800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
062900           DELIMITED BY SIZE INTO FELTEXT-STR                             
063000         DISPLAY FELTEXT                                                  
063100         CALL FELLOG                                                      
063200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
063300         CONTINUE                                                         
063400     END-SEARCH                                                           
063500     .                                                                    

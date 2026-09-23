000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5019700.                                                
000300 AUTHOR.         MARKUS ASPFJÄLL                                          
000400 DATE-WRITTEN.   98/09/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMET LÄGGER BARA UT INFO SOM KOMPLETTERAR                   
000900*        DEN INFO SOM BILD 5107 GER                                       
001000*        ÅTERHOPP TILL BILD 5107 SKER GENOM ENTER TRYCKNING               
001100*                                                                         
001200*                                                                         
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W5T197                                              
001600*        MID:         W5I197N1                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W5O197N1                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W5019700'.            
002900                                                                          
003000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
003800     88  ALLT-OK                             VALUE 'J'.                   
003900                                                                          
004000 77  BYT-SW                      PIC X       VALUE 'J'.                   
004100     88  BYT-BILD                            VALUE 'J'.                   
004200     88  BYT-EJ-BILD                         VALUE 'N'.                   
004300                                                                          
004400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004500     88  NYCKLAR-OK                          VALUE 'J'.                   
004600     88  NYCKLAR-FEL                         VALUE 'N'.                   
004700                                                                          
004800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004900     88  EGEN-MID                            VALUE '5197'.                
005000     88  GODK-MID                            VALUE '5107'                 
005100                                                   '5197'.                
005200     88  HELP-MID                            VALUE '0551'.                
005300     EJECT                                                                
005400*    ---AREA FÖR ATT HÄMTA UPP MSGI-SPAR-AREA TILL PGM                    
005500 01  FILLER                      PIC X(16) VALUE 'SPAR-AREA '.            
005600 01  SPAR-AREA.                                                           
005700     03  SPAR-IDARTNR            PIC X(9)  VALUE SPACE.                   
005800     03  SPAR-IDDC               PIC X(2)  VALUE SPACE.                   
005900     03  SPAR-IDINLEV-NEXT       PIC 9(16) VALUE ZERO.                    
006000     03  SPAR-IDINLEV-ENTER      PIC 9(16) VALUE ZERO.                    
006100     03  SPAR-IDPTYP             PIC X(3)  VALUE SPACE.                   
006200     03  SPAR-IDLOPNRM           PIC Z(7)9 VALUE ZERO.                    
006300     03  SPAR-TIAAVVD            PIC S9(5) VALUE ZERO.                    
006400     03  SPAR-IDLEVNR            PIC X(5)  VALUE SPACE.                   
006500     03  SPAR-KDRT               PIC Z9    VALUE ZERO.                    
006600     03  SPAR-IDFS               PIC X(8)  VALUE SPACE.                   
006700     03 FILLER REDEFINES SPAR-IDFS.                                       
006800       05 SPAR-IDAVINR           PIC 9(8).                                
006900     03  SPAR-TIAVSDAT           PIC 9(6)  VALUE ZERO.                    
007000     03  SPAR-IDKONTO            PIC Z(9)9 VALUE ZERO.                    
007100     03  SPAR-IDANALYS           PIC X(12) VALUE SPACE.                   
007200     03  SPAR-IDKST              PIC X(10) VALUE SPACE.                   
007300     03  SPAR-IDDISTR            PIC 9(5)  VALUE ZERO.                    
007400     03  SPAR-IDKUNDNR           PIC 9(7)  VALUE ZERO.                    
007500     03  SPAR-IDORDER            PIC 9(7)  VALUE ZERO.                    
007600     03  SPAR-IDPRODNR           PIC 9(7)  VALUE ZERO.                    
007700     03  SPAR-IDFAKT             PIC 9(7)  VALUE ZERO.                    
007800     03  SPAR-DATUM-SOK          PIC 9(6)  VALUE ZERO.                    
007900     03  SPAR-IDPTYP-SOK         PIC X(3)  VALUE SPACE.                   
008000     03  SPAR-IDLEVNR-SOK        PIC X(5)  VALUE SPACE.                   
008100     03  SPAR-KDRT-SOK           PIC 9(2)  VALUE ZERO.                    
008200     03  SPAR-IDAVINR-SOK        PIC X(8)  VALUE SPACE.                   
008300     03  SPAR-BILD               PIC X(4)  VALUE SPACE.                   
008400     03  SPAR-IDSHIPM            PIC 9(7)  VALUE ZERO.                    
008500     EJECT                                                                
008600*    ---GENERELLA ARBETSFÄLT                                              
008700     EJECT                                                                
008800                                                                          
008900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009000 01  GENERELLA-SUBPROGRAM.                                                
009100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     EJECT                                                                
009600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009700*01 -COPY WMEDAREA                                                        
009800     SKIP3                                                                
009900 01  MESSAGE-CODES.                                                       
010000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010300*                                                                         
010400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010500     SKIP3                                                                
010600*01 -COPY WMSGINIT                                                        
010700     EJECT                                                                
010800 01  STATUS-WS                   PIC XX.                                  
010900     88  SEGMENT-FINNS                       VALUE '  '.                  
011000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011200     SKIP2                                                                
011300                                                                          
011400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011700     SKIP3                                                                
011800*01  MID -COPY W5I19701                                                   
011900     EJECT                                                                
012000     EJECT                                                                
012100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012200     SKIP3                                                                
012300*01  -COPY WMSGAREA                                                       
012400     EJECT                                                                
012500     03  MOD REDEFINES MSG-AREA.                                          
012600*      05  -COPY W5O19701                                                 
012700     EJECT                                                                
012800 01  W-PROG-TO-PROG-SW-5107.                                              
012900     03  M-SW-LL-5107            PIC S9(4)   VALUE +240 COMP SYNC.        
013000     03  M-SW-Z1-Z2-5107         PIC X(2)    VALUE LOW-VALUE.             
013100     03  M-SW-KDTRANS-5107       PIC X(8)    VALUE 'W5T107  '.            
013200     03  M-SW-IDTRANS-5107       PIC X(4)    VALUE '5197'.                
013300     03  M-SW-KDMFSTYP-5107      PIC X(1)    VALUE '2'.                   
013400                                                                          
013500*    03  MID -COPY W5I10701 -PRE 5107-                                    
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013800     SKIP3                                                                
013900*01  -COPY WMFSAREA                                                       
014000     EJECT                                                                
014100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014200*                                                                         
014300     EJECT                                                                
014400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014500     SKIP3                                                                
014600 01  GODK-STATUSKODER.                                                    
014700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014800     SKIP3                                                                
014900 01  SSA1                        PIC X(64).                               
015000 01  SSA2                        PIC X(64).                               
015100     EJECT                                                                
015200*    --- IMS FUNKTIONSKODER                                               
015300*01  -COPY W0003                                                          
015400     EJECT                                                                
015500*    ---  DLI INPUT-OUTPUT AREA                                           
015600                                                                          
015700     EJECT                                                                
015800 LINKAGE SECTION.                                                         
015900*01  -COPY W0009   -PRE MSG-                                              
016000     EJECT                                                                
016100*01  -COPY W0009   -PRE ALT-                                              
016200     EJECT                                                                
016300*01  -COPY W0008   -PRE USEA-                                             
016400     05  FILLER                  PIC X.                                   
016500                                                                          
016600     EJECT                                                                
016700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB.                      
016800                                                                          
016900 MAIN SECTION.                                                            
017000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB.                      
017100                                                                          
017200                                                                          
017300     PERFORM IMS-GET-MSG                                                  
017400     IF SEGMENT-FINNS                                                     
017500       PERFORM A-INIT                                                     
017600       PERFORM B-KOLLA-NYCKLAR                                            
017700       IF NYCKLAR-OK                                                      
017800          IF MFS-ENTER AND EGEN-MID                                       
017900             PERFORM C-BYT-BILD                                           
018000          END-IF                                                          
018100          IF ALLT-OK                                                      
018200             PERFORM F-LAES-VISA-INFO                                     
018300          END-IF                                                          
018400       END-IF                                                             
018500       IF BYT-EJ-BILD                                                     
018600          COMPUTE MSG-KVLL = LENGTH OF MOD-W5O19701 + 4                   
018700          PERFORM IMS-INSERT-MSG                                          
018800       END-IF                                                             
018900     END-IF                                                               
019000                                                                          
019100     MOVE ZERO TO RETURN-CODE                                             
019200     GOBACK                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 A-INIT SECTION.                                                          
019600                                                                          
019700     IF MSG-DUBBLA-TRANSKODER                                             
019800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I19701                 
019900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
020000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
020100     ELSE                                                                 
020200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I19701                  
020300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
020400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
020500     END-IF                                                               
020600                                                                          
020700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
020900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
021000                                                                          
021100     MOVE LOW-VALUE TO MSG-AREA                                           
021200     MOVE 'W5O19701' TO MFS-IDMOD                                         
021300     MOVE '5197' TO MOD-IDTRANS                                           
021400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
021500                                                                          
021600                                                                          
021700     IF EGEN-MID OR HELP-MID                                              
021800       CONTINUE                                                           
021900     ELSE                                                                 
022000       MOVE SPACE TO MFS-KDTRTYP                                          
022100       MOVE '7' TO MFS-IDPFK                                              
022200     END-IF                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 B-KOLLA-NYCKLAR SECTION.                                                 
022600                                                                          
022700     MOVE JA TO NYCKLAR-SW                                                
022800                                                                          
022900* ---HÄR GÖRS INGEN KONTROLL AV NYCKLAR EFTERSOM MAN                      
023000* ---INTE HAR NÅGRA VALBARA FÄLT. DÄRFÖR FLYTTAS                          
023100* ---MID-AREAN TILL MOD-AREAN DIREKT                                      
023200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023300     MOVE '001'             TO MSGI-KDCALL                                
023400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
023600     MOVE '5197'            TO MSGI-IDTRANS                               
023700                                                                          
023800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023900                                                                          
024000     IF GODK-MID                                                          
024100        MOVE MSGI-SPAR-AREA      TO SPAR-AREA                             
024200     END-IF                                                               
024300                                                                          
024400* ---ANVÄNDS FÖR ATT SKICKA TILLBAKA MID-VÄRDEN                           
024500* ---TILL 5107-BILDEN                                                     
024600     IF NOT GODK-MID            OR                                        
024700       (GODK-MID AND                                                      
024800        SPAR-BILD NOT = '5197')                                           
024900       MOVE NEJ TO NYCKLAR-SW                                             
025000     ELSE                                                                 
025100       IF NOT EGEN-MID                                                    
025200          MOVE SPAR-IDARTNR         TO MOD-IDARTNR                        
025300          MOVE SPAR-IDDC            TO MOD-IDDC                           
025400       END-IF                                                             
025500     END-IF                                                               
025600     IF MSGI-IDLAND-SPR = 'SE'                                            
025700        MOVE 'S' TO MED-IDSKYLT                                           
025800     ELSE                                                                 
025900        IF MSGI-IDLAND-SPR = 'GB'                                         
026000            MOVE 'GB' TO MED-IDSKYLT                                      
026100        ELSE                                                              
026200            MOVE 'US' TO MED-IDSKYLT                                      
026300        END-IF                                                            
026400     END-IF                                                               
026500     MOVE NEJ TO BYT-SW                                                   
026600     MOVE JA TO ALLT-SW                                                   
026700                                                                          
026800     IF NYCKLAR-FEL                                                       
026900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
027000       CALL WMEDKONV USING MED-WMEDAREA                                   
027100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
027200       PERFORM MFS-RENSA-FAELT-UT                                         
027300     END-IF                                                               
027400     .                                                                    
027500     EJECT                                                                
027600 C-BYT-BILD SECTION.                                                      
027700     SKIP2                                                                
027800     MOVE JA TO BYT-SW                                                    
027900     MOVE NEJ TO ALLT-SW                                                  
028000                                                                          
028100* ---SKICKAR VÄRDE TILL 5107-MID FÖR ATT SEDAN                            
028200* ---STARTA UPP 5107-BILDEN                                               
028300     MOVE LOW-VALUE            TO 5107-MID-W5I10701                       
028400     MOVE SPAR-IDARTNR         TO 5107-MID-IDARTNR-IN                     
028500     MOVE SPAR-IDDC            TO 5107-MID-IDDC-IN                        
028600     MOVE ALL '+'              TO 5107-MID-DATUM-IN                       
028700     MOVE SPAR-DATUM-SOK       TO 5107-MID-DATUM-UT                       
028800     MOVE ALL '+'              TO 5107-MID-IDPTYP-IN                      
028900     MOVE SPAR-IDPTYP-SOK      TO 5107-MID-IDPTYP-UT                      
029000     MOVE ALL '+'              TO 5107-MID-IDLEVNR-IN                     
029100     MOVE SPAR-IDLEVNR-SOK     TO 5107-MID-IDLEVNR-UT                     
029200     MOVE ALL '+'              TO 5107-MID-KDRT-IN                        
029300     MOVE SPAR-KDRT-SOK        TO 5107-MID-KDRT-UT                        
029400     MOVE ALL '+'              TO 5107-MID-IDFS-IN                        
029500     MOVE SPAR-IDAVINR-SOK     TO 5107-MID-IDFS-UT                        
029600     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O19701 + 17                       
029700     PERFORM IMS-INSERT-ALT-MSG-5107                                      
029800     .                                                                    
029900     EJECT                                                                
030000 F-LAES-VISA-INFO SECTION.                                                
030100     IF SPAR-IDPTYP = 'R33'                                               
030200       MOVE SPAR-IDPTYP          TO MOD-IDPTYP2                           
030300       MOVE SPAR-IDDISTR         TO MOD-IDDISTR                           
030400       MOVE SPAR-IDKUNDNR        TO MOD-IDKUNDNR                          
030500       MOVE SPAR-IDORDER         TO MOD-IDORDER                           
030600       MOVE SPAR-IDPRODNR        TO MOD-IDPRODNR                          
030700       MOVE SPAR-IDFAKT          TO MOD-IDFAKT                            
030800       MOVE SPAR-IDKONTO         TO MOD-IDKONTO2                          
030900       MOVE SPAR-IDANALYS        TO MOD-IDANALYS2                         
031000       MOVE SPAR-IDKST           TO MOD-IDKST2                            
031100     ELSE                                                                 
031200       MOVE SPAR-IDARTNR         TO MOD-IDARTNR                           
031300       MOVE SPAR-IDDC            TO MOD-IDDC                              
031400       MOVE SPAR-IDPTYP          TO MOD-IDPTYP                            
031500       MOVE SPAR-IDLOPNRM        TO MOD-IDLOPNRM                          
031600       MOVE SPAR-TIAAVVD         TO MOD-TIAAVVD                           
031700       MOVE SPAR-IDLEVNR         TO MOD-IDLEVNR                           
031800       MOVE SPAR-KDRT            TO MOD-KDRT                              
031900       MOVE SPAR-TIAVSDAT        TO MOD-TIAVSDAT                          
032000       MOVE SPAR-IDKONTO         TO MOD-IDKONTO                           
032100       MOVE SPAR-IDANALYS        TO MOD-IDANALYS                          
032200       MOVE SPAR-IDKST           TO MOD-IDKST                             
032300       MOVE SPAR-IDSHIPM         TO MOD-IDSHIPM                           
032400     END-IF                                                               
032500     MOVE '002'      TO MSGI-KDCALL                                       
032600     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
032700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
032800     .                                                                    
032900     EJECT                                                                
033000 MFS-RENSA-FAELT-UT SECTION.                                              
033100                                                                          
033200*    --- ALLA UTDATA-FÄLT                                                 
033300     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR                                  
033400                             MOD-IDDC                                     
033500                             MOD-IDPTYP                                   
033600                             MOD-IDLOPNRM                                 
033700                             MOD-TIAAVVD                                  
033800                             MOD-IDLEVNR                                  
033900                             MOD-KDRT                                     
034000                             MOD-TIAVSDAT                                 
034100                             MOD-IDKONTO                                  
034200                             MOD-IDANALYS                                 
034300                             MOD-IDKST                                    
034400                             MOD-IDDISTR                                  
034500                             MOD-IDKUNDNR                                 
034600                             MOD-IDORDER                                  
034700                             MOD-IDFAKT                                   
034800                             MOD-IDKONTO2                                 
034900                             MOD-IDANALYS2                                
035000                             MOD-IDKST2                                   
035100                             MOD-IDSHIPM                                  
035200                                                                          
035300     .                                                                    
035400     SKIP3                                                                
035500*MFS-ROER-EJ-FAELT-UT  SECTION.                                           
035600                                                                          
035700*    --- ALLA UTDATA-FÄLT                                                 
035800*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR                                
035900*                              MOD-IDDC                                   
036000*                              MOD-IDPTYP                                 
036100*                              MOD-IDLOPNRM                               
036200*                              MOD-TIAAVVD                                
036300*                              MOD-IDLEVNR                                
036400*                              MOD-KDRT                                   
036500*                              MOD-TIAVSDAT                               
036600*                              MOD-IDANALYS                               
036700*                              MOD-IDKST                                  
036800*    .                                                                    
036900*    SKIP3                                                                
037000* --- IMS SEKTIONER ---                                                   
037100     SKIP3                                                                
037200 IMS-GET-MSG SECTION.                                                     
037300                                                                          
037400     MOVE '  QC' TO GODK-STATUSKODER                                      
037500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
037600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
037700     PERFORM IMS-STATUSKONTROLL                                           
037800     .                                                                    
037900     SKIP3                                                                
038000 IMS-INSERT-MSG SECTION.                                                  
038100                                                                          
038200     IF MSGI-IDLAND-SPR = 'GB'                                            
038300       MOVE 'N' TO MFS-KDHUVOMR                                           
038400     ELSE                                                                 
038500       MOVE 0   TO MFS-KDHUVOMR                                           
038600     END-IF                                                               
038700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
038800     MOVE SPACE TO GODK-STATUSKODER                                       
038900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
039000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039100     PERFORM IMS-STATUSKONTROLL                                           
039200     .                                                                    
039300     EJECT                                                                
039400 IMS-INSERT-ALT-MSG-5107 SECTION.                                         
039500                                                                          
039600     MOVE SPACE TO GODK-STATUSKODER                                       
039700     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW-5107               
039800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
039900     PERFORM IMS-STATUSKONTROLL                                           
040000     .                                                                    
040100     EJECT                                                                
040200 IMS-STATUSKONTROLL SECTION.                                              
040300                                                                          
040400     SET STATUS-IX TO 1                                                   
040500     SEARCH GODK-STATUS                                                   
040600       AT END                                                             
040700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
040800         DELIMITED BY SIZE INTO FELTEXT                                   
040900         CALL FELLOG                                                      
041000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041100         CONTINUE                                                         
041200     END-SEARCH                                                           
041300     .                                                                    

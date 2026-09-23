000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3017700.                                                
000300 AUTHOR.         GAVIN SMITH.                                             
000400 DATE-WRITTEN.   01/05/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISA HISTORIK FÖR BYTES RETURER.                                 
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDM6                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W3T177                                              
001400*        MID:         W3I177n1                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W3O177n1                                            
001800*                                                                         
001900*    CHANGE LOG:                                                          
002000*                                                                         
002100*    DIGAMBAR/20021003                                                    
002200*    WDM6E INDEX IS CHANGED TO REFER THE IDBYTRAP-9KOMPL INSTEAD          
002300*    OF THE IDBYTRAP. THIS IS TO SHOW THE DETAILS IN DESCENDING           
002400*    ORDER OF THE IDBYTRAP.IDBYTRAP-9KOMPL FIELD IS ADDED IN              
002500*    WDM611                                                               
002600                                                                          
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000                                                                          
003100 DATA DIVISION.                                                           
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)   VALUE 'W3017700'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003800                                                                          
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100                                                                          
004200 01  ALL-SPACE.                                                           
004300     03  FILLER                  PIC X(50)  VALUE SPACE.                  
004400 01  ALL-PLUS.                                                            
004500     03  FILLER                  PIC X(50)  VALUE ALL '+'.                
004600                                                                          
004700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  MAX-KVRADER                 PIC S9(4)  VALUE +13   COMP.             
005000*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005100                                                                          
005200                                                                          
005300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005400     88  NYCKLAR-OK                          VALUE 'J'.                   
005500     88  NYCKLAR-FEL                         VALUE 'N'.                   
005600                                                                          
005700 77  VALID-INTERVAL              PIC X       VALUE 'N'.                   
005800     88  VAL-INT                             VALUE 'J'.                   
005900     88  FEL-INT                             VALUE 'N'.                   
006000                                                                          
006100 77  HOPP-UT                    PIC X        VALUE 'N'.                   
006200     88  3172-HOPP                           VALUE 'J'.                   
006300     88  NO-3172-HOPP                        VALUE 'N'.                   
006400                                                                          
006500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006600     88  EGEN-MID                            VALUE '3177'.                
006700     88  GODK-MID                            VALUE '3177'.                
006800     88  HELP-MID                            VALUE '0551'.                
006900     EJECT                                                                
007000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007100 01  GENERELLA-SUBPROGRAM.                                                
007200     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
007300     03  W3017710                PIC X(8)    VALUE 'W3017710'.            
007400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL SUBPROGRAM WL01MCNV                              
007900*01 -COPY WL01MCNV                                                        
008000     SKIP3                                                                
008100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008400     SKIP3                                                                
008500*01 -COPY WMSGINIT                                                        
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
008800 01  REQU-AREA.                                                           
008900*    03 -COPY WZ01REQU                                                    
009000*    03 -COPY W30177I1                                                    
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
009300 01  RESP-AREA.                                                           
009400*    03 -COPY WZ01RESP                                                    
009500*    03 -COPY W30177O1                                                    
009600     EJECT                                                                
009700*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009800*                                                                         
009900 01  SPAR-AREA.                                                           
010000     03  FILLER                    PIC X(250).                            
010100     03  SPAR-IDTRANS              PIC X(4)    VALUE '3177'.              
010200     03  SPAR-IDARTNR-ENTER        PIC S9(9)        COMP-3.               
010300     03  SPAR-IDDISTR-ENTER        PIC S9(5)        COMP-3.               
010400     03  SPAR-IDKUNDNR-ENTER       PIC S9(7)        COMP-3.               
010600     03  SPAR-IDBYTRAP-ENTER       PIC S9(7)        COMP-3.               
010700     03  SPAR-IDBYTRAD-ENTER       PIC S9(5)        COMP-3.               
010800     03  SPAR-KDBYTSTA-ENTER       PIC X.                                 
010900     03  SPAR-IDDC-ENTER           PIC XX.                                
011000     03  SPAR-IDDISTR-NEXT         PIC S9(5)        COMP-3.               
011100     03  SPAR-IDKUNDNR-NEXT        PIC S9(7)        COMP-3.               
011200     03  SPAR-IDBYTRAP-NEXT        PIC S9(7)        COMP-3.               
011300     03  SPAR-IDBYTRAD-NEXT        PIC S9(5)        COMP-3.               
011400     03  SPAR-KDBYTSTA-NEXT        PIC X.                                 
011500     03  SPAR-IDDC-NEXT            PIC XX.                                
011600     03  SPAR-RAD OCCURS 13.                                              
011700       05 PAR-IDDISTR              PIC  9(4)        .                     
011800       05 PAR-IDBYTRAP             PIC S9(7)        COMP-3.               
011900     EJECT                                                                
012000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012100*                                                                         
012200                                                                          
012300 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA'.            
012400                                                                          
012500     SKIP3                                                                
012600                                                                          
012700 01  ALT-IO-3172.                                                         
012800     03 M-SW-LL                PIC   S9(4)  VALUE +367 COMP SYNC.         
012900     03 M-SW-Z1-Z2             PIC    X(2)  VALUE LOW-VALUE.              
013000     03 M-SW-KDTRANS           PIC    X(8)  VALUE 'W3T172 7'.             
013100     03 M-SW-IDTRANS           PIC    X(4)  VALUE '3177'.                 
013200     03 M-SW-KDMFSFOR          PIC    X(1)  VALUE '2'.                    
013300*03  -COPY W3I17201   -PRE  M-                                            
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013600     SKIP3                                                                
013700*01  MID -COPY W3I17701                                                   
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014000     SKIP3                                                                
014100*01  -COPY WMSGAREA                                                       
014200     EJECT                                                                
014300     03  MOD REDEFINES MSG-AREA.                                          
014400*      05  -COPY W3O17701                                                 
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014700     SKIP3                                                                
014800*01  -COPY WMFSAREA                                                       
014900     EJECT                                                                
015000*    --- STATUS-KOD FRÅN IMS                                              
015100 01  STATUS-WS                   PIC XX.                                  
015200     88  SEGMENT-FINNS                       VALUE '  '.                  
015300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015500     SKIP2                                                                
015600 01  GODK-STATUSKODER.                                                    
015700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015800     SKIP3                                                                
015900 01  SSA1                        PIC X(64).                               
016000 01  SSA2                        PIC X(64).                               
016100     EJECT                                                                
016200*    --- IMS FUNKTIONSKODER                                               
016300*01  -COPY W0003                                                          
016400     EJECT                                                                
016500*    ---  DLI INPUT-OUTPUT AREA                                           
016600                                                                          
016700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM6E1'.                      
016800 01  DLI-IO-WDM6E1.                                                       
016900*    03  -COPY WDM6E1                                                     
017000     EJECT                                                                
017100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM601'.                      
017200 01  DLI-IO-WDM601.                                                       
017300*    03  -COPY WDM601                                                     
017400     EJECT                                                                
017500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM611'.                      
017600 01  DLI-IO-WDM611.                                                       
017700*    03  -COPY WDM611                                                     
017800     EJECT                                                                
017900 LINKAGE SECTION.                                                         
018000*01  -COPY W0009   -PRE MSG-                                              
018100*01  -COPY W0009   -PRE ALT-                                              
018200*01  -COPY W0008   -PRE USEA-                                             
018300     05  FILLER                  PIC X.                                   
018400                                                                          
018500*01  -COPY W0008  -PRE WDM6E-                                             
018600     05  FILLER                  PIC X.                                   
018700*01  -COPY W0008  -PRE WDM6-                                              
018800     05  FILLER                  PIC X.                                   
018900     EJECT                                                                
019000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB WDM6E-PCB             
019100                                                    WDM6-PCB.             
019200 MAIN SECTION.                                                            
019300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB WDM6E-PCB             
019400                                                    WDM6-PCB.             
019500     PERFORM IMS-GET-MSG                                                  
019600     IF SEGMENT-FINNS                                                     
019700       PERFORM A-INIT                                                     
019800       PERFORM B-INIT-KEYS                                                
019900       PERFORM C-INIT-REQU                                                
020000       IF NYCKLAR-OK                                                      
020100         IF MFS-FIRST                                                     
020200           SET REQU-FIRST        TO TRUE                                  
020300           PERFORM MFS-RENSA-FAELT-IN                                     
020400         ELSE                                                             
020500           IF MFS-NEXT                                                    
020600             SET REQU-NEXT       TO TRUE                                  
020700             PERFORM D-NAESTA-SIDA                                        
020800           ELSE                                                           
020900             SET REQU-QUERY      TO TRUE                                  
021000             PERFORM E-SAMMA-SIDA                                         
021100           END-IF                                                         
021200         END-IF                                                           
021300         IF NO-3172-HOPP                                                  
021400           PERFORM F-CALL-BIZ-LOGIC-W3017710                              
021500           MOVE '002'            TO MSGI-KDCALL                           
021600           MOVE '3177'           TO SPAR-IDTRANS                          
021700           MOVE SPAR-AREA        TO MSGI-SPAR-AREA                        
021800           CALL W005INIT      USING MSGI-WMSGINIT USEA-PCB                
021900         END-IF                                                           
022000       END-IF                                                             
022100       IF NO-3172-HOPP                                                    
022200         COMPUTE MSG-KVLL = LENGTH OF MOD-W3O17701 + 4                    
022300         PERFORM IMS-INSERT-MSG                                           
022400       ELSE                                                               
022500         CONTINUE                                                         
022600       END-IF                                                             
022700     END-IF                                                               
022800                                                                          
022900     MOVE ZERO                   TO RETURN-CODE                           
023000                                                                          
023100     GOBACK                                                               
023200     .                                                                    
023300     EJECT                                                                
023400 A-INIT SECTION.                                                          
023500                                                                          
023600     IF MSG-DUBBLA-TRANSKODER                                             
023700       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
023800                                 TO MID-W3I17701                          
023900       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
024000       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
024100     ELSE                                                                 
024200       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
024300                                 TO MID-W3I17701                          
024400       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
024500       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
024600     END-IF                                                               
024700                                                                          
024800     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
024900     MOVE MSG-IDPFK              TO MFS-IDPFK                             
025000     MOVE MFS-IDTRANS            TO W-IDTRANS                             
025100                                                                          
025200     MOVE LOW-VALUE              TO MSG-AREA                              
025300     MOVE 'W3O177N1'             TO MFS-IDMOD                             
025400     MOVE '3177'                 TO MOD-IDTRANS                           
025500     MOVE MFS-RENSA-FAELT        TO MOD-TEMFSFEL                          
025600                                    MOD-TEMFSINF                          
025700                                                                          
025800     IF EGEN-MID OR HELP-MID                                              
025900       CONTINUE                                                           
026000     ELSE                                                                 
026100       MOVE SPACE                TO MFS-KDTRTYP                           
026200       MOVE '7'                  TO MFS-IDPFK                             
026300     END-IF                                                               
026400     .                                                                    
026500     EJECT                                                                
026600 B-INIT-KEYS SECTION.                                                     
026700                                                                          
026800     MOVE ALL '+'                TO MSGI-WMSGINIT                         
026900     MOVE '001'                  TO MSGI-KDCALL                           
027000     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
027100     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
027200     MOVE '3177'                 TO MSGI-IDTRANS                          
027300     IF EGEN-MID                                                          
027400       MOVE MID-IDARTNR          TO MSGI-IDARTNR                          
027500       MOVE MID-IDDISTR          TO MSGI-IDDISTR                          
027600       MOVE MID-IDKUNDNR         TO MSGI-IDKUNDNR                         
027700       MOVE MID-IDBYTRAP         TO MSGI-IDBYTRAP                         
027800       MOVE MID-KDBYTSTA         TO MSGI-KDBYTSTA                         
027900       MOVE MID-IDDC             TO MSGI-IDDC-KEY                         
028000     END-IF                                                               
028100     CALL W005INIT            USING MSGI-WMSGINIT                         
028200                                    USEA-PCB                              
028300     MOVE MSGI-SPAR-AREA         TO SPAR-AREA                             
028400                                                                          
028500     MOVE MSGI-IDSPRAK           TO MCNV-IDSPRAK                          
028600                                                                          
028700     MOVE '101'                  TO REQU-IDMSGVER                         
028800     MOVE MSGI-IDUSER            TO REQU-IDUSER                           
028900                                                                          
029000*    -- KONTROLL AV IDARTNR                                               
029100     MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR-IN                        
029200                                                                          
029300     IF MID-IDARTNR NOT = ALL '+'                                         
029400       MOVE '7'                  TO MFS-IDPFK                             
029500       MOVE SPACE                TO MFS-KDTRTYP                           
029600     END-IF                                                               
029700                                                                          
029800     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
029810     IF MSGI-IDARTNR NOT NUMERIC                                          
029811        MOVE ZERO TO MSGI-IDARTNR                                         
029820     END-IF                                                               
029900                                                                          
030000     MOVE MSGI-IDARTNR           TO REQU-IDARTNR-KEY                      
030100                                                                          
030200*    -- KONTROLL AV MID-IDDISTR                                           
030300     MOVE MFS-RENSA-FAELT        TO MOD-IDDISTR-IN                        
030400                                                                          
030500     IF MID-IDDISTR NOT = ALL '+'                                         
030600       MOVE '7'                  TO MFS-IDPFK                             
030700       MOVE SPACE                TO MFS-KDTRTYP                           
030800     END-IF                                                               
030900                                                                          
031000     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
031010     IF MSGI-IDDISTR NOT NUMERIC                                          
031020        MOVE ZERO TO MSGI-IDDISTR                                         
031030     END-IF                                                               
031100                                                                          
031200     MOVE MSGI-IDDISTR           TO REQU-IDDISTR-KEY                      
031300                                                                          
031400*    -- KONTROLL AV MID-IDKUNDNR-IN                                       
031500     MOVE MFS-RENSA-FAELT        TO MOD-IDKUNDNR-IN                       
031600                                                                          
031700     IF MID-IDKUNDNR NOT = ALL '+'                                        
031800       MOVE '7'                  TO MFS-IDPFK                             
031900       MOVE SPACE                TO MFS-KDTRTYP                           
032000     END-IF                                                               
032100                                                                          
032200     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
032210     IF MSGI-IDKUNDNR NOT NUMERIC                                         
032220        MOVE ZERO TO MSGI-IDKUNDNR                                        
032230     END-IF                                                               
032300                                                                          
032400     MOVE MSGI-IDKUNDNR          TO REQU-IDKUNDNR-KEY                     
032500                                                                          
032600*    -- KONTROLL AV MID-IDBYTRAP-IN                                       
032700     MOVE MFS-RENSA-FAELT        TO MOD-IDBYTRAP-IN                       
032800                                                                          
032900     IF MID-IDBYTRAP NOT = ALL '+'                                        
033000       MOVE '7'                  TO MFS-IDPFK                             
033100       MOVE SPACE                TO MFS-KDTRTYP                           
033200     END-IF                                                               
033300                                                                          
033400     INSPECT MSGI-IDBYTRAP REPLACING LEADING SPACE BY ZERO                
033410     IF MSGI-IDBYTRAP NOT NUMERIC                                         
033420        MOVE ZERO TO MSGI-IDBYTRAP                                        
033430     END-IF                                                               
033440                                                                          
033500     MOVE MSGI-IDBYTRAP          TO REQU-IDBYTRAP-KEY                     
033600                                                                          
033700*    -- KONTROLL AV MID-KDBYTSTA                                          
033800     MOVE MFS-RENSA-FAELT        TO MOD-KDBYTSTA-IN                       
033900                                                                          
034000     IF MID-KDBYTSTA NOT = ALL '+'                                        
034100       MOVE '7'                  TO MFS-IDPFK                             
034200       MOVE SPACE                TO MFS-KDTRTYP                           
034300     END-IF                                                               
034400                                                                          
034500     MOVE MSGI-KDBYTSTA          TO REQU-KDBYTSTA-KEY                     
034600                                                                          
034700*    -- KONTROLL AV MID-IDDC                                              
034800     MOVE MFS-RENSA-FAELT        TO MOD-IDDC-IN                           
034900                                                                          
035000     IF MID-IDDC NOT = ALL '+'                                            
035100       MOVE '7'                  TO MFS-IDPFK                             
035200       MOVE SPACE                TO MFS-KDTRTYP                           
035300     END-IF                                                               
035400                                                                          
035500     MOVE MSGI-IDDC-KEY          TO REQU-IDDC-KEY                         
035600***********************************************                           
035700     IF GODK-MID                                                          
035800       MOVE MSGI-IDARTNR         TO MOD-IDARTNR-UT                        
035900       MOVE MSGI-IDDISTR         TO MOD-IDDISTR-UT                        
036000       MOVE MSGI-IDKUNDNR        TO MOD-IDKUNDNR-UT                       
036100       MOVE MSGI-IDBYTRAP        TO MOD-IDBYTRAP-UT                       
036200       MOVE MSGI-KDBYTSTA        TO MOD-KDBYTSTA-UT                       
036300       MOVE MSGI-IDDC-KEY        TO MOD-IDDC-UT                           
036400       INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                
036500     ELSE                                                                 
036600       MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-UT                        
036700                                    MOD-IDDISTR-UT                        
036800                                    MOD-IDKUNDNR-UT                       
036900                                    MOD-IDBYTRAP-UT                       
037000                                    MOD-KDBYTSTA-UT                       
037100                                    MOD-IDDC-UT                           
037200     END-IF                                                               
037300                                                                          
037400     .                                                                    
037500     EJECT                                                                
037600 C-INIT-REQU SECTION.                                                     
037700                                                                          
037800     MOVE MAX-KVRADER            TO REQU-KVRADER                          
037900                                                                          
038000     .                                                                    
038100     EJECT                                                                
038200 D-NAESTA-SIDA SECTION.                                                   
038300                                                                          
038400     IF SPAR-IDTRANS = '3177'                                             
038500       MOVE SPAR-IDARTNR-ENTER   TO REQU-IDARTNR-START                    
038600       MOVE SPAR-IDDISTR-NEXT    TO REQU-IDDISTR-START                    
038700       MOVE SPAR-IDKUNDNR-NEXT   TO REQU-IDKUNDNR-START                   
038800       MOVE SPAR-IDBYTRAP-NEXT                                            
038900                                 TO REQU-IDBYTRAP-START                   
039000       MOVE SPAR-KDBYTSTA-NEXT   TO REQU-KDBYTSTA-START                   
039100       MOVE SPAR-IDDC-NEXT       TO REQU-IDDC-START                       
039200       MOVE SPAR-IDBYTRAD-NEXT   TO REQU-IDBYTRAD-START                   
039300     ELSE                                                                 
039400       PERFORM MFS-RENSA-FAELT-IN                                         
039500     END-IF                                                               
039600     .                                                                    
039700     EJECT                                                                
039800 E-SAMMA-SIDA SECTION.                                                    
039900     MOVE +1                     TO INDX                                  
040000     PERFORM                                                              
040100       UNTIL INDX > MAX-KVRADER                                           
040200       IF MID-KDSVAR (INDX)  =  'S' OR 'S'                                
040300         MOVE ALL '+'            TO M-MID-W3I17201                        
040400         MOVE PAR-IDDISTR(INDX)  TO M-MID-IDDISTR-IN                      
040500         MOVE PAR-IDBYTRAP(INDX) TO M-MID-IDBYTRAP-IN                     
040600         PERFORM IMS-INSERT-ALT-3172                                      
040700         MOVE +14                TO INDX                                  
040800         MOVE 'J'                TO HOPP-UT                               
040900       END-IF                                                             
041000       ADD +1                    TO INDX                                  
041100     END-PERFORM                                                          
041200     IF SPAR-IDTRANS = '3177'                                             
041300       MOVE SPAR-IDARTNR-ENTER   TO REQU-IDARTNR-START                    
041400       MOVE SPAR-IDDISTR-ENTER   TO REQU-IDDISTR-START                    
041500       MOVE SPAR-IDKUNDNR-ENTER  TO REQU-IDKUNDNR-START                   
041600       MOVE SPAR-IDBYTRAP-ENTER                                           
041700                                 TO REQU-IDBYTRAP-START                   
041800       MOVE SPAR-KDBYTSTA-ENTER  TO REQU-KDBYTSTA-START                   
041900       MOVE SPAR-IDDC-ENTER      TO REQU-IDDC-START                       
042000       MOVE SPAR-IDBYTRAD-ENTER  TO REQU-IDBYTRAD-START                   
042100     ELSE                                                                 
042200       PERFORM MFS-RENSA-FAELT-IN                                         
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600 F-CALL-BIZ-LOGIC-W3017710 SECTION.                                       
042700                                                                          
042800     CALL W3017710           USING REQU-AREA RESP-AREA                    
042900                                   MAX-KVRADER                            
043000                                   WDM6E-PCB WDM6-PCB                     
043100                                                                          
043200     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
043300        RESP-IDMSG-INFO  NOT = SPACE                                      
043400       PERFORM FA-SET-MSG-AND-HILIGHT                                     
043500     END-IF                                                               
043600     PERFORM FB-MOVE-RESP-TO-MOD                                          
043700     .                                                                    
043800     EJECT                                                                
043900 FA-SET-MSG-AND-HILIGHT SECTION.                                          
044000                                                                          
044100     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
044200     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
044300     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
044400                                                                          
044500     CALL WL01MCNV            USING MCNV-AREA                             
044600     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
044700     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
044800     .                                                                    
044900     EJECT                                                                
045000 FB-MOVE-RESP-TO-MOD SECTION.                                             
045100                                                                          
045200     IF RESP-IDARTNR-START = SPACE                                        
045300       MOVE ZERO                 TO SPAR-IDARTNR-ENTER                    
045400     ELSE                                                                 
045500       IF RESP-IDARTNR-START = ALL '+'                                    
045600         CONTINUE                                                         
045700       ELSE                                                               
045800         MOVE RESP-IDARTNR-START TO SPAR-IDARTNR-ENTER                    
045900       END-IF                                                             
046000     END-IF                                                               
046100                                                                          
046200     IF RESP-IDDISTR-START = SPACE                                        
046300       MOVE ZERO                 TO SPAR-IDDISTR-ENTER                    
046400     ELSE                                                                 
046500       IF RESP-IDDISTR-START = ALL '+'                                    
046600         CONTINUE                                                         
046700       ELSE                                                               
046800         MOVE RESP-IDDISTR-START TO SPAR-IDDISTR-ENTER                    
046900       END-IF                                                             
047000     END-IF                                                               
047100                                                                          
047200     IF RESP-IDDISTR-NEXT = SPACE                                         
047300       MOVE ZERO                 TO SPAR-IDDISTR-NEXT                     
047400     ELSE                                                                 
047500       IF RESP-IDDISTR-NEXT = ALL '+'                                     
047600         CONTINUE                                                         
047700       ELSE                                                               
047800         MOVE RESP-IDDISTR-NEXT  TO SPAR-IDDISTR-NEXT                     
047900       END-IF                                                             
048000     END-IF                                                               
048100                                                                          
048200     IF RESP-IDKUNDNR-START = SPACE                                       
048300       MOVE ZERO                 TO SPAR-IDKUNDNR-ENTER                   
048400     ELSE                                                                 
048500       IF RESP-IDKUNDNR-START = ALL '+'                                   
048600         CONTINUE                                                         
048700       ELSE                                                               
048800         MOVE RESP-IDKUNDNR-START                                         
048900                                 TO SPAR-IDKUNDNR-ENTER                   
049000       END-IF                                                             
049100     END-IF                                                               
049200                                                                          
049300     IF RESP-IDKUNDNR-NEXT = SPACE                                        
049400       MOVE ZERO                 TO SPAR-IDKUNDNR-NEXT                    
049500     ELSE                                                                 
049600       IF RESP-IDKUNDNR-NEXT = ALL '+'                                    
049700         CONTINUE                                                         
049800       ELSE                                                               
049900         MOVE RESP-IDKUNDNR-NEXT TO SPAR-IDKUNDNR-NEXT                    
050000       END-IF                                                             
050100     END-IF                                                               
050200                                                                          
050300     IF RESP-IDBYTRAP-START = SPACE                                       
050400       MOVE ZERO                 TO SPAR-IDBYTRAP-ENTER                   
050500     ELSE                                                                 
050600       IF RESP-IDBYTRAP-START = ALL '+'                                   
050700         CONTINUE                                                         
050800       ELSE                                                               
050900         MOVE RESP-IDBYTRAP-START                                         
051000                                 TO SPAR-IDBYTRAP-ENTER                   
051100       END-IF                                                             
051200     END-IF                                                               
051300                                                                          
051400     IF RESP-IDBYTRAP-NEXT = SPACE                                        
051500       MOVE ZERO                 TO SPAR-IDBYTRAP-NEXT                    
051600     ELSE                                                                 
051700       IF RESP-IDBYTRAP-NEXT = ALL '+'                                    
051800         MOVE ZERO               TO SPAR-IDBYTRAP-NEXT                    
051900       ELSE                                                               
052000         MOVE RESP-IDBYTRAP-NEXT                                          
052100                                 TO SPAR-IDBYTRAP-NEXT                    
052200       END-IF                                                             
052300     END-IF                                                               
052400                                                                          
052500     IF RESP-KDBYTSTA-START NOT = ALL '+'                                 
053200       MOVE RESP-KDBYTSTA-START    TO SPAR-KDBYTSTA-ENTER                 
053300     END-IF                                                               
053400     IF RESP-KDBYTSTA-NEXT  NOT = ALL '+'                                 
053500       MOVE RESP-KDBYTSTA-NEXT     TO SPAR-KDBYTSTA-NEXT                  
053600     END-IF                                                               
053700                                                                          
053800     IF RESP-IDDC-START     NOT = ALL '+'                                 
053900       MOVE RESP-IDDC-START        TO SPAR-IDDC-ENTER                     
054000     END-IF                                                               
054100     IF RESP-IDDC-NEXT      NOT = ALL '+'                                 
054200       MOVE RESP-IDDC-NEXT         TO SPAR-IDDC-NEXT                      
054300     END-IF                                                               
054500                                                                          
056600     IF RESP-IDBYTRAD-START = SPACE                                       
056700       MOVE ZERO                 TO SPAR-IDBYTRAD-ENTER                   
056800     ELSE                                                                 
056900       IF RESP-IDBYTRAD-START = ALL '+'                                   
057000         CONTINUE                                                         
057100       ELSE                                                               
057200         MOVE RESP-IDBYTRAD-START                                         
057300                                 TO SPAR-IDBYTRAD-ENTER                   
057400       END-IF                                                             
057500     END-IF                                                               
057600                                                                          
057700     IF RESP-IDBYTRAD-NEXT = SPACE                                        
057800       MOVE ZERO                 TO SPAR-IDBYTRAD-NEXT                    
057900     ELSE                                                                 
058000       IF RESP-IDBYTRAD-NEXT = ALL '+'                                    
058100         CONTINUE                                                         
058200       ELSE                                                               
058300         MOVE RESP-IDBYTRAD-NEXT TO SPAR-IDBYTRAD-NEXT                    
058400       END-IF                                                             
058500     END-IF                                                               
058600                                                                          
058700     PERFORM                                                              
058800     VARYING INDX FROM +1 BY +1                                           
058900       UNTIL INDX > RESP-KVRADER                                          
059000                                                                          
059100       IF RESP-IDDISTR-LINE (INDX) = SPACE                                
059200         MOVE MFS-RENSA-FAELT    TO MOD-IDDISTR (INDX)                    
059300         MOVE ZERO               TO PAR-IDDISTR (INDX)                    
059400       ELSE                                                               
059500         IF RESP-IDDISTR-LINE (INDX) = ALL '+'                            
059600           MOVE MFS-ROER-EJ-FAELT                                         
059700                                 TO MOD-IDDISTR (INDX)                    
059800         ELSE                                                             
059900           MOVE RESP-IDDISTR-LINE (INDX)                                  
060000                                 TO MOD-IDDISTR (INDX)                    
060100                                    PAR-IDDISTR (INDX)                    
060200         END-IF                                                           
060300       END-IF                                                             
060400                                                                          
060500       IF RESP-IDKUNDNR-LINE (INDX) = SPACE                               
060600         MOVE MFS-RENSA-FAELT    TO MOD-IDKUNDNR (INDX)                   
060800       ELSE                                                               
060900         IF RESP-IDKUNDNR-LINE (INDX) = ALL '+'                           
061000           MOVE MFS-ROER-EJ-FAELT                                         
061100                                 TO MOD-IDKUNDNR (INDX)                   
061200         ELSE                                                             
061300           MOVE RESP-IDKUNDNR-LINE (INDX)                                 
061400                                 TO MOD-IDKUNDNR (INDX)                   
061500         END-IF                                                           
061600       END-IF                                                             
061700                                                                          
061800       IF RESP-IDBYTRAP-LINE (INDX) = SPACE                               
061900         MOVE MFS-RENSA-FAELT    TO MOD-IDBYTRAP (INDX)                   
062000         MOVE ZERO               TO PAR-IDBYTRAP (INDX)                   
062100       ELSE                                                               
062200         IF RESP-IDBYTRAP-LINE (INDX) = ALL '+'                           
062300           MOVE MFS-ROER-EJ-FAELT                                         
062400                                 TO MOD-IDBYTRAP (INDX)                   
062500         ELSE                                                             
062600           MOVE RESP-IDBYTRAP-LINE (INDX)                                 
062700                                 TO MOD-IDBYTRAP (INDX)                   
062800                                    PAR-IDBYTRAP (INDX)                   
062900         END-IF                                                           
063000       END-IF                                                             
063100                                                                          
063200       IF RESP-TIREGDAT-LINE (INDX) = SPACE                               
063300         MOVE MFS-RENSA-FAELT    TO MOD-TIREGDAT (INDX)                   
063400       ELSE                                                               
063500         IF RESP-TIREGDAT-LINE (INDX) = ALL '+'                           
063600           MOVE MFS-ROER-EJ-FAELT                                         
063700                                 TO MOD-TIREGDAT (INDX)                   
063800         ELSE                                                             
063900           MOVE RESP-TIREGDAT-LINE (INDX)                                 
064000                                 TO MOD-TIREGDAT (INDX)                   
064100         END-IF                                                           
064200       END-IF                                                             
064300                                                                          
064400       IF RESP-TIANKDAG-LINE (INDX) = SPACE                               
064500         MOVE MFS-RENSA-FAELT    TO MOD-TIANKDAG (INDX)                   
064600       ELSE                                                               
064700         IF RESP-TIANKDAG-LINE (INDX) = ALL '+'                           
064800           MOVE MFS-ROER-EJ-FAELT                                         
064900                                 TO MOD-TIANKDAG (INDX)                   
065000         ELSE                                                             
065100           MOVE RESP-TIANKDAG-LINE (INDX)                                 
065200                                 TO MOD-TIANKDAG (INDX)                   
065300         END-IF                                                           
065400       END-IF                                                             
065500                                                                          
065600       IF RESP-TIREGDAT-GODK-LINE (INDX) = SPACE                          
065700         MOVE MFS-RENSA-FAELT    TO MOD-TIREGDAT-GODK (INDX)              
065800       ELSE                                                               
065900         IF RESP-TIREGDAT-GODK-LINE (INDX) = ALL '+'                      
066000           MOVE MFS-ROER-EJ-FAELT                                         
066100                                 TO MOD-TIREGDAT-GODK (INDX)              
066200         ELSE                                                             
066300           MOVE RESP-TIREGDAT-GODK-LINE (INDX)                            
066400                                 TO MOD-TIREGDAT-GODK (INDX)              
066500         END-IF                                                           
066600       END-IF                                                             
066700                                                                          
066800       IF RESP-KDBYTSTA-LINE (INDX) = SPACE                               
066900         MOVE MFS-RENSA-FAELT    TO MOD-KDBYTSTA (INDX)                   
067000       ELSE                                                               
067100         IF RESP-KDBYTSTA-LINE (INDX) = ALL '+'                           
067200           MOVE MFS-ROER-EJ-FAELT                                         
067300                                 TO MOD-KDBYTSTA (INDX)                   
067400         ELSE                                                             
067500           MOVE RESP-KDBYTSTA-LINE (INDX)                                 
067600                                 TO MOD-KDBYTSTA (INDX)                   
067700         END-IF                                                           
067800       END-IF                                                             
067900                                                                          
068000       IF RESP-FLBYTGAR-LINE (INDX) = SPACE                               
068100         MOVE MFS-RENSA-FAELT    TO MOD-FLBYTGAR (INDX)                   
068200       ELSE                                                               
068300         IF RESP-FLBYTGAR-LINE (INDX) = ALL '+'                           
068400           MOVE MFS-ROER-EJ-FAELT                                         
068500                                 TO MOD-FLBYTGAR (INDX)                   
068600         ELSE                                                             
068700           MOVE RESP-FLBYTGAR-LINE (INDX)                                 
068800                                 TO MOD-FLBYTGAR (INDX)                   
068900         END-IF                                                           
069000       END-IF                                                             
069100                                                                          
069200       IF RESP-KVRETUR-URSP-LINE (INDX) = SPACE                           
069300         MOVE MFS-RENSA-FAELT    TO MOD-KVRETUR-URSP (INDX)               
069400       ELSE                                                               
069500         IF RESP-KVRETUR-URSP-LINE (INDX) = ALL '+'                       
069600           MOVE MFS-ROER-EJ-FAELT                                         
069700                                 TO MOD-KVRETUR-URSP (INDX)               
069800         ELSE                                                             
069900           MOVE RESP-KVRETUR-URSP-LINE (INDX)                             
070000                                 TO MOD-KVRETUR-URSP (INDX)               
070100         END-IF                                                           
070200       END-IF                                                             
070300                                                                          
070400       IF RESP-KVRETUR-GODK-LINE (INDX) = SPACE                           
070500         MOVE MFS-RENSA-FAELT    TO MOD-KVRETUR-GODK (INDX)               
070600       ELSE                                                               
070700         IF RESP-KVRETUR-GODK-LINE (INDX) = ALL '+'                       
070800           MOVE MFS-ROER-EJ-FAELT                                         
070900                                 TO MOD-KVRETUR-GODK (INDX)               
071000         ELSE                                                             
071100           MOVE RESP-KVRETUR-GODK-LINE (INDX)                             
071200                                 TO MOD-KVRETUR-GODK (INDX)               
071300         END-IF                                                           
071400       END-IF                                                             
071500                                                                          
071600       IF RESP-ANMARKNINGKOD-LINE (INDX) = SPACE                          
071700         MOVE MFS-RENSA-FAELT    TO MOD-ANMARKNINGKOD (INDX)              
071800       ELSE                                                               
071900         IF RESP-ANMARKNINGKOD-LINE (INDX) = ALL '+'                      
072000           MOVE MFS-ROER-EJ-FAELT                                         
072100                                 TO MOD-ANMARKNINGKOD (INDX)              
072200         ELSE                                                             
072300           MOVE RESP-ANMARKNINGKOD-LINE (INDX)                            
072400                                 TO MOD-ANMARKNINGKOD (INDX)              
072500         END-IF                                                           
072600       END-IF                                                             
072700                                                                          
072800     END-PERFORM                                                          
072900                                                                          
073000     PERFORM                                                              
073100     VARYING INDX FROM INDX BY +1                                         
073200       UNTIL INDX > MAX-KVRADER                                           
073300       MOVE MFS-RENSA-FAELT      TO MOD-IDDISTR (INDX)                    
073400                                    MOD-IDKUNDNR (INDX)                   
073500                                    MOD-IDBYTRAP (INDX)                   
073600                                    MOD-TIREGDAT (INDX)                   
073700                                    MOD-TIANKDAG (INDX)                   
073800                                    MOD-TIREGDAT-GODK (INDX)              
073900                                    MOD-KDBYTSTA (INDX)                   
074000                                    MOD-FLBYTGAR (INDX)                   
074100                                    MOD-KVRETUR-URSP (INDX)               
074200                                    MOD-KVRETUR-GODK (INDX)               
074300                                    MOD-ANMARKNINGKOD (INDX)              
074400       MOVE MFS-CLOSE-FIELD      TO MOD-KDSVAR-ATTR (INDX)                
074500     END-PERFORM                                                          
074600                                                                          
074700     .                                                                    
074800     EJECT                                                                
074900 MFS-RENSA-FAELT-IN SECTION.                                              
075000                                                                          
075100     PERFORM                                                              
075200     VARYING INDX FROM +1 BY +1                                           
075300       UNTIL INDX > MAX-KVRADER                                           
075400       MOVE MFS-RENSA-FAELT      TO MOD-KDSVAR (INDX)                     
075500     END-PERFORM                                                          
075600                                                                          
075700     .                                                                    
075800     EJECT                                                                
075900* --- IMS SEKTIONER ---                                                   
076000     SKIP3                                                                
076100 IMS-GET-MSG SECTION.                                                     
076200                                                                          
076300     MOVE '  QC' TO GODK-STATUSKODER                                      
076400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
076500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076600     PERFORM IMS-STATUSKONTROLL                                           
076700     .                                                                    
076800     SKIP3                                                                
076900 IMS-INSERT-MSG SECTION.                                                  
077000                                                                          
077100     IF MSGI-IDLAND-SPR = 'GB'                                            
077200       MOVE 'N' TO MFS-KDHUVOMR                                           
077300     END-IF                                                               
077400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
077500     MOVE SPACE TO GODK-STATUSKODER                                       
077600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
077700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
077800     PERFORM IMS-STATUSKONTROLL                                           
077900     .                                                                    
078000     EJECT                                                                
078100 IMS-INSERT-ALT-3172 SECTION.                                             
078200                                                                          
078300     MOVE SPACE TO GODK-STATUSKODER                                       
078400     CALL CBLTDLI USING ISRT ALT-PCB ALT-IO-3172                          
078500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
078600     PERFORM IMS-STATUSKONTROLL                                           
078700     .                                                                    
078800     EJECT                                                                
078900 IMS-STATUSKONTROLL SECTION.                                              
079000                                                                          
079100     SET STATUS-IX TO 1                                                   
079200     SEARCH GODK-STATUS                                                   
079300       AT END                                                             
079400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
079500         DELIMITED BY SIZE INTO FELTEXT                                   
079600         CALL FELLOG                                                      
079700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
079800         CONTINUE                                                         
079900     END-SEARCH                                                           
080000     .                                                                    

000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6010800.                                                
000400*AUTHOR.         ROS-MARIE CLASON - GUIDE DATAKONSULT AB.                 
000500*DATE-WRITTEN.   92/02/26.                                                
000600                                                                          
000700**   REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        ALLMÄN BESKRIVNING:                                              
001100*        PROGRAMMET ÄR EN MPP SOM VISAR ANGIVEN ARTIKELS                  
001200*        FÖRBEHANDLINGS- OCH/ELLER FÖRPACKNINGSGRUPP.                     
001300*        OCH OLIKA ANSVARIGA FÖR ARTIKELN                                 
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W6T108                                              
001700*        MID:         W6I10801                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W6O10801                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W6010800'.            
003000                                                                          
003100*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003200 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
003300                                                                          
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600 77  SW-TRAEFF                   PIC X       VALUE SPACE.                 
003700                                                                          
003800*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
003900                                                                          
004000 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
004100 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
004200                                                                          
004300*      --- VALID IDDC CODES                                               
004400*01    -COPY WWDCKONS                                                     
004500*01    -COPY WWDC99                                                       
004600       EJECT                                                              
004700                                                                          
004800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004900     88  OWN-MID                             VALUE '6108'.                
005000     88  GOOD-MID                            VALUE '6108'.                
005100     88  HELP-MID                            VALUE '0551'.                
005200     EJECT                                                                
005300                                                                          
005400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
005500 01  GENERAL-SUBPROGRAM.                                                  
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005900     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
006000     03  W6010810                PIC X(8)    VALUE 'W6010810'.            
006100     EJECT                                                                
006200                                                                          
006300*01 -COPY WMSGINIT                                                        
006400     SKIP3                                                                
006500*    ---  COPYTEXT FÖR TRANS TILL WL01MCNV                                
006600*01  -COPY WL01MCNV                                                       
006700     EJECT                                                                
006800 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
006900 01  REQU-AREA.                                                           
007000*    03 -COPY WZ01REQU                                                    
007100*    03 -COPY W60108I1                                                    
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
007400 01  RESP-AREA.                                                           
007500*    03 -COPY WZ01RESP                                                    
007600*    03 -COPY W60108O1                                                    
007700     EJECT                                                                
007800*    --- AREAS FOR MFS AND SCREENHANDLING                                 
007900*                                                                         
008000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008100     SKIP3                                                                
008200*01  MID -COPY W6I10801                                                   
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
008500     SKIP3                                                                
008600*01  -COPY WMSGAREA                                                       
008700     EJECT                                                                
008800                                                                          
008900     03  MOD REDEFINES MSG-AREA.                                          
009000*      05  -COPY W6O10801                                                 
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009300     SKIP3                                                                
009400*01  -COPY WMFSAREA                                                       
009500     EJECT                                                                
009600                                                                          
009700*    --- WORK-AREAS FOR IMS-SECTIONS                                      
009800*                                                                         
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     SKIP3                                                                
010100*                                                                         
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FOUND                       VALUE '  '.                  
010500     SKIP2                                                                
010600 01  GOOD-STATUSCODES.                                                    
010700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010800     SKIP3                                                                
010900     EJECT                                                                
011000*                                                                         
011100*    --- IMS FUNCTION CODES                                               
011200*01  -COPY W0003                                                          
011300     EJECT                                                                
011400*                                                                         
011500 LINKAGE SECTION.                                                         
011600                                                                          
011700*01  -COPY W0009   -PRE MSG-                                              
011800     EJECT                                                                
011900 01  USEA-PCB                    PIC X.                                   
012000 01  HANA-PCB                    PIC X.                                   
012100 01  PLAA-PCB                    PIC X.                                   
012200 01  WDP3-PCB                    PIC X.                                   
012300 01  ARTC-PCB                    PIC X.                                   
012400 01  WDK7-PCB                    PIC X.                                   
012500 01  WDP3A-PCB                   PIC X.                                   
012600 01  WDP3B-PCB                   PIC X.                                   
012700 01  WDP3C-PCB                   PIC X.                                   
012800     EJECT                                                                
012900 PROCEDURE DIVISION  USING MSG-PCB                                        
013000                           USEA-PCB                                       
013100                           HANA-PCB                                       
013200                           PLAA-PCB                                       
013300                           WDP3-PCB                                       
013400                           ARTC-PCB                                       
013500                           WDK7-PCB                                       
013600                           WDP3A-PCB                                      
013700                           WDP3B-PCB                                      
013800                           WDP3C-PCB.                                     
013900                                                                          
014000     ENTRY 'DLITCBL' USING MSG-PCB                                        
014100                           USEA-PCB                                       
014200                           HANA-PCB                                       
014300                           PLAA-PCB                                       
014400                           WDP3-PCB                                       
014500                           ARTC-PCB                                       
014600                           WDK7-PCB                                       
014700                           WDP3A-PCB                                      
014800                           WDP3B-PCB                                      
014900                           WDP3C-PCB.                                     
015000                                                                          
015100     PERFORM IMS-GET-MSG                                                  
015200     IF SEGMENT-FOUND                                                     
015300       PERFORM A-INIT                                                     
015400       PERFORM B-CHECK-KEYS                                               
015500       PERFORM F-CALL-BIZ-LOGIC-W6010810                                  
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O10801 + 4                      
015700       PERFORM IMS-INSERT-MSG                                             
015800     END-IF                                                               
015900                                                                          
016000     MOVE ZERO                   TO RETURN-CODE                           
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500                                                                          
016600     IF MSG-DOUBLE-TRANSACTIONS                                           
016700       MOVE MSG-INDATA-MINUS-2-TRANSACT                                   
016800                                 TO MID-W6I10801                          
016900       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
017000       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSACT                                   
017300                                 TO MID-W6I10801                          
017400       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
017500       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
017600     END-IF                                                               
017700                                                                          
017800     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
017900     MOVE MSG-IDPFK              TO MFS-IDPFK                             
018000     MOVE MFS-IDTRANS            TO W-IDTRANS                             
018100                                                                          
018200     MOVE LOW-VALUE              TO MSG-AREA                              
018300     MOVE 'W6O108N1'             TO MFS-IDMOD                             
018400     MOVE '6108'                 TO MOD-IDTRANS                           
018500     MOVE MFS-ERASE-FIELD        TO MOD-TEMFSFEL MOD-TEMFSINF             
018600                                                                          
018700     IF OWN-MID OR HELP-MID                                               
018800        CONTINUE                                                          
018900     ELSE                                                                 
019000       MOVE SPACE                TO MFS-KDTRTYP                           
019100       MOVE '7'                  TO MFS-IDPFK                             
019200     END-IF                                                               
019300                                                                          
019400     .                                                                    
019500     EJECT                                                                
019600*----------------------------------------------------------------*        
019700 B-CHECK-KEYS SECTION.                                                    
019800                                                                          
019900     MOVE MFS-RENSA-FAELT        TO MOD-IDARTNR-IN                        
020000                                    MOD-IDLEVNR-IN                        
020100                                    MOD-IDDC-IN                           
020200                                                                          
020300     MOVE ALL '+'                TO MSGI-WMSGINIT                         
020400     MOVE '001'                  TO MSGI-KDCALL                           
020500     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
020600     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
020700     MOVE '6108'                 TO MSGI-IDTRANS                          
020800                                                                          
020900     IF MFS-IDTRANS = '6108'                                              
021000       MOVE MID-IDLEVNR-IN       TO MSGI-IDLEVNR                          
021100       MOVE MID-IDARTNR-IN       TO MSGI-IDARTNR                          
021200       MOVE MID-IDDC-IN          TO MSGI-IDDC                             
021300     ELSE                                                                 
021400       IF MID-IDARTNR-IN NUMERIC AND                                      
021500          MID-IDARTNR-IN > ZERO                                           
021600         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
021700         MOVE SPACE              TO MSGI-IDLEVNR                          
021810       END-IF                                                             
021900       MOVE SPACE                TO MSGI-IDDC                             
022000     END-IF                                                               
022100                                                                          
022200     CALL W005INIT            USING MSGI-WMSGINIT USEA-PCB                
022300                                                                          
022400     MOVE MSGI-IDSPRAK           TO MCNV-IDSPRAK                          
022500                                                                          
022600     MOVE MSGI-IDARTNR           TO WS-IDARTNR                            
022700     INSPECT WS-IDARTNR   REPLACING LEADING SPACE BY ZERO                 
022800                                                                          
022900     IF MFS-IDTRANS = '6108'                                              
023000       IF MID-IDARTNR-IN = ALL '+'                                        
023100         CONTINUE                                                         
023200       ELSE                                                               
023300         MOVE '7'                TO MFS-IDPFK                             
023400         MOVE SPACE              TO MFS-KDTRTYP                           
023500       END-IF                                                             
023600     ELSE                                                                 
023700       IF WS-IDARTNR > ZERO                                               
023800         MOVE '7'                TO MFS-IDPFK                             
023900         MOVE SPACE              TO MFS-KDTRTYP                           
024000       END-IF                                                             
024100     END-IF                                                               
024200                                                                          
024300                                                                          
024400     MOVE MSGI-IDLEVNR           TO WS-IDLEVNR                            
024500                                                                          
024600     IF MFS-IDTRANS = '6108'                                              
024700       IF MID-IDLEVNR-IN = ALL '+'                                        
024800         CONTINUE                                                         
024900       ELSE                                                               
025000         MOVE '7'                TO MFS-IDPFK                             
025100         MOVE SPACE              TO MFS-KDTRTYP                           
025200       END-IF                                                             
025300     ELSE                                                                 
025400       IF WS-IDLEVNR NOT = SPACE                                          
025500         MOVE '7'                TO MFS-IDPFK                             
025600         MOVE SPACE              TO MFS-KDTRTYP                           
025700       END-IF                                                             
025800     END-IF                                                               
025900                                                                          
025910     IF MFS-IDTRANS = '6108'                                              
026000       IF MID-IDARTNR-IN   = ALL '+'                                      
026100         IF MID-IDLEVNR-IN = ALL '+'                                      
026200           CONTINUE                                                       
026300         ELSE                                                             
026400           MOVE ALL '+'            TO WS-IDARTNR                          
026500         END-IF                                                           
026600       ELSE                                                               
026700         IF MID-IDLEVNR-IN = ALL '+'                                      
026800           MOVE ALL '+'            TO WS-IDLEVNR                          
026900         END-IF                                                           
027000       END-IF                                                             
027010     END-IF                                                               
027100                                                                          
027200     MOVE WS-IDARTNR             TO REQU-IDARTNR-KEY                      
027300     MOVE WS-IDLEVNR             TO REQU-IDLEVNR-KEY                      
027400                                                                          
027500     IF MID-IDDC-IN = ALL '+'                                             
027600       MOVE MSGI-IDDC            TO WS-IDDC                               
027700       INSPECT WS-IDDC    REPLACING LEADING SPACE BY ZERO                 
027800     ELSE                                                                 
027900       IF MFS-IDTRANS = '6108'                                            
028000         MOVE MID-IDDC-IN        TO WS-IDDC                               
028100       ELSE                                                               
028200         MOVE MSGI-IDDC          TO WS-IDDC                               
028300       END-IF                                                             
028400       INSPECT WS-IDDC    REPLACING LEADING SPACE BY ZERO                 
028500       MOVE '7'                  TO MFS-IDPFK                             
028600       MOVE SPACE                TO MFS-KDTRTYP                           
028700     END-IF                                                               
028800     MOVE WS-IDDC                TO REQU-IDDC-KEY                         
028900                                                                          
029000     .                                                                    
029100     EJECT                                                                
029200*----------------------------------------------------------------*        
029300 F-CALL-BIZ-LOGIC-W6010810 SECTION.                                       
029400     CALL W6010810 USING REQU-AREA RESP-AREA                              
029500                         HANA-PCB  PLAA-PCB  WDP3-PCB  ARTC-PCB           
029600                         WDK7-PCB  WDP3A-PCB WDP3B-PCB WDP3C-PCB          
029700                                                                          
029800     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
029900        RESP-IDMSG-INFO  NOT = SPACE                                      
030000       PERFORM FA-SET-MSG-AND-HILIGHT                                     
030100     END-IF                                                               
030200     PERFORM FB-MOVE-RESP-TO-MOD                                          
030300     .                                                                    
030400     EJECT                                                                
030500 FA-SET-MSG-AND-HILIGHT SECTION.                                          
030600                                                                          
030700     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
030800     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
030900     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
031000                                                                          
031100     CALL WL01MCNV            USING MCNV-AREA                             
031200     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
031300     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
031400     .                                                                    
031500     EJECT                                                                
031600 FB-MOVE-RESP-TO-MOD SECTION.                                             
031700                                                                          
031800     IF RESP-IDARTNR           = SPACE                                    
031900       MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-UT                        
032000     ELSE                                                                 
032100       IF RESP-IDARTNR         = ALL '+'                                  
032200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR-UT                        
032300       ELSE                                                               
032400         MOVE RESP-IDARTNR       TO MOD-IDARTNR-UT                        
032500       END-IF                                                             
032600     END-IF                                                               
032700                                                                          
032800     IF RESP-IDLEVNR           = SPACE                                    
032900       MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-UT                        
033000     ELSE                                                                 
033100       IF RESP-IDLEVNR         = ALL '+'                                  
033200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR-UT                        
033300       ELSE                                                               
033400         MOVE RESP-IDLEVNR       TO MOD-IDLEVNR-UT                        
033500       END-IF                                                             
033600     END-IF                                                               
033700                                                                          
033800     IF RESP-IDDC              = SPACE                                    
033900       MOVE MFS-RENSA-FAELT      TO MOD-IDDC-UT                           
034000     ELSE                                                                 
034100       IF RESP-IDDC            = ALL '+'                                  
034200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDDC-UT                           
034300       ELSE                                                               
034400         MOVE RESP-IDDC          TO MOD-IDDC-UT                           
034500       END-IF                                                             
034600     END-IF                                                               
034700                                                                          
034800     IF RESP-ADINLOMR-FB       = SPACE                                    
034900       MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-FB                       
035000     ELSE                                                                 
035100       IF RESP-ADINLOMR-FB     = ALL '+'                                  
035200         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADINLOMR-FB                       
035300       ELSE                                                               
035400         MOVE RESP-ADINLOMR-FB   TO MOD-ADINLOMR-FB                       
035500       END-IF                                                             
035600     END-IF                                                               
035700                                                                          
035800     IF RESP-ADINLOMR-FP       = SPACE                                    
035900       MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-FP                       
036000     ELSE                                                                 
036100       IF RESP-ADINLOMR-FP     = ALL '+'                                  
036200         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADINLOMR-FP                       
036300       ELSE                                                               
036400         MOVE RESP-ADINLOMR-FP   TO MOD-ADINLOMR-FP                       
036500       END-IF                                                             
036600     END-IF                                                               
036700                                                                          
036800     IF RESP-ADLAGOMR          = SPACE                                    
036900       MOVE MFS-RENSA-FAELT      TO MOD-ADLAGOMR                          
037000     ELSE                                                                 
037100       IF RESP-ADLAGOMR        = ALL '+'                                  
037200         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADLAGOMR                          
037300       ELSE                                                               
037400         MOVE RESP-ADLAGOMR      TO MOD-ADLAGOMR                          
037500       END-IF                                                             
037600     END-IF                                                               
037700                                                                          
037800     IF RESP-IDNAMN-INLEV      = SPACE                                    
037900       MOVE MFS-RENSA-FAELT      TO MOD-IDNAMN-INLEV                      
038000     ELSE                                                                 
038100       IF RESP-IDNAMN-INLEV    = ALL '+'                                  
038200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDNAMN-INLEV                      
038300       ELSE                                                               
038400         MOVE RESP-IDNAMN-INLEV  TO MOD-IDNAMN-INLEV                      
038500       END-IF                                                             
038600     END-IF                                                               
038700                                                                          
038800     IF RESP-IDTFN-INLEV       = SPACE                                    
038900       MOVE MFS-RENSA-FAELT      TO MOD-IDTFN-INLEV                       
039000     ELSE                                                                 
039100       IF RESP-IDTFN-INLEV     = ALL '+'                                  
039200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTFN-INLEV                       
039300       ELSE                                                               
039400         MOVE RESP-IDTFN-INLEV   TO MOD-IDTFN-INLEV                       
039500       END-IF                                                             
039600     END-IF                                                               
039700                                                                          
039800     IF RESP-IDNAMN-LO         = SPACE                                    
039900       MOVE MFS-RENSA-FAELT      TO MOD-IDNAMN-LO                         
040000     ELSE                                                                 
040100       IF RESP-IDNAMN-LO       = ALL '+'                                  
040200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDNAMN-LO                         
040300       ELSE                                                               
040400         MOVE RESP-IDNAMN-LO     TO MOD-IDNAMN-LO                         
040500       END-IF                                                             
040600     END-IF                                                               
040700                                                                          
040800     IF RESP-IDTFN-LO          = SPACE                                    
040900       MOVE MFS-RENSA-FAELT      TO MOD-IDTFN-LO                          
041000     ELSE                                                                 
041100       IF RESP-IDTFN-LO        = ALL '+'                                  
041200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTFN-LO                          
041300       ELSE                                                               
041400         MOVE RESP-IDTFN-LO      TO MOD-IDTFN-LO                          
041500       END-IF                                                             
041600     END-IF                                                               
041700                                                                          
041800     IF RESP-IDNAMN-ANSK       = SPACE                                    
041900       MOVE MFS-RENSA-FAELT      TO MOD-IDNAMN-ANSK                       
042000     ELSE                                                                 
042100       IF RESP-IDNAMN-ANSK     = ALL '+'                                  
042200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDNAMN-ANSK                       
042300       ELSE                                                               
042400         MOVE RESP-IDNAMN-ANSK   TO MOD-IDNAMN-ANSK                       
042500       END-IF                                                             
042600     END-IF                                                               
042700                                                                          
042800     IF RESP-IDTFN-ANSK        = SPACE                                    
042900       MOVE MFS-RENSA-FAELT      TO MOD-IDTFN-ANSK                        
043000     ELSE                                                                 
043100       IF RESP-IDTFN-ANSK      = ALL '+'                                  
043200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTFN-ANSK                        
043300       ELSE                                                               
043400         MOVE RESP-IDTFN-ANSK    TO MOD-IDTFN-ANSK                        
043500       END-IF                                                             
043600     END-IF                                                               
043700                                                                          
043800     IF RESP-IDNAMN-BEREDARE   = SPACE                                    
043900       MOVE MFS-RENSA-FAELT      TO MOD-IDNAMN-BEREDARE                   
044000     ELSE                                                                 
044100       IF RESP-IDNAMN-BEREDARE = ALL '+'                                  
044200         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDNAMN-BEREDARE                   
044300       ELSE                                                               
044400         MOVE RESP-IDNAMN-BEREDARE                                        
044500                                 TO MOD-IDNAMN-BEREDARE                   
044600       END-IF                                                             
044700     END-IF                                                               
044800                                                                          
044900     IF RESP-IDTFN-BEREDARE    = SPACE                                    
045000       MOVE MFS-RENSA-FAELT      TO MOD-IDTFN-BEREDARE                    
045100     ELSE                                                                 
045200       IF RESP-IDTFN-BEREDARE  = ALL '+'                                  
045300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTFN-BEREDARE                    
045400       ELSE                                                               
045500         MOVE RESP-IDTFN-BEREDARE                                         
045600                                 TO MOD-IDTFN-BEREDARE                    
045700       END-IF                                                             
045800     END-IF                                                               
045900                                                                          
046000     IF RESP-IDNAMN-INK        = SPACE                                    
046100       MOVE MFS-RENSA-FAELT      TO MOD-IDNAMN-INK                        
046200     ELSE                                                                 
046300       IF RESP-IDNAMN-INK      = ALL '+'                                  
046400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDNAMN-INK                        
046500       ELSE                                                               
046600         MOVE RESP-IDNAMN-INK    TO MOD-IDNAMN-INK                        
046700       END-IF                                                             
046800     END-IF                                                               
046900                                                                          
047000     IF RESP-IDTFN-INK         = SPACE                                    
047100       MOVE MFS-RENSA-FAELT      TO MOD-IDTFN-INK                         
047200     ELSE                                                                 
047300       IF RESP-IDTFN-INK       = ALL '+'                                  
047400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTFN-INK                         
047500       ELSE                                                               
047600         MOVE RESP-IDTFN-INK     TO MOD-IDTFN-INK                         
047700       END-IF                                                             
047800     END-IF                                                               
047900                                                                          
048000     IF RESP-IDNAMN-FORP       = SPACE                                    
048100       MOVE MFS-RENSA-FAELT      TO MOD-IDNAMN-FORP                       
048200     ELSE                                                                 
048300       IF RESP-IDNAMN-FORP     = ALL '+'                                  
048400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDNAMN-FORP                       
048500       ELSE                                                               
048600         MOVE RESP-IDNAMN-FORP   TO MOD-IDNAMN-FORP                       
048700       END-IF                                                             
048800     END-IF                                                               
048900                                                                          
049000     IF RESP-IDTFN-FORP        = SPACE                                    
049100       MOVE MFS-RENSA-FAELT      TO MOD-IDTFN-FORP                        
049200     ELSE                                                                 
049300       IF RESP-IDTFN-FORP      = ALL '+'                                  
049400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTFN-FORP                        
049500       ELSE                                                               
049600         MOVE RESP-IDTFN-FORP    TO MOD-IDTFN-FORP                        
049700       END-IF                                                             
049800     END-IF                                                               
049900                                                                          
050000     IF RESP-IDNAMN-KVAL       = SPACE                                    
050100       MOVE MFS-RENSA-FAELT      TO MOD-IDNAMN-KVAL                       
050200     ELSE                                                                 
050300       IF RESP-IDNAMN-KVAL     = ALL '+'                                  
050400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDNAMN-KVAL                       
050500       ELSE                                                               
050600         MOVE RESP-IDNAMN-KVAL   TO MOD-IDNAMN-KVAL                       
050700       END-IF                                                             
050800     END-IF                                                               
050900                                                                          
051000     IF RESP-IDTFN-KVAL        = SPACE                                    
051100       MOVE MFS-RENSA-FAELT      TO MOD-IDTFN-KVAL                        
051200     ELSE                                                                 
051300       IF RESP-IDTFN-KVAL      = ALL '+'                                  
051400         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDTFN-KVAL                        
051500       ELSE                                                               
051600         MOVE RESP-IDTFN-KVAL    TO MOD-IDTFN-KVAL                        
051700       END-IF                                                             
051800     END-IF                                                               
051900     .                                                                    
052000     EJECT                                                                
052100*----------------------------------------------------------------*        
052200* --- IMS SECTIONS ---                                                    
052300*----------------------------------------------------------------*        
052400 IMS-GET-MSG SECTION.                                                     
052500     MOVE '  QC'                 TO GOOD-STATUSCODES                      
052600     CALL CBLTDLI             USING GU MSG-PCB MSG-IO-AREA                
052700     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
052800     PERFORM IMS-STATUSCHECK                                              
052900     .                                                                    
053000     SKIP2                                                                
053100*                                                                         
053200 IMS-INSERT-MSG SECTION.                                                  
053300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
053400       MOVE '0'                  TO MFS-KDHUVOMR                          
053500     END-IF                                                               
053600     MOVE LOW-VALUE              TO MSG-KDZ1 MSG-KDZ2                     
053700     MOVE SPACE                  TO GOOD-STATUSCODES                      
053800     CALL CBLTDLI             USING ISRT MSG-PCB                          
053900                                    MSG-IO-AREA MFS-IDMOD                 
054000     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
054100     PERFORM IMS-STATUSCHECK                                              
054200     .                                                                    
054300     SKIP2                                                                
054400*                                                                         
054500 IMS-STATUSCHECK SECTION.                                                 
054600     SET STATUS-IX               TO 1                                     
054700     SEARCH GOOD-STATUS                                                   
054800       AT END                                                             
054900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
055000         DELIMITED BY SIZE INTO ERROR-TEXT                                
055100         CALL FELLOG                                                      
055200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
055300         CONTINUE                                                         
055400     END-SEARCH                                                           
055500     .                                                                    

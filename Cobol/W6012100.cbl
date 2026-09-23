000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6012100.                                                
000400*AUTHOR.         ROS-MARIE CLASON - GUIDE DATAKONSULT AB.                 
000500*DATE-WRITTEN.   92/05/27.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ALLMÄN BESKRIVNING:                                              
001100*        PROGRAMMET ÄR EN MPP SOM ANVÄNDS VID LOSSNING AV EN              
001200*        BIL.                                                             
001300*        DEN INNEHÅLLER INFORMATION OM VAD SOM FINNS PÅ                   
001400*        BILEN SAMT VART DET SKALL.                                       
001500*        ALLTEFTERSOM LOSSAREN LASTAR AV PARTIER FRÅN BILEN               
001600*        REGISTRERAR PERSONEN DET PÅ BILDEN, SAMT OM EV. FEL              
001700*        UPPTÄCKS.                                                        
001800*        DET KAN EX.VIS VARA FEL EMBALLAGE PÅ GODSET,                     
001900*        ELLER SÅ SAKNAS KANSKE PARTIER.                                  
002000*                                                                         
002100*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
002200*                              W6LASA (W6G2)                              
002300*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSAKTION: W6T121                                              
002700*        MID:         W6I12101                                            
002800*                                                                         
002900*    UTDATA.                                                              
003000*        MOD:         W6O12101                                            
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500                                                                          
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W6012100'.            
004100 77  WS-IDRADNR                  PIC S9(5)   COMP-3.                      
004200                                                                          
004300*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004500                                                                          
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  YES                         PIC X       VALUE 'Y'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005100 77  IX                          PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  MAX-IX                      PIC S9(4)  VALUE +13   COMP SYNC.        
005300 77  TIX                         PIC S9(4)  VALUE +0    COMP SYNC.        
005400 77  MAX-TIX                     PIC S9(4)  VALUE +0    COMP SYNC.        
005500 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005600                                                                          
005700 77  MAX-KVRADER                 PIC S9(4)  COMP VALUE +13.               
005800 01  ALL-SPACE.                                                           
005900     03  FILLER                  PIC X(50)  VALUE SPACE.                  
006000 01  ALL-PLUS.                                                            
006100     03  FILLER                  PIC X(50)  VALUE ALL '+'.                
006200                                                                          
006300 01  ALLT-SW                     PIC X      VALUE 'Y'.                    
006400     88  ALLT-OK                            VALUE 'Y'.                    
006500                                                                          
006600*    --- RÄKNARE                                                          
006700 77  FELRAKN                     PIC S9(2)  VALUE +0    COMP SYNC.        
006800 77  SAK-RAKN                    PIC S9(2)  VALUE +0    COMP SYNC.        
006900 77  COUNTER                     PIC S9(3)  VALUE +0    COMP SYNC.        
007000                                                                          
007100*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = (1424) MOD-LÄNGD + 4             
007200 77  MAX-MOD-LAENGD              PIC S9(4) VALUE +1424 COMP SYNC.         
007300                                                                          
007400***********************************                                       
007500*        ARBETSFÄLT *                                                     
007600***********************************                                       
007700*    --- ARBETSFÄLT FÖR SWITCHAR                                          
007800                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  EGEN-MID                            VALUE '6121'.                
008100     88  GODK-MID                            VALUE '6121' '6122'          
008200                                                   '6123'.                
008300     88  HELP-MID                            VALUE '0551'.                
008400     EJECT                                                                
008500                                                                          
008600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008700 01  GENERELLA-SUBPROGRAM.                                                
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009100     03  W6012110                PIC X(8)    VALUE 'W6012110'.            
009200     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
009300     EJECT                                                                
009400                                                                          
009500*01 -COPY WMSGINIT                                                        
009600     SKIP3                                                                
009700                                                                          
009800*    ---  COPYTEXT FÖR TRANS TILL WL01MCNV                                
009900*01  -COPY WL01MCNV                                                       
010000     EJECT                                                                
010100                                                                          
010200 01  FILLER                      PIC X(16)  VALUE 'W6012110'.             
010300 01  REQU-AREA.                                                           
010400*    03 -COPY WZ01REQU                                                    
010500*    03 -COPY W60121I1                                                    
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
010800 01  RESP-AREA.                                                           
010900*    03 -COPY WZ01RESP                                                    
011000*    03 -COPY W60121O1                                                    
011100     EJECT                                                                
011200                                                                          
011300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011400*                                                                         
011500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011600     SKIP3                                                                
011700*01  MID -COPY W6I12101                                                   
011800     EJECT                                                                
011900                                                                          
012000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012100     SKIP3                                                                
012200*01  -COPY WMSGAREA                                                       
012300     EJECT                                                                
012400                                                                          
012500     03  MOD REDEFINES MSG-AREA.                                          
012600*      05  -COPY W6O12101                                                 
012700     EJECT                                                                
012800                                                                          
012900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013000     SKIP3                                                                
013100*01  -COPY WMFSAREA                                                       
013200     EJECT                                                                
013300                                                                          
013400*    --- STATUS-KOD FRÅN IMS                                              
013500 01  STATUS-WS                   PIC XX.                                  
013600     88  SEGMENT-FINNS                       VALUE '  '.                  
013700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013900     SKIP2                                                                
014000                                                                          
014100 01  GODK-STATUSKODER.                                                    
014200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014300     SKIP3                                                                
014400                                                                          
014500 01  SSA1                        PIC X(90).                               
014600 01  SSA2                        PIC X(64).                               
014700 01  SSA3                        PIC X(64).                               
014800     EJECT                                                                
014900                                                                          
015000*    --- IMS FUNKTIONSKODER                                               
015100*01  -COPY W0003                                                          
015200     EJECT                                                                
015300                                                                          
015400                                                                          
015500 LINKAGE SECTION.                                                         
015600                                                                          
015700*01  -COPY W0009   -PRE MSG-                                              
015800     EJECT                                                                
015900 01  ALT-PCB                     PIC X.                                   
016000 01  ALT1-PCB                    PIC X.                                   
016100 01  DISP-PCB                    PIC X.                                   
016200 01  USEA-PCB                    PIC X.                                   
016300 01  LISB-PCB                    PIC X.                                   
016400 01  INLA-PCB                    PIC X.                                   
016500 01  INLA-ALT-PCB                PIC X.                                   
016600 01  LASA-A-PCB                  PIC X.                                   
016700 01  LASA-B-PCB                  PIC X.                                   
016800 01  PLAA-PCB                    PIC X.                                   
016900 01  HANA-PCB                    PIC X.                                   
017000 01  STYR-PLAA-PCB               PIC X.                                   
017100 01  KOM-KOMA-PCB                PIC X.                                   
017200 01  LASA-W6012111-PCB           PIC X.                                   
017300 01  INLA-W6012111-PCB           PIC X.                                   
017400 01  LASA-W6012112-PCB           PIC X.                                   
017500 01  INLA-W6012112-PCB           PIC X.                                   
017600 01  WDK6-W6012112-PCB           PIC X.                                   
017700 01  WDK7-PCB                    PIC X.                                   
017800                                                                          
017900     EJECT                                                                
018000                                                                          
018100                                                                          
018200 PROCEDURE DIVISION  USING MSG-PCB                                        
018300                           ALT-PCB                                        
018400                           ALT1-PCB                                       
018500                           DISP-PCB                                       
018600                           USEA-PCB                                       
018700                           LISB-PCB                                       
018800                           INLA-PCB                                       
018900                           INLA-ALT-PCB                                   
019000                           LASA-A-PCB                                     
019100                           LASA-B-PCB                                     
019200                           PLAA-PCB                                       
019300                           HANA-PCB STYR-PLAA-PCB                         
019400                           KOM-KOMA-PCB                                   
019500                           LASA-W6012111-PCB                              
019600                           INLA-W6012111-PCB                              
019700                           LASA-W6012112-PCB                              
019800                           INLA-W6012112-PCB                              
019900                           WDK6-W6012112-PCB                              
020000                           WDK7-PCB.                                      
020100                                                                          
020200     ENTRY 'DLITCBL' USING MSG-PCB                                        
020300                           ALT-PCB                                        
020400                           ALT1-PCB                                       
020500                           DISP-PCB                                       
020600                           USEA-PCB                                       
020700                           LISB-PCB                                       
020800                           INLA-PCB                                       
020900                           INLA-ALT-PCB                                   
021000                           LASA-A-PCB                                     
021100                           LASA-B-PCB                                     
021200                           PLAA-PCB                                       
021300                           HANA-PCB STYR-PLAA-PCB                         
021400                           KOM-KOMA-PCB                                   
021500                           LASA-W6012111-PCB                              
021600                           INLA-W6012111-PCB                              
021700                           LASA-W6012112-PCB                              
021800                           INLA-W6012112-PCB                              
021900                           WDK6-W6012112-PCB                              
022000                           WDK7-PCB.                                      
022100                                                                          
022200     EJECT                                                                
022300                                                                          
022400*----------------------------------------------------------------*        
022500     PERFORM IMS-GET-MSG                                                  
022600     IF SEGMENT-FINNS                                                     
022700       PERFORM A-INIT                                                     
022800       PERFORM B-INIT-KEYS                                                
022900       PERFORM C-INIT-REQU                                                
023000       IF MFS-UPDATE                                                      
023100         SET REQU-UPDATE         TO TRUE                                  
023200         PERFORM E-SAMMA-SIDA                                             
023300       ELSE                                                               
023400         IF MFS-FIRST                                                     
023500           SET REQU-FIRST        TO TRUE                                  
023600         ELSE                                                             
023700           IF MFS-NEXT                                                    
023800             SET REQU-NEXT       TO TRUE                                  
023900             PERFORM D-NAESTA-SIDA                                        
024000           ELSE                                                           
024100             IF MFS-PRINT                                                 
024200               SET REQU-PRINT TO TRUE                                     
024300               PERFORM E-SAMMA-SIDA                                       
024400             ELSE                                                         
024500               SET REQU-QUERY    TO TRUE                                  
024600               PERFORM E-SAMMA-SIDA                                       
024700             END-IF                                                       
024800           END-IF                                                         
024900         END-IF                                                           
025000       END-IF                                                             
025100       IF ALLT-OK                                                         
025200         PERFORM F-CALL-BIZ-LOGIC-W6012110                                
025300       END-IF                                                             
025400       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O12101-CTX + 4                  
025500       PERFORM IMS-INSERT-MSG                                             
025600                                                                          
025700     END-IF                                                               
025800                                                                          
025900     MOVE ZERO                   TO RETURN-CODE                           
026000     GOBACK                                                               
026100     .                                                                    
026200     EJECT                                                                
026300*----------------------------------------------------------------*        
026400 A-INIT SECTION.                                                          
026500                                                                          
026600     IF MSG-DUBBLA-TRANSKODER                                             
026700       MOVE MSG-INDATA-MINUS-2-TRANSKODER                                 
026800                                 TO MID-W6I12101                          
026900       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
027000       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
027100     ELSE                                                                 
027200       MOVE MSG-INDATA-MINUS-1-TRANSKOD                                   
027300                                 TO MID-W6I12101                          
027400       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
027500       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
027600     END-IF                                                               
027700                                                                          
027800     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
027900     MOVE MSG-IDPFK              TO MFS-IDPFK                             
028000     MOVE MFS-IDTRANS            TO W-IDTRANS                             
028100                                                                          
028200     MOVE LOW-VALUE              TO MSG-AREA                              
028300     MOVE 'W6O121N1'             TO MFS-IDMOD                             
028400     MOVE '6121'                 TO MOD-IDTRANS                           
028500     MOVE MFS-RENSA-FAELT        TO MOD-TEMFSFEL MOD-TEMFSINF             
028600                                                                          
028700     IF EGEN-MID OR HELP-MID                                              
028800       CONTINUE                                                           
028900     ELSE                                                                 
029000       MOVE SPACE                TO MFS-KDTRTYP                           
029100       MOVE '7'                  TO MFS-IDPFK                             
029200     END-IF                                                               
029300                                                                          
029400     PERFORM AA-INIT-NYCKLAR                                              
029500                                                                          
029600     MOVE '101'                  TO REQU-IDMSGVER                         
029700     MOVE MSGI-IDSPRAK           TO MCNV-IDSPRAK                          
029800                                                                          
029900     .                                                                    
030000     EJECT                                                                
030100*----------------------------------------------------------------*        
030200 AA-INIT-NYCKLAR SECTION.                                                 
030300                                                                          
030400     MOVE ALL '+'                TO MSGI-WMSGINIT                         
030500     MOVE '001'                  TO MSGI-KDCALL                           
030600     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
030700     CALL W005INIT            USING MSGI-WMSGINIT USEA-PCB                
030800                                                                          
030900     .                                                                    
031000     EJECT                                                                
031100*----------------------------------------------------------------*        
031200 B-INIT-KEYS SECTION.                                                     
031300                                                                          
031400     PERFORM MFS-RENSA-FAELT-IN                                           
031500                                                                          
031600     IF MID-IDLBBET-IN  = ALL '+'                                         
031700       MOVE MID-IDLBBET-UT         TO REQU-IDLBBET-KEY                    
031800     ELSE                                                                 
031900       MOVE MID-IDLBBET-IN         TO REQU-IDLBBET-KEY                    
032000     END-IF                                                               
032100                                                                          
032200     IF MID-IDARTNR-IN  = ALL '+'                                         
032300       MOVE MID-IDARTNR-UT         TO REQU-IDARTNR-KEY                    
032400     ELSE                                                                 
032500       MOVE MID-IDARTNR-IN         TO REQU-IDARTNR-KEY                    
032600     END-IF                                                               
032700                                                                          
032800     IF MID-IDLEVNR-IN  = ALL '+'                                         
032900       MOVE MID-IDLEVNR-UT         TO REQU-IDLEVNR-KEY                    
033000     ELSE                                                                 
033100       MOVE MID-IDLEVNR-IN         TO REQU-IDLEVNR-KEY                    
033200     END-IF                                                               
033300                                                                          
033400     IF MID-IDFS-IN     = ALL '+'                                         
033500       MOVE MID-IDFS-UT            TO REQU-IDFS-KEY                       
033600     ELSE                                                                 
033700       MOVE MID-IDFS-IN            TO REQU-IDFS-KEY                       
033800     END-IF                                                               
033900                                                                          
034000     IF MID-IDLBBET-IN  = ALL '+' AND                                     
034100        MID-IDARTNR-IN  = ALL '+' AND                                     
034200        MID-IDLEVNR-IN  = ALL '+' AND                                     
034300        MID-IDFS-IN     = ALL '+'                                         
034400       CONTINUE                                                           
034500     ELSE                                                                 
034600        MOVE SPACE TO MFS-KDTRTYP                                         
034700        MOVE '7' TO MFS-IDPFK                                             
034800     END-IF                                                               
034900                                                                          
035000*    IF W-IDTRANS NOT = '6122'                                            
035100     IF W-IDTRANS NOT = '6121'                                            
035200*-------HÄR MÅSTE MAN RENSA VISSA MID-FÄLT                                
035300        MOVE SPACE         TO MID-ADINLOMR                                
035400                              MID-TELOSSN1                                
035500                              MID-TELOSSN2                                
035600                              MID-FLKLAR-TOT-IN                           
035700                              MID-FLKLAR-TOT-UT                           
035800                              MID-ENTER-IDFS                              
035900                              MID-NEXT-IDFS                               
036000                              MID-ENTER-IDLEVNR                           
036100                              MID-NEXT-IDLEVNR                            
036200        MOVE NEJ           TO MID-FLKLAR-BIL                              
036300        MOVE ZERO          TO MID-ENTER-IDARTNR                           
036400                              MID-NEXT-IDARTNR                            
036500        MOVE NEJ           TO MID-NEXT                                    
036600     END-IF                                                               
036700                                                                          
036800     IF MID-IDLOPNRM-IN = ALL '+'                                         
036900        MOVE MID-IDLOPNRM-UT  TO REQU-IDLOPNRM-KEY                        
037000     ELSE                                                                 
037100        MOVE MID-IDLOPNRM-IN  TO REQU-IDLOPNRM-KEY                        
037200     END-IF                                                               
037300                                                                          
037400     MOVE MFS-RENSA-FAELT     TO MOD-IDDC-IN                              
037500     IF MID-IDDC-IN NOT = ALL '+'                                         
037600        MOVE MID-IDDC-IN      TO MOD-IDDC-UT                              
037700                                 REQU-IDDC-KEY                            
037800        MOVE SPACE TO MFS-KDTRTYP                                         
037900        MOVE '7' TO MFS-IDPFK                                             
038000     ELSE                                                                 
038100        MOVE MSGI-IDDC     TO MOD-IDDC-UT                                 
038200                                 REQU-IDDC-KEY                            
038300     END-IF                                                               
038400                                                                          
038500     IF MID-FLKLAR-TOT-IN = ALL '+'                                       
038600       MOVE MID-FLKLAR-TOT-UT  TO REQU-FLKLAR-TOT-KEY                     
038700     ELSE                                                                 
038800       MOVE MID-FLKLAR-TOT-IN  TO REQU-FLKLAR-TOT-KEY                     
038900     END-IF                                                               
039000                                                                          
039100     IF GODK-MID OR HELP-MID                                              
039200        CONTINUE                                                          
039300     ELSE                                                                 
039400        MOVE MFS-RENSA-FAELT    TO MOD-IDLOPNRM-UT                        
039500                                   MOD-IDARTNR-UT                         
039600                                   MOD-IDLEVNR-UT                         
039700                                   MOD-IDFS-UT                            
039800                                   MOD-ADINLOMR-PRT                       
039900                                   MOD-TEMFSFEL                           
040000        PERFORM MFS-RENSA-FAELT-UT                                        
040100        MOVE NEJ               TO ALLT-SW                                 
040200     END-IF                                                               
040210                                                                          
040220     IF EGEN-MID                                                          
040221        CONTINUE                                                          
040222     ELSE                                                                 
040223        MOVE MFS-RENSA-FAELT    TO REQU-IDARTNR-KEY                       
040230     END-IF                                                               
040300     .                                                                    
040400     EJECT                                                                
040500*----------------------------------------------------------------*        
040600 C-INIT-REQU   SECTION.                                                   
040700                                                                          
040800     MOVE MAX-KVRADER            TO REQU-KVRADER                          
040900                                                                          
041000     IF MID-ADINLOMR-PRT = ALL '+'                                        
041100       MOVE ALL-PLUS          TO REQU-ADINLOMR-PRT                        
041200     ELSE                                                                 
041300       MOVE MID-ADINLOMR-PRT  TO REQU-ADINLOMR-PRT                        
041400     END-IF                                                               
041500                                                                          
041600     IF MID-ADINLOMR = ALL '+'                                            
041700       MOVE ALL-PLUS          TO REQU-ADINLOMR                            
041800     ELSE                                                                 
041900       MOVE MID-ADINLOMR      TO REQU-ADINLOMR                            
042000     END-IF                                                               
042100                                                                          
042200     IF MID-TELOSSN1 = ALL '+'                                            
042300       MOVE ALL-PLUS       TO REQU-TELOSSN1                               
042400     ELSE                                                                 
042500        MOVE MID-TELOSSN1  TO REQU-TELOSSN1                               
042600     END-IF                                                               
042700     IF MID-TELOSSN2 = ALL '+'                                            
042800       MOVE ALL-PLUS       TO REQU-TELOSSN2                               
042900     ELSE                                                                 
043000        MOVE MID-TELOSSN2  TO REQU-TELOSSN2                               
043100     END-IF                                                               
043200                                                                          
043300     MOVE MID-FLKLAR-BIL        TO REQU-FLKLAR-BIL                        
043400                                                                          
043500     PERFORM                                                              
043600     VARYING IX FROM +1 BY +1                                             
043700       UNTIL IX > REQU-KVRADER                                            
043800       MOVE MID-KDCMDVAL-INPUT (IX)                                       
043900                              TO REQU-KDCMDVAL-INPUT-LINE (IX)            
044000       MOVE MID-IDARTNR (IX)  TO REQU-IDARTNR-LINE (IX)                   
044100       MOVE MID-IDLEVNR (IX)  TO REQU-IDLEVNR-LINE (IX)                   
044200       MOVE MID-IDFS (IX)     TO REQU-IDFS-LINE (IX)                      
044300     END-PERFORM                                                          
044400                                                                          
044500     .                                                                    
044600     EJECT                                                                
044700*----------------------------------------------------------------*        
044800 D-NAESTA-SIDA SECTION.                                                   
044900                                                                          
045000     MOVE MID-NEXT-IDARTNR   TO REQU-IDARTNR-START                        
045100     MOVE MID-NEXT-IDLEVNR   TO REQU-IDLEVNR-START                        
045200     MOVE MID-NEXT-IDFS      TO REQU-IDFS-START                           
045300     MOVE MID-NEXT           TO REQU-START                                
045400     .                                                                    
045500     EJECT                                                                
045600*----------------------------------------------------------------*        
045700 E-SAMMA-SIDA SECTION.                                                    
045800                                                                          
045900     MOVE MID-ENTER-IDARTNR   TO REQU-IDARTNR-START                       
046000     MOVE MID-ENTER-IDLEVNR   TO REQU-IDLEVNR-START                       
046100     MOVE MID-ENTER-IDFS      TO REQU-IDFS-START                          
046200     .                                                                    
046300     EJECT                                                                
046400                                                                          
046500 F-CALL-BIZ-LOGIC-W6012110 SECTION.                                       
046600                                                                          
046700     CALL W6012110 USING REQU-AREA  RESP-AREA MAX-KVRADER MSG-PCB         
046800                         ALT-PCB    ALT1-PCB  DISP-PCB    USEA-PCB        
046900                         LISB-PCB   INLA-PCB  INLA-ALT-PCB                
047000                         LASA-A-PCB LASA-B-PCB PLAA-PCB   HANA-PCB        
047100                         STYR-PLAA-PCB        KOM-KOMA-PCB                
047200                         LASA-W6012111-PCB                                
047300                         INLA-W6012111-PCB                                
047400                         LASA-W6012112-PCB                                
047500                         INLA-W6012112-PCB                                
047600                         WDK6-W6012112-PCB                                
047700                         WDK7-PCB                                         
047800                                                                          
047900     IF RESP-IDMSG-ERROR NOT = SPACE OR                                   
048000        RESP-IDMSG-INFO  NOT = SPACE                                      
048100       PERFORM FA-SET-MSG-AND-HILIGHT                                     
048200     END-IF                                                               
048300     PERFORM FB-MOVE-RESP-TO-MOD                                          
048400     .                                                                    
048500     EJECT                                                                
048600 FA-SET-MSG-AND-HILIGHT SECTION.                                          
048700                                                                          
048800     MOVE RESP-IDMSG-ERROR       TO MCNV-IDMSG-ERROR                      
048900     MOVE RESP-IDMSG-INFO        TO MCNV-IDMSG-INFO                       
049000     MOVE RESP-IDELMT-ERROR      TO MCNV-IDELMT-ERROR                     
049100                                                                          
049200     CALL WL01MCNV USING MCNV-AREA                                        
049300     MOVE MCNV-MFSFEL            TO MOD-TEMFSFEL                          
049400     MOVE MCNV-MFSINF            TO MOD-TEMFSINF                          
049500     .                                                                    
049600     EJECT                                                                
049700 FB-MOVE-RESP-TO-MOD SECTION.                                             
049800                                                                          
049900     IF RESP-IDARTNR-KEY = SPACE                                          
050000       MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-UT                        
050100     ELSE                                                                 
050200       IF RESP-IDARTNR-KEY = ALL '+'                                      
050300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR-UT                        
050400       ELSE                                                               
050500         MOVE RESP-IDARTNR-KEY TO MOD-IDARTNR-UT                          
050600       END-IF                                                             
050700     END-IF                                                               
050800                                                                          
050900     IF RESP-IDLOPNRM-KEY = SPACE                                         
051000       MOVE MFS-RENSA-FAELT      TO MOD-IDLOPNRM-UT                       
051100     ELSE                                                                 
051200       IF RESP-IDLOPNRM-KEY = ALL '+'                                     
051300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLOPNRM-UT                       
051400       ELSE                                                               
051500         MOVE RESP-IDLOPNRM-KEY TO MOD-IDLOPNRM-UT                        
051600       END-IF                                                             
051700     END-IF                                                               
051800                                                                          
051900     IF RESP-IDLEVNR-KEY = SPACE                                          
052000       MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-UT                        
052100     ELSE                                                                 
052200       IF RESP-IDLEVNR-KEY = ALL '+'                                      
052300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR-UT                        
052400       ELSE                                                               
052500         MOVE RESP-IDLEVNR-KEY TO MOD-IDLEVNR-UT                          
052600       END-IF                                                             
052700     END-IF                                                               
052800                                                                          
052900     IF RESP-IDFS-KEY = SPACE                                             
053000       MOVE MFS-RENSA-FAELT      TO MOD-IDFS-UT                           
053100     ELSE                                                                 
053200       IF RESP-IDFS-KEY = ALL '+'                                         
053300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDFS-UT                           
053400       ELSE                                                               
053500         MOVE RESP-IDFS-KEY TO MOD-IDFS-UT                                
053600       END-IF                                                             
053700     END-IF                                                               
053800                                                                          
053900     IF RESP-IDLBBET-KEY = SPACE                                          
054000       MOVE MFS-RENSA-FAELT      TO MOD-IDLBBET-UT                        
054100     ELSE                                                                 
054200       IF RESP-IDLBBET-KEY = ALL '+'                                      
054300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLBBET-UT                        
054400       ELSE                                                               
054500         MOVE RESP-IDLBBET-KEY TO MOD-IDLBBET-UT                          
054600       END-IF                                                             
054700     END-IF                                                               
054800                                                                          
054900     IF RESP-IDDC-KEY = SPACE                                             
055000       MOVE MFS-RENSA-FAELT      TO MOD-IDDC-UT                           
055100     ELSE                                                                 
055200       IF RESP-IDDC-KEY = ALL '+'                                         
055300         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDDC-UT                           
055400       ELSE                                                               
055500         MOVE RESP-IDDC-KEY TO MOD-IDDC-UT                                
055600       END-IF                                                             
055700     END-IF                                                               
055800                                                                          
055900     MOVE RESP-ADINLOMR-PRT-ATTR TO MOD-ADINLOMR-PRT-ATTR                 
056000     IF RESP-ADINLOMR-PRT = SPACE                                         
056100       MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-PRT                      
056200     ELSE                                                                 
056300       IF RESP-ADINLOMR-PRT = ALL '+'                                     
056400         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADINLOMR-PRT                      
056500       ELSE                                                               
056600         MOVE RESP-ADINLOMR-PRT TO MOD-ADINLOMR-PRT                       
056700       END-IF                                                             
056800     END-IF                                                               
056900                                                                          
057000     IF RESP-FLKLAR-TOT-KEY = SPACE                                       
057100       MOVE MFS-RENSA-FAELT      TO MOD-FLKLAR-TOT-UT                     
057200     ELSE                                                                 
057300       IF RESP-FLKLAR-TOT-KEY = ALL '+'                                   
057400         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLKLAR-TOT-UT                     
057500       ELSE                                                               
057600         MOVE RESP-FLKLAR-TOT-KEY TO MOD-FLKLAR-TOT-UT                    
057700       END-IF                                                             
057800     END-IF                                                               
057900                                                                          
058000     IF RESP-IDARTNR-START = SPACE                                        
058100       MOVE MFS-RENSA-FAELT      TO MOD-ENTER-IDARTNR                     
058200     ELSE                                                                 
058300       IF RESP-IDARTNR-START = ALL '+'                                    
058400         MOVE MFS-ROER-EJ-FAELT  TO MOD-ENTER-IDARTNR                     
058500       ELSE                                                               
058600         MOVE RESP-IDARTNR-START TO MOD-ENTER-IDARTNR                     
058700       END-IF                                                             
058800     END-IF                                                               
058900                                                                          
059000     IF RESP-IDLEVNR-START = SPACE                                        
059100       MOVE MFS-RENSA-FAELT      TO MOD-ENTER-IDLEVNR                     
059200     ELSE                                                                 
059300       IF RESP-IDLEVNR-START = ALL '+'                                    
059400         MOVE MFS-ROER-EJ-FAELT  TO MOD-ENTER-IDLEVNR                     
059500       ELSE                                                               
059600         MOVE RESP-IDLEVNR-START TO MOD-ENTER-IDLEVNR                     
059700       END-IF                                                             
059800     END-IF                                                               
059900                                                                          
060000     IF RESP-IDFS-START = SPACE                                           
060100       MOVE MFS-RENSA-FAELT      TO MOD-ENTER-IDFS                        
060200     ELSE                                                                 
060300       IF RESP-IDFS-START = ALL '+'                                       
060400         MOVE MFS-ROER-EJ-FAELT  TO MOD-ENTER-IDFS                        
060500       ELSE                                                               
060600         MOVE RESP-IDFS-START TO MOD-ENTER-IDFS                           
060700       END-IF                                                             
060800     END-IF                                                               
060900                                                                          
061000     IF RESP-IDARTNR-NEXT = SPACE                                         
061100       MOVE MFS-RENSA-FAELT      TO MOD-NEXT-IDARTNR                      
061200     ELSE                                                                 
061300       IF RESP-IDARTNR-NEXT = ALL '+'                                     
061400         MOVE MFS-ROER-EJ-FAELT  TO MOD-NEXT-IDARTNR                      
061500       ELSE                                                               
061600         MOVE RESP-IDARTNR-NEXT TO MOD-NEXT-IDARTNR                       
061700       END-IF                                                             
061800     END-IF                                                               
061900                                                                          
062000     IF RESP-IDLEVNR-NEXT = SPACE                                         
062100       MOVE MFS-RENSA-FAELT      TO MOD-NEXT-IDLEVNR                      
062200     ELSE                                                                 
062300       IF RESP-IDLEVNR-NEXT = ALL '+'                                     
062400         MOVE MFS-ROER-EJ-FAELT  TO MOD-NEXT-IDLEVNR                      
062500       ELSE                                                               
062600         MOVE RESP-IDLEVNR-NEXT TO MOD-NEXT-IDLEVNR                       
062700       END-IF                                                             
062800     END-IF                                                               
062900                                                                          
063000     IF RESP-IDFS-NEXT = SPACE                                            
063100       MOVE MFS-RENSA-FAELT      TO MOD-NEXT-IDFS                         
063200     ELSE                                                                 
063300       IF RESP-IDFS-NEXT = ALL '+'                                        
063400         MOVE MFS-ROER-EJ-FAELT  TO MOD-NEXT-IDFS                         
063500       ELSE                                                               
063600         MOVE RESP-IDFS-NEXT TO MOD-NEXT-IDFS                             
063700       END-IF                                                             
063800     END-IF                                                               
063900                                                                          
064000     IF RESP-NEXT = SPACE                                                 
064100       MOVE MFS-RENSA-FAELT      TO MOD-NEXT                              
064200     ELSE                                                                 
064300       IF RESP-NEXT = ALL '+'                                             
064400         MOVE MFS-ROER-EJ-FAELT  TO MOD-NEXT                              
064500       ELSE                                                               
064600         MOVE RESP-NEXT TO MOD-NEXT                                       
064700       END-IF                                                             
064800     END-IF                                                               
064900                                                                          
065000     MOVE RESP-FLKLAR-BIL-ATTR TO MOD-FLKLAR-BIL-ATTR                     
065100     IF RESP-FLKLAR-BIL = SPACE                                           
065200       MOVE MFS-RENSA-FAELT      TO MOD-FLKLAR-BIL                        
065300     ELSE                                                                 
065400       IF RESP-FLKLAR-BIL = ALL '+'                                       
065500         MOVE MFS-ROER-EJ-FAELT  TO MOD-FLKLAR-BIL                        
065600       ELSE                                                               
065700         MOVE RESP-FLKLAR-BIL TO MOD-FLKLAR-BIL                           
065800       END-IF                                                             
065900     END-IF                                                               
066000                                                                          
066100     MOVE RESP-ADINLOMR-ATTR TO MOD-ADINLOMR-ATTR                         
066200     IF RESP-ADINLOMR = SPACE                                             
066300       MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR                          
066400     ELSE                                                                 
066500       IF RESP-ADINLOMR = ALL '+'                                         
066600         MOVE MFS-ROER-EJ-FAELT  TO MOD-ADINLOMR                          
066700       ELSE                                                               
066800         MOVE RESP-ADINLOMR TO MOD-ADINLOMR                               
066900       END-IF                                                             
067000     END-IF                                                               
067100                                                                          
067200     IF RESP-TELOSSN1 = SPACE                                             
067300       MOVE MFS-RENSA-FAELT      TO MOD-TELOSSN1                          
067400     ELSE                                                                 
067500       IF RESP-TELOSSN1 = ALL '+'                                         
067600         MOVE MFS-ROER-EJ-FAELT  TO MOD-TELOSSN1                          
067700       ELSE                                                               
067800         MOVE RESP-TELOSSN1 TO MOD-TELOSSN1                               
067900       END-IF                                                             
068000     END-IF                                                               
068100                                                                          
068200     IF RESP-TELOSSN2 = SPACE                                             
068300       MOVE MFS-RENSA-FAELT      TO MOD-TELOSSN2                          
068400     ELSE                                                                 
068500       IF RESP-TELOSSN2 = ALL '+'                                         
068600         MOVE MFS-ROER-EJ-FAELT  TO MOD-TELOSSN2                          
068700       ELSE                                                               
068800         MOVE RESP-TELOSSN2 TO MOD-TELOSSN2                               
068900       END-IF                                                             
069000     END-IF                                                               
069100                                                                          
069200     IF RESP-BEPRTLST = ALL '+'                                           
069300       CONTINUE                                                           
069400     ELSE                                                                 
069500       MOVE RESP-BEPRTLST        TO MOD-TEMFSFEL                          
069600     END-IF                                                               
069700                                                                          
069800     PERFORM                                                              
069900     VARYING IX FROM +1 BY +1                                             
070000       UNTIL IX > RESP-KVRADER                                            
070100                                                                          
070200       MOVE RESP-KDCMDVAL-INPUT-LINE-ATTR (IX)                            
070300                                TO MOD-KDCMDVAL-INPUT-ATTR (IX)           
070400       IF RESP-KDCMDVAL-INPUT-LINE (IX) = SPACE                           
070500         MOVE MFS-RENSA-FAELT      TO MOD-KDCMDVAL-INPUT (IX)             
070600       ELSE                                                               
070700         IF RESP-KDCMDVAL-INPUT-LINE (IX) = ALL '+'                       
070800           MOVE MFS-ROER-EJ-FAELT  TO MOD-KDCMDVAL-INPUT (IX)             
070900         ELSE                                                             
071000           MOVE RESP-KDCMDVAL-INPUT-LINE (IX)                             
071100                                   TO MOD-KDCMDVAL-INPUT (IX)             
071200         END-IF                                                           
071300       END-IF                                                             
071400                                                                          
071500       MOVE RESP-IDLEVNR-LINE-ATTR (IX)                                   
071600                                   TO MOD-IDLEVNR-ATTR (IX)               
071700       IF RESP-IDLEVNR-LINE (IX) = SPACE                                  
071800         MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR (IX)                    
071900       ELSE                                                               
072000         IF RESP-IDLEVNR-LINE (IX) = ALL '+'                              
072100           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDLEVNR (IX)                    
072200         ELSE                                                             
072300           MOVE RESP-IDLEVNR-LINE (IX)                                    
072400                                   TO MOD-IDLEVNR (IX)                    
072500         END-IF                                                           
072600       END-IF                                                             
072700                                                                          
072800       MOVE RESP-IDFS-LINE-ATTR (IX)                                      
072900                                   TO MOD-IDFS-ATTR (IX)                  
073000       IF RESP-IDFS-LINE (IX) = SPACE                                     
073100         MOVE MFS-RENSA-FAELT      TO MOD-IDFS (IX)                       
073200       ELSE                                                               
073300         IF RESP-IDFS-LINE (IX) = ALL '+'                                 
073400           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDFS (IX)                       
073500         ELSE                                                             
073600           MOVE RESP-IDFS-LINE (IX)                                       
073700                                   TO MOD-IDFS (IX)                       
073800         END-IF                                                           
073900       END-IF                                                             
074000                                                                          
074100       MOVE RESP-IDARTNR-LINE-ATTR (IX)                                   
074200                                   TO MOD-IDARTNR-ATTR (IX)               
074300       IF RESP-IDARTNR-LINE (IX) = SPACE                                  
074400         MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR (IX)                    
074500       ELSE                                                               
074600         IF RESP-IDARTNR-LINE (IX) = ALL '+'                              
074700           MOVE MFS-ROER-EJ-FAELT  TO MOD-IDARTNR (IX)                    
074800         ELSE                                                             
074900           MOVE RESP-IDARTNR-LINE (IX)                                    
075000                                   TO MOD-IDARTNR (IX)                    
075100         END-IF                                                           
075200       END-IF                                                             
075300                                                                          
075400       MOVE RESP-KVAVIS-TOT-LINE-ATTR (IX)                                
075500                                   TO MOD-KVAVIS-TOT-ATTR (IX)            
075600       IF RESP-KVAVIS-TOT-LINE (IX) = SPACE                               
075700         MOVE MFS-RENSA-FAELT      TO MOD-KVAVIS-TOT (IX)                 
075800       ELSE                                                               
075900         IF RESP-KVAVIS-TOT-LINE (IX) = ALL '+'                           
076000           MOVE MFS-ROER-EJ-FAELT  TO MOD-KVAVIS-TOT (IX)                 
076100         ELSE                                                             
076200           MOVE RESP-KVAVIS-TOT-LINE (IX)                                 
076300                                   TO MOD-KVAVIS-TOT (IX)                 
076400         END-IF                                                           
076500       END-IF                                                             
076600                                                                          
076700       MOVE RESP-KVKOLLI-LINE-ATTR (IX)                                   
076800                                   TO MOD-KVKOLLI-ATTR (IX)               
076900       IF RESP-KVKOLLI-LINE (IX) = SPACE                                  
077000         MOVE MFS-RENSA-FAELT      TO MOD-KVKOLLI (IX)                    
077100       ELSE                                                               
077200         IF RESP-KVKOLLI-LINE (IX) = ALL '+'                              
077300           MOVE MFS-ROER-EJ-FAELT  TO MOD-KVKOLLI (IX)                    
077400         ELSE                                                             
077500           MOVE RESP-KVKOLLI-LINE (IX)                                    
077600                                   TO MOD-KVKOLLI (IX)                    
077700         END-IF                                                           
077800       END-IF                                                             
077900                                                                          
078000       MOVE RESP-KDLAGEMB-LINE-ATTR (IX)                                  
078100                                   TO MOD-KDLAGEMB-ATTR (IX)              
078200       IF RESP-KDLAGEMB-LINE (IX) = SPACE                                 
078300         MOVE MFS-RENSA-FAELT      TO MOD-KDLAGEMB (IX)                   
078400       ELSE                                                               
078500         IF RESP-KDLAGEMB-LINE (IX) = ALL '+'                             
078600           MOVE MFS-ROER-EJ-FAELT  TO MOD-KDLAGEMB (IX)                   
078700         ELSE                                                             
078800           MOVE RESP-KDLAGEMB-LINE (IX)                                   
078900                                   TO MOD-KDLAGEMB (IX)                   
079000         END-IF                                                           
079100       END-IF                                                             
079200                                                                          
079300       MOVE RESP-BEFT-LINE-ATTR (IX)                                      
079400                                   TO MOD-BEFT-ATTR (IX)                  
079500       IF RESP-BEFT-LINE (IX) = SPACE                                     
079600         MOVE MFS-RENSA-FAELT      TO MOD-BEFT (IX)                       
079700       ELSE                                                               
079800         IF RESP-BEFT-LINE (IX) = ALL '+'                                 
079900           MOVE MFS-ROER-EJ-FAELT  TO MOD-BEFT (IX)                       
080000         ELSE                                                             
080100           MOVE RESP-BEFT-LINE (IX)                                       
080200                                   TO MOD-BEFT (IX)                       
080300         END-IF                                                           
080400       END-IF                                                             
080500                                                                          
080600       MOVE RESP-ADINLOMR-NXT-LINE-ATTR (IX)                              
080700                                TO MOD-ADINLOMR-NXT-ATTR (IX)             
080800       IF RESP-ADINLOMR-NXT-LINE (IX) = SPACE                             
080900         MOVE MFS-RENSA-FAELT      TO MOD-ADINLOMR-NXT (IX)               
081000       ELSE                                                               
081100         IF RESP-ADINLOMR-NXT-LINE (IX) = ALL '+'                         
081200           MOVE MFS-ROER-EJ-FAELT  TO MOD-ADINLOMR-NXT (IX)               
081300         ELSE                                                             
081400           MOVE RESP-ADINLOMR-NXT-LINE (IX)                               
081500                                   TO MOD-ADINLOMR-NXT (IX)               
081600         END-IF                                                           
081700       END-IF                                                             
081800                                                                          
081900       MOVE RESP-KVAVIS-LINE-ATTR (IX)                                    
082000                                   TO MOD-KVAVIS-ATTR (IX)                
082100       IF RESP-KVAVIS-LINE (IX) = SPACE                                   
082200         MOVE MFS-RENSA-FAELT      TO MOD-KVAVIS (IX)                     
082300       ELSE                                                               
082400         IF RESP-KVAVIS-LINE (IX) = ALL '+'                               
082500           MOVE MFS-ROER-EJ-FAELT  TO MOD-KVAVIS (IX)                     
082600         ELSE                                                             
082700           MOVE RESP-KVAVIS-LINE (IX)                                     
082800                                   TO MOD-KVAVIS (IX)                     
082900         END-IF                                                           
083000       END-IF                                                             
083100                                                                          
083200       MOVE RESP-KVAVIS-PRIO-LINE-ATTR (IX)                               
083300                                TO MOD-KVAVIS-PRIO-ATTR (IX)              
083400       IF RESP-KVAVIS-PRIO-LINE (IX) = SPACE                              
083500         MOVE MFS-RENSA-FAELT      TO MOD-KVAVIS-PRIO (IX)                
083600       ELSE                                                               
083700         IF RESP-KVAVIS-PRIO-LINE (IX) = ALL '+'                          
083800           MOVE MFS-ROER-EJ-FAELT  TO MOD-KVAVIS-PRIO (IX)                
083900         ELSE                                                             
084000           MOVE RESP-KVAVIS-PRIO-LINE (IX)                                
084100                                   TO MOD-KVAVIS-PRIO (IX)                
084200         END-IF                                                           
084300       END-IF                                                             
084400                                                                          
084500       MOVE RESP-KVAVIS-KIT-LINE-ATTR (IX)                                
084600                                TO MOD-KVAVIS-KIT-ATTR (IX)               
084700       IF RESP-KVAVIS-KIT-LINE (IX) = SPACE                               
084800         MOVE MFS-RENSA-FAELT      TO MOD-KVAVIS-KIT (IX)                 
084900       ELSE                                                               
085000         IF RESP-KVAVIS-KIT-LINE (IX) = ALL '+'                           
085100           MOVE MFS-ROER-EJ-FAELT  TO MOD-KVAVIS-KIT (IX)                 
085200         ELSE                                                             
085300           MOVE RESP-KVAVIS-KIT-LINE (IX)                                 
085400                                   TO MOD-KVAVIS-KIT (IX)                 
085500         END-IF                                                           
085600       END-IF                                                             
085700                                                                          
085800       MOVE RESP-ADTRDEST-KIT-LINE-ATTR (IX)                              
085900                                TO MOD-ADTRDEST-KIT-ATTR (IX)             
086000       IF RESP-ADTRDEST-KIT-LINE (IX) = SPACE                             
086100         MOVE MFS-RENSA-FAELT      TO MOD-ADTRDEST-KIT (IX)               
086200       ELSE                                                               
086300         IF RESP-ADTRDEST-KIT-LINE (IX) = ALL '+'                         
086400           MOVE MFS-ROER-EJ-FAELT  TO MOD-ADTRDEST-KIT (IX)               
086500         ELSE                                                             
086600           MOVE RESP-ADTRDEST-KIT-LINE (IX)                               
086700                                   TO MOD-ADTRDEST-KIT (IX)               
086800         END-IF                                                           
086900       END-IF                                                             
087000                                                                          
087100     END-PERFORM                                                          
087200                                                                          
087300     PERFORM                                                              
087400     VARYING IX FROM IX BY +1                                             
087500       UNTIL IX > MAX-IX                                                  
087600       MOVE MFS-RENSA-FAELT      TO                                       
087700                                    MOD-KDCMDVAL-INPUT(IX)                
087800                                    MOD-IDLEVNR(IX)                       
087900                                    MOD-IDFS(IX)                          
088000                                    MOD-IDARTNR(IX)                       
088100                                    MOD-KVAVIS-TOT(IX)                    
088200                                    MOD-KVKOLLI(IX)                       
088300                                    MOD-KDLAGEMB(IX)                      
088400                                    MOD-BEFT(IX)                          
088500                                    MOD-ADINLOMR-NXT(IX)                  
088600                                    MOD-KVAVIS(IX)                        
088700                                    MOD-KVAVIS-PRIO(IX)                   
088800                                    MOD-KVAVIS-KIT(IX)                    
088900                                    MOD-ADTRDEST-KIT(IX)                  
089000     MOVE MFS-CLOSE-FIELD        TO MOD-KDCMDVAL-INPUT-ATTR(IX)           
089100     END-PERFORM                                                          
089200                                                                          
089300     .                                                                    
089400     EJECT                                                                
089500*----------------------------------------------------------------*        
089600******************************************************************        
089700*    MFS-REDIGERING AV BILDENS FÄLT                              *        
089800******************************************************************        
089900*----------------------------------------------------------------*        
090000 MFS-RENSA-FAELT-UT SECTION.                                              
090100                                                                          
090200*    --- ALLA UTDATA-FÄLT                                                 
090300*    --- INKL. BLÄDDRINGSNYCKLAR - SPAR-FÄLT                              
090400                                                                          
090500     MOVE MFS-RENSA-FAELT    TO MOD-TELOSSN1                              
090600                                MOD-TELOSSN2                              
090700                                MOD-ADINLOMR                              
090800                                MOD-ENTER-IDARTNR                         
090900                                MOD-ENTER-IDLEVNR                         
091000                                MOD-ENTER-IDFS                            
091100                                MOD-NEXT-IDARTNR                          
091200                                MOD-NEXT-IDLEVNR                          
091300                                MOD-NEXT-IDFS                             
091400                                MOD-NEXT                                  
091500                                                                          
091600*--- RENSA INDEXERADE RADER                                               
091700                                                                          
091800     MOVE +1 TO IX                                                        
091900     PERFORM UNTIL IX > MAX-IX                                            
092000        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
092100        ADD +1 TO IX                                                      
092200     END-PERFORM                                                          
092300     .                                                                    
092400     SKIP2                                                                
092500     EJECT                                                                
092600*----------------------------------------------------------------*        
092700 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
092800                                                                          
092900*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
093000                                                                          
093100     MOVE MFS-RENSA-FAELT    TO MOD-KDCMDVAL-INPUT(IX)                    
093200                                MOD-IDLEVNR(IX)                           
093300                                MOD-IDFS(IX)                              
093400                                MOD-IDARTNR(IX)                           
093500                                MOD-KVAVIS-TOT(IX)                        
093600                                MOD-KVKOLLI(IX)                           
093700                                MOD-KDLAGEMB(IX)                          
093800                                MOD-BEFT(IX)                              
093900                                MOD-ADINLOMR-NXT(IX)                      
094000                                MOD-KVAVIS(IX)                            
094100                                MOD-KVAVIS-PRIO(IX)                       
094200                                MOD-KVAVIS-KIT(IX)                        
094300                                MOD-ADTRDEST-KIT(IX)                      
094400     .                                                                    
094500     EJECT                                                                
094600     SKIP2                                                                
094700*----------------------------------------------------------------*        
094800 MFS-RENSA-FAELT-IN SECTION.                                              
094900                                                                          
095000*    --- ALLA INDATA-FÄLT                                                 
095100     MOVE MFS-RENSA-FAELT TO MOD-IDLBBET-IN                               
095200                             MOD-IDARTNR-IN                               
095300                             MOD-IDLEVNR-IN                               
095400                             MOD-IDFS-IN                                  
095500                             MOD-FLKLAR-TOT-IN                            
095600                             MOD-IDLOPNRM-IN                              
095700     .                                                                    
095800     EJECT                                                                
095900     SKIP2                                                                
096000*----------------------------------------------------------------*        
096100******************************************************************        
096200*  IMS-SECTIONER                                                 *        
096300******************************************************************        
096400*----------------------------------------------------------------*        
096500 IMS-GET-MSG SECTION.                                                     
096600                                                                          
096700     MOVE '  QC' TO GODK-STATUSKODER                                      
096800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
096900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
097000     PERFORM IMS-STATUSKONTROLL                                           
097100     .                                                                    
097200     SKIP3                                                                
097300*----------------------------------------------------------------*        
097400 IMS-INSERT-MSG SECTION.                                                  
097500                                                                          
097600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
097700       MOVE '0' TO MFS-KDHUVOMR                                           
097800     END-IF                                                               
097900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
098000     MOVE SPACE TO GODK-STATUSKODER                                       
098100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
098200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
098300     PERFORM IMS-STATUSKONTROLL                                           
098400     .                                                                    
098500     EJECT                                                                
098600     SKIP3                                                                
098700                                                                          
098800******************************************************************        
098900*    IMS-UPPDATERING VIA LASA-PCB                                *        
099000******************************************************************        
099100*----------------------------------------------------------------*        
099200*----------------------------------------------------------------*        
099300 IMS-STATUSKONTROLL SECTION.                                              
099400                                                                          
099500     SET STATUS-IX TO 1                                                   
099600     SEARCH GODK-STATUS                                                   
099700       AT END                                                             
099800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
099900         DELIMITED BY SIZE INTO FELTEXT                                   
100000         CALL FELLOG                                                      
100100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
100200         CONTINUE                                                         
100300     END-SEARCH                                                           
100400     .                                                                    
100500     EJECT                                                                
100600     SKIP3                                                                

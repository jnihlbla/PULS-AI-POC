000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1051800.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   DECEMBER 1989.                                           
000500 DATE-COMPILED.                                                           
000600*    FUNKTION.   TP-UPPDATERINGSPROGRAM. BESTÄLLER W151S7  I SOP          
000700*                MED STYRDATA I SOP-TESYMBV FÖR KÖRNINGEN.                
000800*                                                                         
000900*    INDATA.                                                              
001000*        TRANSAKTION: W1T518                                              
001100*        MID:         W1I51801                                            
001200*    UTDATA.                                                              
001300*        MOD:         W1O51801     OM INDATA FEL                          
001400*    SUBPROGRAM.                                                          
001500*        FELLOG                                                           
001600*    SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP3                                                                
001900 DATA DIVISION.                                                           
002000     EJECT                                                                
002100 WORKING-STORAGE SECTION.                                                 
002200*    -- CHECKED BY WY2000                                                 
002300 77   PROGRAM-NAMN           VALUE 'W1051800'                             
002400                                 PIC X(8).                                
002500 77  JA                          PIC X(1)    VALUE 'J'.                   
002600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
002700 77  INDATA-RAETT                PIC X(1)    VALUE 'J'.                   
002800 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
002900 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
003000 77  SPR-IX                      PIC S9      VALUE +1 COMP-3.             
003100 77  TAB-IX                      PIC S9(9)   VALUE ZERO COMP-3.           
003200 77  Y2K-IX                      PIC S9(9)  VALUE +0   COMP SYNC.         
003300                                                                          
003400 77  TRANS-TEST                  PIC X(4).                                
003500     88  EGEN-BILD   VALUE '1518'.                                        
003600     88  GODK-BILD   VALUE '1511' '1512' '1513' '1514'                    
003700                           '1515' '1519'.                                 
003800 01  WS-IDCATNR                  PIC X(5)    VALUE ZERO.                  
003900 01  FILLER REDEFINES WS-IDCATNR.                                         
004000     03 NUM-IDCATNR              PIC 9(5).                                
004100                                                                          
004200 01  WS-IDCATGRP-FRAN            PIC X(2)    VALUE ZERO.                  
004300 01  FILLER REDEFINES WS-IDCATGRP-FRAN.                                   
004400     03 NUM-IDCATGRP-FRAN        PIC 9(2).                                
004500 01  FILLER REDEFINES WS-IDCATGRP-FRAN.                                   
004600     03 FILLER                   PIC 9.                                   
004700     03 NUM-IDCATGRP-FRAN-2      PIC 9.                                   
004800                                                                          
004900 01  WS-IDCATGRP-TILL            PIC X(2)    VALUE ZERO.                  
005000 01  FILLER REDEFINES WS-IDCATGRP-TILL.                                   
005100     03 NUM-IDCATGRP-TILL        PIC 9(2).                                
005200                                                                          
005300 01  IDAG-KDCATPUB               PIC X(6)    VALUE SPACE.                 
005400 01  WS-KDCATPUB                 PIC X(6)    VALUE SPACE.                 
005500 01  WS-KDCATPUB-R-AVV           PIC X(3)    VALUE SPACE.                 
005600 01  WS-KDCATPUB-AAAAVV          PIC X(6)    VALUE SPACE.                 
005700                                                                          
005800 01  WS-GILTIGA-AAR.                                                      
005900   03 WS-TIAAAA                  PIC 9(4)    VALUE ZERO                   
006000                                 OCCURS 4.                                
006100                                                                          
006200 01  WS-IDCATAVS                 PIC X(4)    VALUE ZERO.                  
006300 01  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
006400 01  WS-IDCATRAD                 PIC X(4)    VALUE ZERO.                  
006500                                                                          
006600 01  W-DAGENS-VECKA              PIC X(4).                                
006700 01  W-INMATAD-VECKA-FROM        PIC X(4).                                
006800 01  W-INMATAD-VECKA-FROM-NUM    REDEFINES W-INMATAD-VECKA-FROM           
006900                                 PIC 9(4).                                
007000 01  W-INMATAD-VECKA-TOM         PIC X(4).                                
007100 01  W-INMATAD-VECKA-TOM-NUM     REDEFINES W-INMATAD-VECKA-TOM            
007200                                 PIC 9(4).                                
007300 01  W-AAR                       PIC 9(2).                                
007400     SKIP1                                                                
007500 01  SPRAAK-KOLL                 PIC X.                                   
007600     88  KOD-FINNS               VALUE 'J'.                               
007700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
007800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
007900                                                                          
008000     EJECT                                                                
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200     03 FELLOG                  PIC X(8)     VALUE 'FELLOG  '.            
008300     03 CBLTDLI                 PIC X(8)     VALUE 'CBLTDLI '.            
008400     03 WDATKONV                PIC X(8)     VALUE 'WDATKONV'.            
008500                                                                          
008600                                                                          
008700     EJECT                                                                
008800*01  -COPY WDATAREA                                                       
008900     SKIP2                                                                
009000*- - - - - - - - - - - -   - - - PARAMETRAR TILL SOP                      
009100 01  W-PROG-TO-PROG-SW.                                                   
009200*03  -COPY WMSGSOP                                                        
009300     SKIP2                                                                
009400*01  FILLER -COPY WWLAND03                                                
009500     EJECT                                                                
009600 01  NYCKLAR-TILL-DLI.                                                    
009700     03  W-IDCATNR-X.                                                     
009800         05  W-IDCATNR           PIC 9(5)    VALUE ZERO.                  
009900     03  W-TIAAAA-X.                                                      
010000         05  W-TIAAAA            PIC 9(4)    VALUE ZERO.                  
010100     EJECT                                                                
010200                                                                          
010300*               ******     DATA SOM SKICKAS TILL SOP                      
010400 01  PARM-TESYMBV.                                                        
010500     03  FILLER                  PIC X(4)                                 
010600                                 VALUE 'KAT('.                            
010700     03  PARM-IDCATNR            PIC 9(5).                                
010800     03  FILLER                  PIC X       VALUE ')'.                   
010900     03  FILLER                  PIC X(5)                                 
011000                                 VALUE 'EMBL('.                           
011100     03  PARM-BEEMBLEM           PIC X(5).                                
011200     03  FILLER                  PIC X       VALUE ')'.                   
011300     03  FILLER                  PIC X(5)                                 
011400                                 VALUE 'OMBR('.                           
011500     03  PARM-TIOMBRYT-SEN       PIC 9(7).                                
011600     03  FILLER                  PIC X       VALUE ')'.                   
011700     03  FILLER                  PIC X(8)                                 
011800                                 VALUE 'GRPFRAN('.                        
011900     03  PARM-IDCATGRP-FRAN      PIC X(2).                                
012000     03  FILLER                  PIC X       VALUE ')'.                   
012100     03  FILLER                  PIC X(8)                                 
012200                                 VALUE 'GRPTILL('.                        
012300     03  PARM-IDCATGRP-TILL      PIC X(2).                                
012400     03  FILLER                  PIC X       VALUE ')'.                   
012500     03  FILLER                  PIC X(6)                                 
012600                                 VALUE 'SKYLT('.                          
012700     03  PARM-IDSKYLT            PIC X(3).                                
012800     03  FILLER                  PIC X       VALUE ')'.                   
012900     03  FILLER                  PIC X(4)                                 
013000                                 VALUE 'PRT('.                            
013100     03  PARM-KDPRTVAL           PIC X.                                   
013200     03  FILLER                  PIC X       VALUE ')'.                   
013300     03  FILLER                  PIC X(8)                                 
013400                                 VALUE 'SKYLT-H('.                        
013500     03  PARM-IDSKYLT-H          PIC X(3).                                
013600     03  FILLER                  PIC X       VALUE ')'.                   
013700     03  FILLER                  PIC X(6)                                 
013800                                 VALUE 'LISTA('.                          
013900     03  PARM-IDLISTA            PIC X(3).                                
014000     03  FILLER                  PIC X       VALUE ')'.                   
014100     03  FILLER                  PIC X(4)                                 
014200                                 VALUE 'PUB('.                            
014300     03  PARM-KDCATPUB           PIC X(6).                                
014400     03  FILLER                  PIC X       VALUE ')'.                   
014500                                                                          
014600     EJECT                                                                
014700 01  FELMEDDELANDE.                                                       
014800*                                                                         
014900     03 FEL3.                                                             
015000        05 FILLER                PIC X(26)                                
015100            VALUE 'UPPLYSTA FÄLT FEL         '.                           
015200        05 FILLER                PIC X(26)                                
015300            VALUE 'HIGHLIGHTED FIELDS WRONG  '.                           
015400     03 FILLER REDEFINES FEL3.                                            
015500        05 FEL-3 OCCURS 2        PIC X(26).                               
015600*                                                                         
015700     03 FEL4.                                                             
015800        05 FILLER                PIC X(26)                                
015900            VALUE '  SPRÅKKOD SAKNAS         '.                           
016000        05 FILLER                PIC X(26)                                
016100            VALUE '  LANGUAGE CODE IS MISSING'.                           
016200     03 FILLER REDEFINES FEL4.                                            
016300        05 FEL-4 OCCURS 2        PIC X(26).                               
016400*                                                                         
016500     03 FEL5.                                                             
016600        05 FILLER                PIC X(26)                                
016700            VALUE '  KATALOG FINNS EJ        '.                           
016800        05 FILLER                PIC X(26)                                
016900            VALUE '  CATALOGUE DOES NOT EXIST'.                           
017000     03 FILLER REDEFINES FEL5.                                            
017100        05 FEL-5 OCCURS 2        PIC X(26).                               
017200*                                                                         
017300     03 FEL6.                                                             
017400        05 FILLER                PIC X(26)                                
017500            VALUE '  GRUPP FRÅN FELAKTIG     '.                           
017600        05 FILLER                PIC X(26)                                
017700            VALUE '  GROUP FROM IS WRONG     '.                           
017800     03 FILLER REDEFINES FEL6.                                            
017900        05 FEL-6 OCCURS 2        PIC X(26).                               
018000                                                                          
018100     03 FEL7.                                                             
018200        05 FILLER                PIC X(26)                                
018300            VALUE '  PRINTERKOD FEL          '.                           
018400        05 FILLER                PIC X(26)                                
018500            VALUE '  WRONG PRINTER CODE      '.                           
018600     03 FILLER REDEFINES FEL7.                                            
018700        05 FEL-7 OCCURS 2        PIC X(26).                               
018800                                                                          
018900     03 FEL8.                                                             
019000        05 FILLER                PIC X(26)                                
019100            VALUE '  PUBKOD FEL              '.                           
019200        05 FILLER                PIC X(26)                                
019300            VALUE '  WRONG PUB CODE          '.                           
019400     03 FILLER REDEFINES FEL8.                                            
019500        05 FEL-8 OCCURS 2        PIC X(26).                               
019600                                                                          
019700     03 FEL9.                                                             
019800        05 FILLER                PIC X(30)                                
019900            VALUE '  PUBKOD EJ AKTIVERAD PÅ 1533.'.                       
020000        05 FILLER                PIC X(30)                                
020100            VALUE '  PUB CODE NOT ACTIVATED      '.                       
020200     03 FILLER REDEFINES FEL9.                                            
020300        05 FEL-9 OCCURS 2        PIC X(30).                               
020400* ---------------------------------------------                           
020500     03 MED1.                                                             
020600        05 FILLER                PIC X(40)                                
020700            VALUE 'STARTA PRINT-JOBB MED PF11             '.              
020800        05 FILLER                PIC X(40)                                
020900            VALUE 'START PRINT-JOB BY PRESSING PF11       '.              
021000     03 FILLER REDEFINES MED1.                                            
021100        05 MED-1 OCCURS 2        PIC X(40).                               
021200     03 MED2.                                                             
021300        05 FILLER                PIC X(40)                                
021400            VALUE 'LISTAN KÖAD FÖR UTSKRIFT    '.                         
021500        05 FILLER                PIC X(40)                                
021600            VALUE 'LIST IS QUEUED TO PRINTER   '.                         
021700     03 FILLER REDEFINES MED2.                                            
021800        05 MED-2 OCCURS 2        PIC X(40).                               
021900*                                                                         
022000     EJECT                                                                
022100*                        ****    TP-AREOR                                 
022200 01  FILLER                      PIC X(16)   VALUE ' TP-AREOR '.          
022300     SKIP2                                                                
022400*01  MID -COPY W1I51801                                                   
022500     EJECT                                                                
022600*01  -COPY WMSGAREA                                                       
022700     EJECT                                                                
022800*    03  MOD -COPY W1O51801  -RED MSG-AREA.                               
022900     EJECT                                                                
023000*01  -COPY WMFSAREA.                                                      
023100     EJECT                                                                
023200******************************************************************        
023300*****                                                                     
023400*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023500*****                                                                     
023600 01  IMS-WS.                                                              
023700     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
023800     SKIP3                                                                
023900*****                    **** STATUS-KOD FRÅN IMS                         
024000     03  STATUS-WS               PIC X(2).                                
024100         88  SEGMENT-FINNS                   VALUE '  '.                  
024200         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
024300     SKIP3                                                                
024400     03  GODK-STATUSKODER.                                                
024500         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
024600     SKIP3                                                                
024700 01  SSA1                        PIC X(64).                               
024800 01  SSA2                        PIC X(64).                               
024900     EJECT                                                                
025000*                            IMS FUNKTIONSKODER                           
025100*01  -COPY W0003                                                          
025200     EJECT                                                                
025300*                            DLI INPUT-OUTPUT AREA                        
025400 01  DLI-IO-AREA.                                                         
025500     03  IO-AREA-1               PIC X(480)  VALUE SPACE.                 
025600     SKIP3                                                                
025700*    03  WLKATM01  -COPY WDN101  -RED IO-AREA-1.                          
025800                                                                          
025900     03  IO-AREA-2               PIC X(480)  VALUE SPACE.                 
026000     SKIP3                                                                
026100*    03  WLKATM11  -COPY WDN111  -RED IO-AREA-2.                          
026200                                                                          
026300     EJECT                                                                
026400 LINKAGE SECTION.                                                         
026500     SKIP2                                                                
026600*01   -COPY W0009    -PRE MSG-                                            
026700     EJECT                                                                
026800*01   -COPY W0009    -PRE ALT-                                            
026900     EJECT                                                                
027000*01   -COPY W0008    -PRE WLKATM-                                         
027100         05  FILLER              PIC X.                                   
027200     EJECT                                                                
027300 PROCEDURE DIVISION USING MSG-PCB ALT-PCB WLKATM-PCB.                     
027400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WLKATM-PCB.                    
027500                                                                          
027600     PERFORM IMS-GET-MSG                                                  
027700     IF SEGMENT-FINNS                                                     
027800        PERFORM A-INIT-SPARA-INPUT                                        
027900        IF MFS-UPDATE                                                     
028000           PERFORM C-KOLLA-INDATA                                         
028100           IF INDATA-RAETT = JA                                           
028200*             PERFORM E1-STARTA-JOB                                       
028300*             PERFORM IMS-INSERT-ALT-MSG                                  
028400              PERFORM E2-STARTA-RUTIN                                     
028500              PERFORM IMS-INSERT-ALT-MSG                                  
028600              MOVE MED-2(SPR-IX) TO MOD-TEMFSINF                          
028700           END-IF                                                         
028800        ELSE                                                              
028900           PERFORM C-KOLLA-INDATA                                         
029000           IF INDATA-RAETT = JA                                           
029100              MOVE MED-1(SPR-IX) TO MOD-TEMFSINF                          
029200           END-IF                                                         
029300        END-IF                                                            
029400        MOVE LENGTH OF MOD  TO MSG-KVLL                                   
029500        ADD +4              TO MSG-KVLL                                   
029600        PERFORM IMS-INSERT-MSG                                            
029700     END-IF                                                               
029800                                                                          
029900     MOVE ZERO TO RETURN-CODE                                             
030000     GOBACK                                                               
030100     .                                                                    
030200     EJECT                                                                
030300 A-INIT-SPARA-INPUT SECTION.                                              
030400     SKIP2                                                                
030500     IF MSG-DUBBLA-TRANSKODER                                             
030600         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I51801               
030700         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
030800         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
030900         MOVE MSG-IDPFK TO MFS-IDPFK                                      
031000         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
031100     ELSE                                                                 
031200         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I51801                
031300         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
031400         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
031500         MOVE SPACE TO MFS-KDTRTYP                                        
031600                       MFS-IDPFK                                          
031700     END-IF                                                               
031800                                                                          
031900     ACCEPT DAGENS-DATUM FROM DATE                                        
032000                                                                          
032100     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
032200     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
032300                                                                          
032400     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
032500                     DAT-O-TIDATUM DAT-KDSVAR                             
032600                                                                          
032700     IF DAT-KDSVAR-OK                                                     
032800       MOVE DAT-TISEKEL(1:2)   TO IDAG-KDCATPUB (1:2)                     
032900       MOVE DAT-TIAAVVD(1:4)   TO IDAG-KDCATPUB (3:4)                     
033000****             HÄMTA SEKELSIFFROR                                       
033100                                                                          
033200       MOVE DAT-TISEKEL    TO DAGENS-AAR(1:2)                             
033300                                                                          
033400     ELSE                                                                 
033500         STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS                
033600         DELIMITED BY SIZE INTO FELTEXT                                   
033700         CALL FELLOG                                                      
033800     END-IF                                                               
033900                                                                          
034000     MOVE DAGENS-DATUM(1:2)  TO DAGENS-AAR(3:2)                           
034100                                                                          
034200     COMPUTE WS-TIAAAA(1) = DAGENS-AAR - 1                                
034300     COMPUTE WS-TIAAAA(2) = DAGENS-AAR                                    
034400     COMPUTE WS-TIAAAA(3) = DAGENS-AAR + 1                                
034500     COMPUTE WS-TIAAAA(4) = DAGENS-AAR + 2                                
034600                                                                          
034700* - - - - - - - SPARA NYCKLAR OCH INDATA                                  
034800                                                                          
034900     MOVE MFS-IDTRANS TO TRANS-TEST                                       
035000     IF NOT EGEN-BILD                                                     
035100        MOVE SPACE TO MFS-KDTRTYP                                         
035200     END-IF                                                               
035300     IF EGEN-BILD                                                         
035400        IF MID-IDCATNR-IN = ALL '+'                                       
035500           MOVE MID-IDCATNR-UT TO WS-IDCATNR                              
035600           INSPECT WS-IDCATNR REPLACING ALL SPACE BY ZERO                 
035700                                        ALL '+'   BY ZERO                 
035800        ELSE                                                              
035900           MOVE MID-IDCATNR-IN TO WS-IDCATNR                              
036000           MOVE SPACE          TO MFS-KDTRTYP                             
036100        END-IF                                                            
036200                                                                          
036300        IF MID-IDCATGRP-FRAN-IN = ALL '+'                                 
036400           MOVE MID-IDCATGRP-FRAN-UT TO WS-IDCATGRP-FRAN                  
036500           INSPECT WS-IDCATGRP-FRAN                                       
036600                               REPLACING ALL SPACE BY ZERO                
036700                                         ALL '+'   BY ZERO                
036800        ELSE                                                              
036900           MOVE MID-IDCATGRP-FRAN-IN TO WS-IDCATGRP-FRAN                  
037000           MOVE SPACE                TO MFS-KDTRTYP                       
037100        END-IF                                                            
037200                                                                          
037300        IF MID-IDCATAVS-IN = ALL '+'                                      
037400           MOVE MID-IDCATAVS-UT TO WS-IDCATAVS                            
037500           INSPECT WS-IDCATAVS REPLACING ALL SPACE BY ZERO                
037600                                         ALL '+'   BY ZERO                
037700        ELSE                                                              
037800           MOVE MID-IDCATAVS-IN TO WS-IDCATAVS                            
037900           MOVE SPACE           TO MFS-KDTRTYP                            
038000        END-IF                                                            
038100                                                                          
038200        IF MID-KDCATPUB-R-IN = ALL '+'                                    
038300           MOVE MID-KDCATPUB-R-UT    TO WS-KDCATPUB-R-AVV                 
038400           PERFORM S50-Y2K-KDCATPUB-R                                     
038500           MOVE WS-KDCATPUB-AAAAVV   TO WS-KDCATPUB                       
038600           INSPECT WS-KDCATPUB REPLACING ALL SPACE BY ZERO                
038700                                         ALL '+'   BY ZERO                
038800        ELSE                                                              
038900           MOVE MID-KDCATPUB-R-IN   TO WS-KDCATPUB-R-AVV                  
039000           PERFORM S50-Y2K-KDCATPUB-R                                     
039100           MOVE WS-KDCATPUB-AAAAVV  TO WS-KDCATPUB                        
039200           MOVE SPACE               TO MFS-KDTRTYP                        
039300        END-IF                                                            
039400                                                                          
039500*                                                                         
039600        IF MID-IDSKYLT-IN = ALL '+'                                       
039700           MOVE MID-IDSKYLT-UT TO WS-IDSKYLT                              
039800           INSPECT WS-IDSKYLT REPLACING ALL '+'   BY SPACE                
039900        ELSE                                                              
040000           MOVE MID-IDSKYLT-IN TO WS-IDSKYLT                              
040100        END-IF                                                            
040200*                                                                         
040300        IF MID-IDCATRAD-IN = ALL '+'                                      
040400           MOVE MID-IDCATRAD-UT TO WS-IDCATRAD                            
040500           INSPECT WS-IDCATRAD REPLACING LEADING SPACE BY ZERO            
040600                                         ALL '+'   BY ZERO                
040700        ELSE                                                              
040800           MOVE MID-IDCATRAD-IN TO WS-IDCATRAD                            
040900           MOVE SPACE           TO MFS-KDTRTYP                            
041000        END-IF                                                            
041100     ELSE                                                                 
041200       IF GODK-BILD                                                       
041300*****       DETTA SKA TAS BORT NÄR NYCKLARNA                              
041400*****       HÄMTAS FRÅN NYCKELDATABASEN                                   
041500                                                                          
041600          IF MID-IDCATNR-IN = ALL '+'                                     
041700             MOVE MID-IDCATNR-UT TO WS-IDCATNR                            
041800             INSPECT WS-IDCATNR REPLACING ALL SPACE BY ZERO               
041900                                          ALL '+'   BY ZERO               
042000          ELSE                                                            
042100             MOVE MID-IDCATNR-IN TO WS-IDCATNR                            
042200             MOVE SPACE          TO MFS-KDTRTYP                           
042300          END-IF                                                          
042400                                                                          
042500          IF MID-IDCATGRP-FRAN-IN = ALL '+'                               
042600             MOVE MID-IDCATGRP-FRAN-UT TO WS-IDCATGRP-FRAN                
042700             INSPECT WS-IDCATGRP-FRAN                                     
042800                                 REPLACING ALL SPACE BY ZERO              
042900                                           ALL '+'   BY ZERO              
043000          ELSE                                                            
043100             MOVE MID-IDCATGRP-FRAN-IN TO WS-IDCATGRP-FRAN                
043200             MOVE SPACE                TO MFS-KDTRTYP                     
043300          END-IF                                                          
043400                                                                          
043500          IF MID-IDCATAVS-IN = ALL '+'                                    
043600             MOVE MID-IDCATAVS-UT TO WS-IDCATAVS                          
043700             INSPECT WS-IDCATAVS REPLACING ALL SPACE BY ZERO              
043800                                           ALL '+'   BY ZERO              
043900          ELSE                                                            
044000             MOVE MID-IDCATAVS-IN TO WS-IDCATAVS                          
044100             MOVE SPACE           TO MFS-KDTRTYP                          
044200          END-IF                                                          
044300                                                                          
044400          MOVE ZERO    TO WS-IDCATRAD                                     
044500                          WS-KDCATPUB                                     
044600          MOVE SPACE   TO WS-IDSKYLT                                      
044700          MOVE ALL '+' TO MID-IDCATGRP-TILL                               
044800                                                                          
044900       ELSE                                                               
045000          MOVE ZERO    TO WS-IDCATGRP-FRAN                                
045100                          WS-IDCATAVS WS-IDCATRAD                         
045200                          WS-KDCATPUB                                     
045300          MOVE SPACE   TO WS-IDSKYLT                                      
045400          MOVE ALL '+' TO MID-IDCATGRP-TILL                               
045500                                                                          
045600          IF TRANS-TEST = '1517'                                          
045700            IF MID-IDCATNR-IN = ALL '+'                                   
045800               MOVE MID-IDCATNR-UT TO WS-IDCATNR                          
045900               INSPECT WS-IDCATNR REPLACING LEADING SPACE BY ZERO         
046000            ELSE                                                          
046100               MOVE MID-IDCATNR-IN TO WS-IDCATNR                          
046200               MOVE SPACE          TO MFS-KDTRTYP                         
046300            END-IF                                                        
046400          ELSE                                                            
046500            MOVE ZERO TO WS-IDCATNR                                       
046600          END-IF                                                          
046700       END-IF                                                             
046800     END-IF                                                               
046900*                                                                         
047000     IF EGEN-BILD                                                         
047100       IF MID-IDCATGRP-TILL = ALL '+'                                     
047200          MOVE ZERO              TO WS-IDCATGRP-TILL                      
047300       ELSE                                                               
047400          MOVE MID-IDCATGRP-TILL TO WS-IDCATGRP-TILL                      
047500       END-IF                                                             
047600       INSPECT WS-IDCATGRP-TILL REPLACING ALL   SPACE BY ZEROES           
047700     ELSE                                                                 
047800       MOVE ALL '+' TO MID-IDCATGRP-TILL                                  
047900       MOVE SPACE   TO MID-KDPRTVAL                                       
048000       MOVE ZERO    TO WS-IDCATGRP-TILL                                   
048100       MOVE '002'   TO MID-IDLISTA                                        
048200     END-IF                                                               
048300*                                                                         
048400     MOVE LOW-VALUE TO MOD-W1O51801                                       
048500     MOVE 'W1O51801' TO MFS-IDMOD                                         
048600     MOVE '1518' TO MOD-IDTRANS                                           
048700                                                                          
048800     IF ENGLISH-TEXT                                                      
048900        MOVE +2 TO SPR-IX                                                 
049000        MOVE 'N' TO MFS-KDHUVOMR                                          
049100     ELSE                                                                 
049200        MOVE +1 TO SPR-IX                                                 
049300     END-IF                                                               
049400* - - - - - - - FLYTTA TILL MOD                                           
049500     MOVE MFS-RENSA-FAELT   TO MOD-TEMFSFEL                               
049600                               MOD-IDCATNR-IN                             
049700                               MOD-IDCATGRP-FRAN-IN                       
049800                               MOD-IDCATAVS-IN                            
049900                               MOD-KDCATPUB-R-IN                          
050000                               MOD-IDSKYLT-IN                             
050100                               MOD-IDCATRAD-IN                            
050200                               MOD-TEMFSINF                               
050300     MOVE WS-IDCATNR        TO MOD-IDCATNR-UT                             
050400     INSPECT MOD-IDCATNR-UT    REPLACING LEADING ZERO BY SPACES           
050500                                                                          
050600     MOVE WS-IDCATGRP-FRAN  TO MOD-IDCATGRP-FRAN-UT                       
050700     INSPECT MOD-IDCATGRP-FRAN-UT REPLACING LEADING ZERO BY SPACE         
050800                                                                          
050900     MOVE WS-IDCATAVS       TO MOD-IDCATAVS-UT                            
051000     INSPECT MOD-IDCATAVS-UT   REPLACING LEADING ZERO BY SPACES           
051100                                                                          
051200     MOVE WS-KDCATPUB (4:3) TO MOD-KDCATPUB-R-UT                          
051300*    INSPECT MOD-KDCATPUB-R-UT REPLACING LEADING ZERO BY SPACE            
051400                                                                          
051500     MOVE WS-IDCATRAD       TO MOD-IDCATRAD-UT                            
051600     INSPECT MOD-IDCATRAD-UT   REPLACING LEADING ZERO BY SPACES           
051700                                                                          
051800     IF   WS-IDCATGRP-TILL   NUMERIC                                      
051900     AND  WS-IDCATGRP-TILL > ZERO                                         
052000       MOVE WS-IDCATGRP-TILL TO MOD-IDCATGRP-TILL                         
052100     ELSE                                                                 
052200       MOVE WS-IDCATGRP-FRAN TO MOD-IDCATGRP-TILL                         
052300                                WS-IDCATGRP-TILL                          
052400     END-IF                                                               
052500                                                                          
052600     INSPECT MOD-IDCATGRP-TILL REPLACING LEADING ZERO BY SPACE            
052700                                                                          
052800     MOVE MID-IDLISTA       TO MOD-IDLISTA                                
052900     MOVE MID-KDPRTVAL      TO MOD-KDPRTVAL                               
053000                                                                          
053100     IF WS-IDSKYLT = SPACE                                                
053200        IF SPR-IX = +2                                                    
053300           MOVE 'GB ' TO WS-IDSKYLT                                       
053400        ELSE                                                              
053500           MOVE 'S  ' TO WS-IDSKYLT                                       
053600        END-IF                                                            
053700     END-IF                                                               
053800     MOVE WS-IDSKYLT        TO MOD-IDSKYLT-UT                             
053900     .                                                                    
054000     EJECT                                                                
054100 C-KOLLA-INDATA           SECTION.                                        
054200     SKIP2                                                                
054300     MOVE JA TO INDATA-RAETT                                              
054400                                                                          
054500     IF ENGLISH-TEXT                                                      
054600        MOVE 'GB ' TO PARM-IDSKYLT-H                                      
054700     ELSE                                                                 
054800        MOVE 'S  ' TO PARM-IDSKYLT-H                                      
054900     END-IF                                                               
055000     PERFORM CA-KOLLA-KATALOG-OCH-GRUPP                                   
055100     IF INDATA-RAETT = JA                                                 
055200        PERFORM I-KONTR-PUBKOD                                            
055300        MOVE WS-KDCATPUB     TO PARM-KDCATPUB                             
055400                                                                          
055500        PERFORM CB-KOLLA-PRINTERVAL-OCH-LISTA                             
055600        IF INDATA-RAETT = JA                                              
055700           PERFORM CC-KOLLA-SPRAAK                                        
055800           IF INDATA-RAETT = JA                                           
055900              PERFORM CD-KOLLA-LISTTYP-MOT-GRUPP-F                        
056000           END-IF                                                         
056100        END-IF                                                            
056200     END-IF                                                               
056300     .                                                                    
056400     EJECT                                                                
056500 CA-KOLLA-KATALOG-OCH-GRUPP SECTION.                                      
056600     SKIP1                                                                
056700     MOVE NUM-IDCATNR TO W-IDCATNR                                        
056800     PERFORM IMS-GU-WLKATM01                                              
056900     IF SEGMENT-FINNS                                                     
057000       IF WS-IDCATGRP-FRAN IS NOT NUMERIC                                 
057100         MOVE FEL-6(SPR-IX) TO MOD-TEMFSFEL                               
057200         MOVE NEJ TO INDATA-RAETT                                         
057300       ELSE                                                               
057400         IF WS-IDCATGRP-TILL IS NOT NUMERIC                               
057500            MOVE MFS-RENSA-FAELT   TO MOD-IDCATGRP-TILL                   
057600            MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-TILL-ATTR              
057700            MOVE FEL-3(SPR-IX)     TO MOD-TEMFSFEL                        
057800            MOVE NEJ TO INDATA-RAETT                                      
057900         ELSE                                                             
058000           IF NUM-IDCATGRP-FRAN > 19                                      
058100             MOVE NUM-IDCATNR       TO PARM-IDCATNR                       
058200             MOVE NUM-IDCATGRP-FRAN TO PARM-IDCATGRP-FRAN                 
058300             MOVE KAT-TIOMBRYT-SEN  TO PARM-TIOMBRYT-SEN                  
058400             MOVE KAT-BEEMBLEM      TO PARM-BEEMBLEM                      
058500                                                                          
058600             IF NUM-IDCATGRP-TILL > 19                                    
058700             AND NUM-IDCATGRP-TILL NOT < NUM-IDCATGRP-FRAN                
058800               MOVE NUM-IDCATGRP-TILL  TO PARM-IDCATGRP-TILL              
058900                                          MOD-IDCATGRP-TILL               
059000               MOVE MFS-FORMATETS-ATTR TO MOD-IDCATGRP-TILL-ATTR          
059100             ELSE                                                         
059200               IF NUM-IDCATGRP-TILL = ZERO                                
059300                 MOVE NUM-IDCATGRP-FRAN TO PARM-IDCATGRP-TILL             
059400                                           MOD-IDCATGRP-TILL              
059500                 MOVE MFS-FORMATETS-ATTR TO                               
059600                                           MOD-IDCATGRP-TILL-ATTR         
059700               ELSE                                                       
059800                 MOVE MFS-RENSA-FAELT   TO MOD-IDCATGRP-TILL              
059900                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDCATGRP-TILL-ATTR         
060000                 MOVE FEL-3(SPR-IX) TO MOD-TEMFSFEL                       
060100                 MOVE NEJ TO INDATA-RAETT                                 
060200               END-IF                                                     
060300             END-IF                                                       
060400           ELSE                                                           
060500             MOVE FEL-6(SPR-IX) TO MOD-TEMFSFEL                           
060600             MOVE NEJ TO INDATA-RAETT                                     
060700           END-IF                                                         
060800         END-IF                                                           
060900       END-IF                                                             
061000     ELSE                                                                 
061100       MOVE FEL-5(SPR-IX) TO MOD-TEMFSFEL                                 
061200       MOVE NEJ TO INDATA-RAETT                                           
061300     END-IF                                                               
061400     .                                                                    
061500     EJECT                                                                
061600 CB-KOLLA-PRINTERVAL-OCH-LISTA SECTION.                                   
061700     SKIP2                                                                
061800     IF MID-IDLISTA = '001' OR '002'                                      
061900       MOVE MFS-ALFA-FAELT-RAETT   TO MOD-IDLISTA-ATTR                    
062000       MOVE MID-IDLISTA            TO PARM-IDLISTA                        
062100     ELSE                                                                 
062200       IF MID-IDLISTA = '+++' OR SPACE                                    
062300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLISTA-ATTR                    
062400         MOVE '002'                TO MID-IDLISTA                         
062500                                      MOD-IDLISTA                         
062600                                      PARM-IDLISTA                        
062700       ELSE                                                               
062800         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLISTA-ATTR                    
062900         MOVE FEL-3(SPR-IX)        TO MOD-TEMFSFEL                        
063000         MOVE NEJ                  TO INDATA-RAETT                        
063100       END-IF                                                             
063200     END-IF                                                               
063300***                                                                       
063400     IF INDATA-RAETT = JA                                                 
063500       IF MID-KDPRTVAL = '+' OR SPACE                                     
063600*                             * SÅ LÄNGE DET BARA ÄR ETT VAL              
063700         MOVE 'A' TO MID-KDPRTVAL                                         
063800       END-IF                                                             
063900                                                                          
064000       IF MID-KDPRTVAL NOT = 'A' AND 'A' AND 'A'                          
064100*                             * ALLA GODKÄNDA PRINTERVAL                  
064200         MOVE        NEJ         TO INDATA-RAETT                          
064300         MOVE   FEL-7 (SPR-IX)   TO MOD-TEMFSFEL                          
064400       ELSE                                                               
064500         EVALUATE MID-KDPRTVAL                                            
064600           WHEN 'A'                                                       
064700             MOVE 'QSE10249'     TO MOD-IDLTERM                           
064800*            MOVE 'QSE02153'     TO MOD-IDLTERM                           
064900*            MOVE 'QSE02817'     TO MOD-IDLTERM                           
065000*            MOVE 'QSE05148'     TO MOD-IDLTERM                           
065100                                                                          
065200           WHEN OTHER                                                     
065300             CONTINUE                                                     
065400         END-EVALUATE                                                     
065500*        *******************************************************          
065600       END-IF                                                             
065700                                                                          
065800       IF INDATA-RAETT = JA                                               
065900         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDPRTVAL-ATTR                   
066000         MOVE MID-KDPRTVAL         TO PARM-KDPRTVAL                       
066100                                      MOD-KDPRTVAL                        
066200       ELSE                                                               
066300         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDPRTVAL-ATTR                   
066400       END-IF                                                             
066500     END-IF                                                               
066600     .                                                                    
066700     EJECT                                                                
066800 CC-KOLLA-SPRAAK SECTION.                                                 
066900     SKIP2                                                                
067000     IF WS-IDSKYLT = SPACE                                                
067100         MOVE FEL-4(SPR-IX) TO MOD-TEMFSFEL                               
067200         MOVE NEJ TO INDATA-RAETT                                         
067300     ELSE                                                                 
067400        SET WWLAND03-IX TO +1                                             
067500        SEARCH WWLAND03-IDSKYLT-RAD                                       
067600            AT END MOVE NEJ TO SPRAAK-KOLL                                
067700            WHEN WWLAND03-IDSKYLT(WWLAND03-IX) = WS-IDSKYLT               
067800               MOVE JA TO SPRAAK-KOLL                                     
067900        END-SEARCH                                                        
068000        IF KOD-FINNS                                                      
068100            MOVE WS-IDSKYLT TO PARM-IDSKYLT                               
068200                                MOD-IDSKYLT-UT                            
068300        ELSE                                                              
068400            MOVE FEL-4(SPR-IX) TO MOD-TEMFSFEL                            
068500            MOVE NEJ TO INDATA-RAETT                                      
068600        END-IF                                                            
068700     END-IF                                                               
068800     .                                                                    
068900     EJECT                                                                
069000 CD-KOLLA-LISTTYP-MOT-GRUPP-F  SECTION.                                   
069100     SKIP2                                                                
069200     IF NUM-IDCATGRP-FRAN-2 > 0                                           
069300     AND MID-IDLISTA = '001'                                              
069400        MOVE FEL-6(SPR-IX) TO MOD-TEMFSFEL                                
069500        MOVE NEJ TO INDATA-RAETT                                          
069600     END-IF                                                               
069700     .                                                                    
069800     EJECT                                                                
069900 E2-STARTA-RUTIN SECTION.                                                 
070000                                                                          
070100     MOVE '1518'           TO MSGSOP-IDTRANS                              
070200     MOVE MFS-KDMFSFOR     TO MSGSOP-KDMFSFOR                             
070300     MOVE 'W151S7  '       TO MSGSOP-IDPROCESS                            
070400     MOVE 'O'              TO MSGSOP-KDSOPFUNK                            
070500     MOVE PARM-TESYMBV     TO MSGSOP-TESYMBV                              
070600                                                                          
070700     .                                                                    
070800                                                                          
070900     EJECT                                                                
071000 F-RENSA-BILD SECTION.                                                    
071100                                                                          
071200     MOVE MFS-RENSA-FAELT TO MOD-IDCATNR-IN                               
071300                             MOD-IDCATNR-UT                               
071400                             MOD-IDCATGRP-FRAN-IN                         
071500                             MOD-IDCATGRP-FRAN-UT                         
071600                             MOD-IDCATAVS-IN                              
071700                             MOD-IDCATAVS-UT                              
071800                             MOD-KDCATPUB-R-IN                            
071900                             MOD-KDCATPUB-R-UT                            
072000                             MOD-IDSKYLT-IN                               
072100                             MOD-IDSKYLT-UT                               
072200                             MOD-IDCATRAD-IN                              
072300                             MOD-IDCATRAD-UT                              
072400                             MOD-IDCATGRP-TILL                            
072500                             MOD-IDLISTA                                  
072600                             MOD-KDPRTVAL                                 
072700                             MOD-IDLTERM                                  
072800     .                                                                    
072900                                                                          
073000     EJECT                                                                
073100 I-KONTR-PUBKOD SECTION.                                                  
073200     SKIP2                                                                
073300     IF WS-KDCATPUB = ZERO                                                
073400*******      ANVÄNDAREN HAR INTE FYLLT I NÅGOT VÄRDE                      
073500*******      HÄMTA PUBKOD FÖR NÄSTA GENERERING                            
073600                                                                          
073700       MOVE IDAG-KDCATPUB    TO WS-KDCATPUB                               
073800                                                                          
073900       MOVE WS-TIAAAA(2)   TO W-TIAAAA                                    
074000*           INNEVARANDE ÅR                                                
074100       PERFORM IMS-GNP-WLKATM11                                           
074200       IF SEGMENT-SAKNAS                                                  
074300         STRING ' WDN111 MÅSTE FINNAS MED DENNA NYCKEL ' STATUS-WS        
074400          DELIMITED BY SIZE INTO FELTEXT                                  
074500          CALL FELLOG                                                     
074600       END-IF                                                             
074700                                                                          
074800       MOVE 1 TO TAB-IX                                                   
074900       PERFORM UNTIL TAB-IX > 12                                          
075000               OR TAB-KDCATPUB-FOM (TAB-IX) > WS-KDCATPUB                 
075100*         --- D.V.S. FÖRSTA PUB SOM ÄR STÖRRE ÄN DAGENS-DATUM             
075200          ADD 1 TO TAB-IX                                                 
075300       END-PERFORM                                                        
075400                                                                          
075500       IF TAB-IX > 12                                                     
075600          ADD 1 TO W-TIAAAA                                               
075700          PERFORM IMS-GNP-WLKATM11                                        
075800          IF SEGMENT-SAKNAS                                               
075900              STRING ' WDN111 MÅSTE FINNAS MED DENNA NYCKEL '             
076000              STATUS-WS                                                   
076100              DELIMITED BY SIZE INTO FELTEXT                              
076200              CALL FELLOG                                                 
076300          END-IF                                                          
076400          MOVE 1  TO TAB-IX                                               
076500          PERFORM UNTIL TAB-IX > 12                                       
076600                 OR TAB-KDCATPUB-FOM (TAB-IX) > WS-KDCATPUB               
076700             ADD 1  TO TAB-IX                                             
076800          END-PERFORM                                                     
076900          IF TAB-IX > 12                                                  
077000             STRING ' FEL FRÅN DATUMRUTIN WDATKONV ' STATUS-WS            
077100             DELIMITED BY SIZE INTO FELTEXT                               
077200             CALL FELLOG                                                  
077300          ELSE                                                            
077400             MOVE TAB-KDCATPUB-FOM (TAB-IX)                               
077500                                TO WS-KDCATPUB                            
077600             MOVE TAB-KDCATPUB-FOM (TAB-IX) (4:3)                         
077700                                TO MOD-KDCATPUB-R-UT                      
077800          END-IF                                                          
077900       ELSE                                                               
078000          MOVE TAB-KDCATPUB-FOM (TAB-IX)                                  
078100                             TO WS-KDCATPUB                               
078200          MOVE TAB-KDCATPUB-FOM (TAB-IX) (4:3)                            
078300                             TO MOD-KDCATPUB-R-UT                         
078400       END-IF                                                             
078500     ELSE                                                                 
078600****** ANVÄNDAREN HAR FYLLT I ETT VÄRDE                                   
078700*      --- KOLLA FÖRST MOT FÖREGÅENDE ÅR (OM DET FINNS UPPLAGT)           
078800       MOVE WS-TIAAAA(1) TO W-TIAAAA                                      
078900                                                                          
079000       PERFORM IMS-GNP-WLKATM11                                           
079100       IF SEGMENT-FINNS                                                   
079200         MOVE 1 TO TAB-IX                                                 
079300         PERFORM UNTIL TAB-IX > 12                                        
079400                    OR TAB-KDCATPUB-FOM (TAB-IX) >= WS-KDCATPUB           
079500           ADD 1 TO TAB-IX                                                
079600         END-PERFORM                                                      
079700       ELSE                                                               
079800         MOVE 13 TO TAB-IX                                                
079900       END-IF                                                             
080000                                                                          
080100       IF TAB-IX > 12                                                     
080200*        --- INGEN TRÄFF I UNDER FÖREGÅENDE ÅR                            
080300*        --- KOLLA INNEVARANDE ÅR                                         
080400         MOVE WS-TIAAAA(2) TO W-TIAAAA                                    
080500                                                                          
080600         PERFORM IMS-GNP-WLKATM11                                         
080700         IF SEGMENT-SAKNAS                                                
080800              STRING ' WDN111 MÅSTE FINNAS MED DENNA NYCKEL '             
080900              STATUS-WS                                                   
081000              DELIMITED BY SIZE INTO FELTEXT                              
081100              CALL FELLOG                                                 
081200         END-IF                                                           
081300         MOVE 1 TO TAB-IX                                                 
081400         PERFORM UNTIL TAB-IX > 12                                        
081500                    OR TAB-KDCATPUB-FOM (TAB-IX) >= WS-KDCATPUB           
081600           ADD 1 TO TAB-IX                                                
081700         END-PERFORM                                                      
081800                                                                          
081900         IF TAB-IX > 12                                                   
082000*          --- INGEN TRÄFF I IÅR HELLER                                   
082100*          --- KOLLA NÄSTA ÅR, SISTA CHANSEN                              
082200           MOVE WS-TIAAAA(3) TO W-TIAAAA                                  
082300                                                                          
082400           PERFORM IMS-GNP-WLKATM11                                       
082500           IF SEGMENT-SAKNAS                                              
082600                STRING ' WDN111 MÅSTE FINNAS MED DENNA NYCKEL '           
082700                STATUS-WS                                                 
082800                DELIMITED BY SIZE INTO FELTEXT                            
082900                CALL FELLOG                                               
083000           END-IF                                                         
083100                                                                          
083200           MOVE 1 TO TAB-IX                                               
083300           PERFORM UNTIL TAB-IX > 12                                      
083400                      OR TAB-KDCATPUB-FOM (TAB-IX) >= WS-KDCATPUB         
083500             ADD 1 TO TAB-IX                                              
083600           END-PERFORM                                                    
083700                                                                          
083800           IF TAB-IX > 12                                                 
083900*            --- INGEN TRÄFF UNDER NÄSTA ÅR HELLER                        
084000             MOVE FEL-8 (SPR-IX) TO MOD-TEMFSFEL                          
084100             MOVE NEJ TO INDATA-RAETT                                     
084200           ELSE                                                           
084300*            --- TRÄFF PÅ KDCATPUB NÄSTA ÅR                               
084400             IF TAB-KDCATPUB-FOM (TAB-IX) > WS-KDCATPUB                   
084500               MOVE FEL-8 (SPR-IX) TO MOD-TEMFSFEL                        
084600               MOVE NEJ TO INDATA-RAETT                                   
084700             ELSE                                                         
084800               IF TAB-FLVADGEN(TAB-IX) = NEJ OR SPACE                     
084900*                --- MEN PUBKODEN VAR INTE AKTIVERAD                      
085000                 MOVE FEL-9 (SPR-IX) TO MOD-TEMFSFEL                      
085100                 MOVE NEJ TO INDATA-RAETT                                 
085200               END-IF                                                     
085300             END-IF                                                       
085400           END-IF                                                         
085500         ELSE                                                             
085600*          --- TRÄFF PÅ KDCATPUB IÅR                                      
085700           IF TAB-KDCATPUB-FOM (TAB-IX) > WS-KDCATPUB                     
085800             MOVE FEL-8 (SPR-IX) TO MOD-TEMFSFEL                          
085900             MOVE NEJ TO INDATA-RAETT                                     
086000           ELSE                                                           
086100             IF TAB-FLVADGEN(TAB-IX) = NEJ OR SPACE                       
086200*              --- MEN PUBKODEN VAR INTE AKTIVERAD                        
086300               MOVE FEL-9 (SPR-IX) TO MOD-TEMFSFEL                        
086400               MOVE NEJ TO INDATA-RAETT                                   
086500             END-IF                                                       
086600           END-IF                                                         
086700         END-IF                                                           
086800       ELSE                                                               
086900*        --- TRÄFF PÅ KDCATPUB IFJOL                                      
087000         IF TAB-KDCATPUB-FOM (TAB-IX) > WS-KDCATPUB                       
087100           MOVE FEL-8 (SPR-IX) TO MOD-TEMFSFEL                            
087200           MOVE NEJ TO INDATA-RAETT                                       
087300         ELSE                                                             
087400           IF TAB-FLVADGEN(TAB-IX) = NEJ OR SPACE                         
087500*            --- MEN PUBKODEN VAR INTE AKTIVERAD                          
087600             MOVE FEL-9 (SPR-IX) TO MOD-TEMFSFEL                          
087700             MOVE NEJ TO INDATA-RAETT                                     
087800           END-IF                                                         
087900         END-IF                                                           
088000       END-IF                                                             
088100     END-IF                                                               
088200     .                                                                    
088300     EJECT                                                                
088400*                                                                         
088500* SECTION S50-Y2K-KDCATPUB-R LIGGER I                                     
088600* COPYTEXT W.PROD.COBOL.W150Y2K1                                          
088700*                                                                         
088800*    -COPY W150Y2K1                                                       
088900     EJECT                                                                
089000* IMS SEKTIONER                                                           
089100     SKIP3                                                                
089200 IMS-GET-MSG SECTION.                                                     
089300                                                                          
089400     MOVE '  QC' TO GODK-STATUSKODER                                      
089500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
089600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089700     PERFORM IMS-STATUSKONTROLL                                           
089800     SKIP3                                                                
089900     .                                                                    
090000 IMS-INSERT-MSG SECTION.                                                  
090100                                                                          
090200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
090300     MOVE SPACE TO GODK-STATUSKODER                                       
090400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
090500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
090600     PERFORM IMS-STATUSKONTROLL                                           
090700     SKIP3                                                                
090800     .                                                                    
090900 IMS-INSERT-ALT-MSG SECTION.                                              
091000                                                                          
091100     MOVE SPACE TO GODK-STATUSKODER                                       
091200     CALL CBLTDLI USING PURG ALT-PCB W-PROG-TO-PROG-SW                    
091300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
091400     PERFORM IMS-STATUSKONTROLL                                           
091500     .                                                                    
091600     EJECT                                                                
091700 IMS-GU-WLKATM01 SECTION.                                                 
091800     SKIP1                                                                
091900     STRING 'WLKATM01(IDCATNR  =' W-IDCATNR-X ')'                         
092000            DELIMITED BY SIZE INTO SSA1                                   
092100     MOVE '  GE' TO GODK-STATUSKODER                                      
092200     CALL CBLTDLI USING GU WLKATM-PCB     IO-AREA-1 SSA1                  
092300     MOVE WLKATM-STATUS-CODE TO STATUS-WS                                 
092400     PERFORM IMS-STATUSKONTROLL                                           
092500     .                                                                    
092600     SKIP3                                                                
092700 IMS-GNP-WLKATM11 SECTION.                                                
092800                                                                          
092900     STRING 'WLKATM11(TIAAAA   =' W-TIAAAA-X ')'                          
093000          DELIMITED BY SIZE INTO SSA1                                     
093100     MOVE '  GE' TO GODK-STATUSKODER                                      
093200     CALL CBLTDLI USING GNP WLKATM-PCB     IO-AREA-2 SSA1                 
093300     MOVE WLKATM-STATUS-CODE TO STATUS-WS                                 
093400     PERFORM IMS-STATUSKONTROLL                                           
093500     .                                                                    
093600     SKIP3                                                                
093700 IMS-STATUSKONTROLL SECTION.                                              
093800     SET STATUS-IX TO 1                                                   
093900     SEARCH GODK-STATUS AT END CALL FELLOG                                
094000        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
094100           CONTINUE                                                       
094200     END-SEARCH                                                           
094300     .                                                                    

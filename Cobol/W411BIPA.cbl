000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411BIPA.                                                
000500 AUTHOR.         LARS THELL CAP GEMINI LOCIC.                             
000600 DATE-WRITTEN.   MAJ   -90.                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*        PROGRAMMET ÄR EN SUBMODUL TILL ETT MPP-PGM                       
001100*                                                                         
001200*    FUNKTION.                                                            
001300*      - KONTROLL MOT ORDERHUVUDUPPGIFTER OM                              
001400*        BIPACKNINGSKLARA RO/TPO-RADER SKA SKE.                           
001500*                                                                         
001600*      - LÄSNING AV NÄSTA BIPACKNINGSMÖJLIG                               
001700*        RO/TPO-RAD.                                                      
001800*                                                                         
001900*      - FINNS RAD ATT BIPACKA UPPDATERAS DEN PÅ REST-                    
002000*        ORDERREGISTRET MED STATUS = BIPACKAD                             
002100*                                                                         
002200*      - MODULEN HÄMTAR UPP 0 - 13 RADER FÖR BIPACK-                      
002300*        NING BEROENDE PÅ VAD DET STÅR I KVBIPACK                         
002400*                                                                         
002500*      - KONTROLLERAR MOT AKTUELLT C-LAGERS LAGEROMRÅDE                   
002600*        ATT ORDERDEL EJ REDAN BLIVIT UTSKRIVEN                           
002700*                                                                         
002800*        LÄNKAREA: W411BIPAC0                                             
002900*                                                                         
003000* E'TRACKER 2218613 DATED 2008-03-18 KAMPANJORDER SEPARAT PRC             
003100*                                                                         
003200*                                                                         
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(08)   VALUE 'W411BIPA'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  SPEC-FORBI                  PIC X       VALUE 'S'.                   
004500 77  IX                          PIC S9(9)   VALUE +0 COMP SYNC.          
004510 77  IX-DCCLEAR                  PIC S9(9)   VALUE +0 COMP SYNC.          
004600 77  IX-DCCLEAR-MAX              PIC S9(3)   VALUE +99  COMP SYNC.        
004700 77  IX3                         PIC S9(9)   VALUE +0 COMP SYNC.          
004800 77  ADLAGOMR-IX                 PIC S9(9)   VALUE +0 COMP SYNC.          
004900 77  INDX                        PIC S9(4)   COMP SYNC.                   
005000 77  IX-MAX                      PIC S9(4)   COMP SYNC VALUE +5.          
005100 77  IX-MIN                      PIC S9(4)   COMP SYNC VALUE +0.          
005200 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005300                                                                          
005400       EJECT                                                              
005500*   ----- SWITCHAR                                                        
005600 77  AVSLUTA-SW                  PIC X       VALUE 'N'.                   
005700     88 AVSLUTA                              VALUE 'J'.                   
005800                                                                          
005900 77  BIPACKNING-SW               PIC X       VALUE 'N'.                   
006000     88 BIPACKNING                           VALUE 'J'.                   
006100                                                                          
006200 01  DUMMY-PCB                   PIC X(4)    VALUE LOW-VALUE.             
006300*   ----- ARBETSFÄLT                                                      
006400                                                                          
006500 01  WS-PRARTNTO                 PIC S9(7)V9(2) COMP-3.                   
006600                                                                          
006700 01  W-IDKUNDRF.                                                          
006800   03 W-IDKUNDRF-1-5             PIC  X(05).                              
006900   03 FILLER                     PIC  X(05).                              
007000                                                                          
007100 01  W-IDBIP.                                                             
007200     03 FILLER                   PIC X(2).                                
007300     03 W-IDBIPREF               PIC X(5).                                
007400                                                                          
007500 01  TAB-RONR                    PIC X(10).                               
007600 01  FILLER REDEFINES TAB-RONR.                                           
007700     03  RONR OCCURS 5           PIC X.                                   
007800     03  FILLER                  PIC X(5).                                
007900                                                                          
008000 01  TAB-REF.                                                             
008100     03  FILLER                  PIC X(2).                                
008200     03  REF-TECKEN OCCURS 5     PIC X.                                   
008300                                                                          
008400 01  W-IDKUNDRF-WIP              PIC S9(7) COMP-3 VALUE ZERO.             
008500                                                                          
008600     EJECT                                                                
008700 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
008800*01 FILLER   -COPY WWDIST20    -RED  TEST-IDDISTR.                        
008900     EJECT                                                                
009000*01 FILLER   -COPY WWDIST79    -RED  TEST-IDDISTR.                        
009100     EJECT                                                                
009200*   ----- SUBPROGRAM                                                      
009300                                                                          
009400 01  GENERELLA-SUBPROGRAM.                                                
009500     03 CBLTDLI                  PIC X(8)     VALUE 'CBLTDLI '.           
009600     03 FELLOG                   PIC X(8)     VALUE 'FELLOG  '.           
009700     03 W411AREG                 PIC X(8)     VALUE 'W411AREG'.           
009800     03 W411SPAR                 PIC X(8)     VALUE 'W411SPAR'.           
009900     03 W411DLEV                 PIC X(8)     VALUE 'W411DLEV'.           
010000     EJECT                                                                
010100*   ----- PARAMETRAR TILL SUBPROGRAM                                      
010200 01  FILLER                  PIC X(16) VALUE 'AREG-AREA'.                 
010300*   -COPY W411AREG                                                        
010400     EJECT                                                                
010500 01  FILLER                  PIC X(16) VALUE 'SPAR-AREA'.                 
010600*   -COPY W411SPAR                                                        
010700     EJECT                                                                
010800 01  FILLER                  PIC X(16) VALUE 'DLEV-AREA'.                 
010900*   -COPY W411DLEV                                                        
011000     EJECT                                                                
011100*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
011200 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
011300     SKIP2                                                                
011400*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
011500                                                                          
011600 01  NYCKLAR-TILL-DLI.                                                    
011700                                                                          
011800   03  W-WDA5BSEQ-X.                                                      
011900     05  WDA5B-IDDISTR    PIC S9(5)    COMP-3.                            
012000     05  WDA5B-IDKUNDNR   PIC S9(7)    COMP-3.                            
012100     05  WDA5B-IDDC       PIC X(2).                                       
012200                                                                          
012300   03  W-WDA5BSEQ-MIN-X.                                                  
012400     05  WDA5B-IDDISTR-MIN  PIC S9(5)    COMP-3.                          
012500     05  WDA5B-IDKUNDNR-MIN PIC S9(7)    COMP-3.                          
012600     05  WDA5B-IDDC-MIN     PIC X(2)     VALUE LOW-VALUE.                 
012700                                                                          
012800   03  W-WDA5BSEQ-MAX-X.                                                  
012900     05  WDA5B-IDDISTR-MAX  PIC S9(5)    COMP-3.                          
013000     05  WDA5B-IDKUNDNR-MAX PIC S9(7)    COMP-3.                          
013100     05  WDA5B-IDDC-MAX     PIC X(2)     VALUE HIGH-VALUE.                
013200                                                                          
013300     03  W-IDDC-B6-X.                                                     
013400         05 W-IDDC-B6                  PIC X(2).                          
013500                                                                          
013600     SKIP2                                                                
013700*    ---- STATUSKOD FRÅN IMS                                              
013800                                                                          
013900 01  STATUS-WS               PIC XX.                                      
014000     88  SEGMENT-FINNS                    VALUE '  '.                     
014100     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
014200     88  SEGMENT-SLUT                     VALUE 'GB'.                     
014300     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
014400     SKIP2                                                                
014500 01  GODK-STATUSKODER.                                                    
014600   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
014700     SKIP2                                                                
014800     EJECT                                                                
014900 01  SSA1                    PIC X(64).                                   
015000     EJECT                                                                
015100*01  -COPY W0003                                                          
015200     EJECT                                                                
015300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
015400     SKIP2                                                                
015500 01  DLI-IO-AREA.                                                         
015600*    03  -COPY WDA501                                                     
015700                                                                          
015800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
015900 01   DLI-IO-AREA-B601.                                                   
016000*     03  -COPY WDB601                                                    
016100     EJECT                                                                
016200 LINKAGE SECTION.                                                         
016300*                                                                         
016400*   -COPY W411BIPA                                                        
016500     EJECT                                                                
016600*   -COPY W0008      -PRE ORDP-                                           
016700     05   FILLER                   PIC X(01).                             
016800*   -COPY W0008      -PRE WDB6-                                           
016900     05   FILLER                   PIC X(01).                             
017000     SKIP3                                                                
017100 01  WDK6-PCB                      PIC X(01).                             
017200 01  WDK7-PCB                      PIC X(01).                             
017300 01  LEVF-PCB                      PIC X(01).                             
017400 01  LEVG-PCB                      PIC X(01).                             
017500 01  WDF8-PCB                      PIC X(01).                             
017600 01  WDF8A-PCB                     PIC X(01).                             
017700 01  LEVA-PCB                      PIC X(01).                             
017800 01  ARTS2-PCB                     PIC X(01).                             
017900     EJECT                                                                
018000 PROCEDURE DIVISION  USING BIPA-W411BIPA ORDP-PCB WDB6-PCB                
018100                           WDK6-PCB WDK7-PCB                              
018200                           LEVF-PCB LEVG-PCB                              
018300                           WDF8-PCB WDF8A-PCB                             
018400                           LEVA-PCB ARTS2-PCB.                            
018500                                                                          
018600 STYR SECTION.                                                            
018700     PERFORM A-INIT                                                       
018800                                                                          
018900     IF BIPA-FLFORBI           = JA     OR                                
019000        BIPA-FLFORBI           = SPEC-FORBI OR                            
019100        BIPA-FLOVRLEV          = JA     OR                                
019200        BIPA-FLORDSPE          = JA     OR                                
019300        BIPA-KDORDKL           = ZERO   OR                                
019400        BIPA-IDKAMPRF          > ZERO                                     
019500         CONTINUE                                                         
019600      ELSE                                                                
019700                                                                          
019800         PERFORM B-KONTR-KDTPOTYP-KDROPACK                                
019900                                                                          
020000         IF NOT AVSLUTA                                                   
020100             PERFORM C-BIPACKNING-RESTORDER                               
020200         END-IF                                                           
020300     END-IF                                                               
020400                                                                          
020500     GOBACK                                                               
020600     .                                                                    
020700     EJECT                                                                
020800                                                                          
020900 A-INIT              SECTION.                                             
021000                                                                          
021100     IF BIPA-KVBIPACK          >  +13                                     
021200         MOVE +13              TO BIPA-KVBIPACK                           
021300     END-IF                                                               
021400                                                                          
021500     MOVE NEJ                  TO BIPACKNING-SW                           
021600                                  AVSLUTA-SW                              
021700     MOVE +1                   TO IX                                      
021800     PERFORM UNTIL IX > 13                                                
021900         MOVE +0               TO BIPA-IDKUNDRF-UT(IX)                    
022000                                  BIPA-IDARTNR(IX)                        
022100                                  BIPA-IDLOPNR(IX)                        
022200                                  BIPA-BERADREF(IX)                       
022300                                  BIPA-IDKONTO-UT(IX)                     
022400                                  BIPA-IDANALYS-UT(IX)                    
022500                                  BIPA-KDDSP(IX)                          
022600                                  BIPA-KDKVBRYT(IX)                       
022700                                  BIPA-KDORDING(IX)                       
022800                                  BIPA-KDORDKL-UT(IX)                     
022900                                  BIPA-KDPRODSL(IX)                       
023000                                  BIPA-KDVRINFO(IX)                       
023100                                  BIPA-KDTPOTYP-UT(IX)                    
023200                                  BIPA-KDFRAKT-UT(IX)                     
023300                                  BIPA-KVART(IX)                          
023400                                  BIPA-PRARTNTO(IX)                       
023500                                  BIPA-PRAVCOST(IX)                       
023600                                  BIPA-REKSIFFR(IX)                       
023700                                  BIPA-TIRODAT(IX)                        
023800                                  BIPA-FLINVEST(IX)                       
023900                                  BIPA-KVBEART-Q(IX)                      
024000                                  BIPA-TIREGDAT(IX)                       
024100                                  BIPA-TITPO(IX)                          
024200                                  BIPA-IDKAMPRF-UT(IX)                    
024300         INITIALIZE               BIPA-DEAL-PR-LINE(IX)                   
024400         MOVE SPACE            TO BIPA-FLERS(IX)                          
024500                                  BIPA-IDKST-UT(IX)                       
024600                                  BIPA-IDLEVNR(IX)                        
024700                                  BIPA-FLPRTILL(IX)                       
024800                                  BIPA-KDPRTYP(IX)                        
024900                                  BIPA-IDSYSTEM-UT(IX)                    
025000                                  BIPA-BEVOLREF(IX)                       
025100                                  BIPA-IDDC-UT(IX)                        
025200                                  BIPA-IDDC-RO(IX)                        
025300                                  BIPA-KDOI   (IX)                        
025400                                  BIPA-CLEARGROUP(IX)                     
025500                                                                          
025600*        LDCN GK START *                                                  
025700         MOVE SPACE            TO  BIPA-KDORDTYP-LDC(IX)                  
025800         MOVE ZERO             TO  BIPA-TIREPDAT(IX)                      
025900         MOVE SPACE            TO  BIPA-IDKUNDRF-WIP(IX)                  
026000*        LDCN GK END   *                                                  
026100                                                                          
026200         ADD +1                TO IX                                      
026300     END-PERFORM                                                          
026400     .                                                                    
026500     EJECT                                                                
026600 B-KONTR-KDTPOTYP-KDROPACK  SECTION.                                      
026700                                                                          
026800     IF BIPA-KDORDBEH = +7                                                
026900        IF BIPA-KDROPACK = SPACE                                          
027000           MOVE JA TO AVSLUTA-SW                                          
027100        END-IF                                                            
027200     ELSE                                                                 
027300        IF BIPA-KDROPACK = ZERO OR SPACE                                  
027400            MOVE JA               TO  AVSLUTA-SW                          
027500        END-IF                                                            
027600     END-IF                                                               
027700                                                                          
027800     IF BIPA-KDTPOTYP          > +0                                       
027900         MOVE JA               TO  AVSLUTA-SW                             
028000     END-IF                                                               
028100     .                                                                    
028200     EJECT                                                                
028300                                                                          
028400 C-BIPACKNING-RESTORDER    SECTION.                                       
028500                                                                          
028600     MOVE BIPA-IDDISTR         TO WDA5B-IDDISTR                           
028700                                  WDA5B-IDDISTR-MIN                       
028800                                  WDA5B-IDDISTR-MAX                       
028900                                  TEST-IDDISTR                            
029000     MOVE BIPA-IDKUNDNR        TO WDA5B-IDKUNDNR                          
029100                                  WDA5B-IDKUNDNR-MIN                      
029200                                  WDA5B-IDKUNDNR-MAX                      
029300     MOVE BIPA-IDDC            TO WDA5B-IDDC                              
029400                                  WS-IDDC                                 
029500     IF WS-IDDC NOT = SPACE                                               
029600       PERFORM S11-LAES-UNIK-WDA5                                         
029700     ELSE                                                                 
029800       PERFORM S12-LAES-ALL-WDA5                                          
029900     END-IF                                                               
030000                                                                          
030100     .                                                                    
030200     EJECT                                                                
030300 CA-SKAPA-UTAREA            SECTION.                                      
030400                                                                          
030500     IF DIST79-DEALER-PRICE                                               
030600       IF RAD-PRARTNTO-LOCPREL > ZERO                                     
030700         MOVE RAD-PRARTNTO-LOCPREL TO WS-PRARTNTO                         
030800       ELSE                                                               
030900         MOVE RAD-PRARTNTO-LOC     TO WS-PRARTNTO                         
031000       END-IF                                                             
031100     ELSE                                                                 
031300       IF DIST79-ECOM-PRICE                                               
031400*      *LOCAL CURRENCY NOT DNI,                                           
031500          IF RAD-PRARTNTO-LOC > 0                                         
031600             MOVE RAD-PRARTNTO-LOC TO WS-PRARTNTO                         
031700          END-IF                                                          
031800       ELSE                                                               
031900         IF RAD-PRAVCOST > 0                                              
032000            MOVE RAD-PRAVCOST      TO WS-PRARTNTO                         
032100         ELSE                                                             
032200            MOVE RAD-PRARTNTO      TO WS-PRARTNTO                         
032300         END-IF                                                           
032400       END-IF                                                             
032500     END-IF                                                               
032600                                                                          
032700     IF RAD-KDFAKTYP   = BIPA-KDFAKTYP                                    
032800*-FIX FÖR ATT EJ BIP. FELAKTIGA RADER MED NOLL I ANTAL EL .PRIS           
032900               AND RAD-KVART > ZERO                                       
033000               AND (WS-PRARTNTO > ZERO  OR                                
033100                     (DIST79-DEALER-PRICE AND WS-PRARTNTO = ZERO))        
033200*-FIX-SLUT                                                                
033300                                                                          
033400        PERFORM CAA-KONTR-BIPACKNINGSKOD                                  
033500        IF BIPACKNING                                                     
033600           PERFORM CAB-KONTR-SPARRAR                                      
033700           IF BIPACKNING                                                  
033800              PERFORM CAC-KONTR-TPO-UTSKR-ORDERDEL                        
033900              IF BIPACKNING                                               
034000                 PERFORM CAD-UPPD-WDA5                                    
034100                 ADD +1              TO IX                                
034200              END-IF                                                      
034300           END-IF                                                         
034400        END-IF                                                            
034500     END-IF                                                               
034600     .                                                                    
034700     EJECT                                                                
034800 CAA-KONTR-BIPACKNINGSKOD   SECTION.                                      
034900                                                                          
035000     IF (BIPA-KDROPACK NOT = 'L'                                          
035100     AND BIPA-KDROPACK NOT = 'N'                                          
035200      AND RAD-IDSYSTEM = 'OREL')                                          
035300      OR                                                                  
035400      (BIPA-KDORDBEH = 7 AND                                              
035500       RAD-IDKAMPRF > 0)                                                  
035600         MOVE NEJ                    TO BIPACKNING-SW                     
035700      ELSE                                                                
035800         EVALUATE TRUE                                                    
035900         WHEN BIPA-KDROPACK = '1'                                         
036000              PERFORM CAAA-KDROPACK-1                                     
036100                                                                          
036200         WHEN BIPA-KDROPACK = '2'                                         
036300              PERFORM CAAB-KDROPACK-2                                     
036400                                                                          
036500         WHEN BIPA-KDROPACK = '3'                                         
036600              PERFORM CAAC-KDROPACK-3                                     
036700                                                                          
036800         WHEN BIPA-KDROPACK = '4'                                         
036900              PERFORM CAAD-KDROPACK-4                                     
037000                                                                          
037100         WHEN BIPA-KDROPACK = '5'                                         
037200              PERFORM CAAE-KDROPACK-5                                     
037300                                                                          
037400         WHEN BIPA-KDROPACK = 'A'                                         
037500              PERFORM CAAI-KDROPACK-A                                     
037600                                                                          
037700         WHEN BIPA-KDROPACK = 'B'                                         
037800              PERFORM CAAJ-KDROPACK-B                                     
037900                                                                          
038000         WHEN BIPA-KDROPACK = 'C'                                         
038100              PERFORM CAAK-KDROPACK-C                                     
038200                                                                          
038300         WHEN BIPA-KDROPACK = 'D'                                         
038400              PERFORM CAAL-KDROPACK-D                                     
038500                                                                          
038600         WHEN BIPA-KDROPACK = 'G'                                         
038700              PERFORM CAAO-KDROPACK-G                                     
038800                                                                          
038900         WHEN BIPA-KDROPACK = 'L'                                         
039000              PERFORM CAAT-KDROPACK-L                                     
039100                                                                          
039200         WHEN BIPA-KDROPACK = '0'                                         
039300              PERFORM CAAX-KDROPACK-NOLL                                  
039400                                                                          
039500         WHEN BIPA-KDROPACK = 'P'                                         
039600              PERFORM CAAY-KDROPACK-P                                     
039700         END-EVALUATE                                                     
039800     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100 CAAA-KDROPACK-1      SECTION.                                            
040200                                                                          
040300     MOVE JA                   TO BIPACKNING-SW                           
040400     .                                                                    
040500     EJECT                                                                
040600 CAAB-KDROPACK-2      SECTION.                                            
040700                                                                          
040800     IF BIPA-KDORDKL           =  RAD-KDORDKL                             
040900         MOVE JA               TO BIPACKNING-SW                           
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 CAAC-KDROPACK-3      SECTION.                                            
041400     IF  RAD-IDARBREF(3:5)  = BIPA-IDKUNDRF(1:5)                          
041500      OR RAD-KDTPOTYP > ZERO                                              
041600         MOVE JA               TO BIPACKNING-SW                           
041700     END-IF                                                               
041800     .                                                                    
041900     EJECT                                                                
042000 CAAD-KDROPACK-4      SECTION.                                            
042100                                                                          
042200     IF RAD-KDFRAKT            =  BIPA-KDFRAKT                            
042300         MOVE JA               TO BIPACKNING-SW                           
042400     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700 CAAE-KDROPACK-5      SECTION.                                            
042800                                                                          
042900     MOVE RAD-IDKUNDRF         TO TAB-RONR                                
043000     MOVE BIPA-IDBIPREF        TO TAB-REF                                 
043100     MOVE IX-MAX               TO INDX                                    
043200     MOVE JA                   TO BIPACKNING-SW                           
043300     PERFORM UNTIL INDX = IX-MIN                                          
043400       IF REF-TECKEN(INDX)       NUMERIC                                  
043500          IF REF-TECKEN(INDX) NOT = RONR(INDX)                            
043600             MOVE NEJ          TO BIPACKNING-SW                           
043700             MOVE IX-MIN       TO INDX                                    
043800          ELSE                                                            
043900             ADD -1            TO INDX                                    
044000          END-IF                                                          
044100       ELSE                                                               
044200          IF INDX = IX-MAX                                                
044300             MOVE NEJ          TO BIPACKNING-SW                           
044400             MOVE IX-MIN       TO INDX                                    
044500          ELSE                                                            
044600             MOVE IX-MIN       TO INDX                                    
044700          END-IF                                                          
044800       END-IF                                                             
044900     END-PERFORM                                                          
045000     .                                                                    
046000     EJECT                                                                
047000 CAAI-KDROPACK-A      SECTION.                                            
047100                                                                          
047200     IF RAD-KDORDKL            =  +3 OR +4                                
047300         MOVE JA               TO BIPACKNING-SW                           
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 CAAJ-KDROPACK-B      SECTION.                                            
047800                                                                          
047900     IF RAD-KDORDKL            =  +2 OR +3 OR +4                          
048000         MOVE JA               TO BIPACKNING-SW                           
048100     END-IF                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 CAAK-KDROPACK-C      SECTION.                                            
048500                                                                          
048600     IF RAD-KDORDKL            =  +1                                      
048700         MOVE JA               TO BIPACKNING-SW                           
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100 CAAL-KDROPACK-D      SECTION.                                            
049200                                                                          
049300     IF RAD-KDORDKL            =  +1 OR +2                                
049400         MOVE JA               TO BIPACKNING-SW                           
049500     END-IF                                                               
049600     .                                                                    
049700     EJECT                                                                
049800 CAAO-KDROPACK-G      SECTION.                                            
049900                                                                          
050000     IF RAD-KDORDKL            =  BIPA-KDORDKL AND                        
050100        RAD-KDFRAKT            =  BIPA-KDFRAKT                            
050200         MOVE JA               TO BIPACKNING-SW                           
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600 CAAT-KDROPACK-L      SECTION.                                            
050700                                                                          
050800     IF RAD-IDSYSTEM           =  'OREL'                                  
050900         MOVE RAD-IDKUNDRF     TO W-IDKUNDRF                              
051000         MOVE BIPA-IDBIPREF    TO W-IDBIP                                 
051100         IF W-IDBIPREF  = W-IDKUNDRF-1-5                                  
051200          OR W-IDBIPREF(5:1) = '*'                                        
051300             MOVE JA           TO BIPACKNING-SW                           
051400         END-IF                                                           
051500     END-IF                                                               
051600     .                                                                    
051700     EJECT                                                                
051800 CAAX-KDROPACK-NOLL   SECTION.                                            
051900                                                                          
052000     IF BIPA-KDORDBEH = +7                                                
052100        IF RAD-KDTPOTYP = +2                                              
052200           IF BIPA-IDKUNDRF = RAD-IDKUNDRF AND                            
052300              RAD-TIREGDAT = RAD-TITPO AND                                
052400              RAD-TIREGDAT = RAD-TIRES                                    
052500              MOVE JA          TO BIPACKNING-SW                           
052600           END-IF                                                         
052700        END-IF                                                            
052800     END-IF                                                               
052900     .                                                                    
053000                                                                          
053100 CAAY-KDROPACK-P      SECTION.                                            
053200                                                                          
053300     IF W-IDKUNDRF-WIP = ZERO                                             
053400       IF RAD-TIREPDAT  NOT = ZERO                                        
053500         MOVE RAD-TIREPDAT     TO W-IDKUNDRF-WIP                          
053600       END-IF                                                             
053700     END-IF                                                               
053800                                                                          
053900     IF RAD-TIREPDAT     NOT = ZERO                                       
054000       IF RAD-TIREPDAT      =  W-IDKUNDRF-WIP                             
054100         IF RAD-KDORDKL  =  BIPA-KDORDKL                                  
054200           MOVE JA               TO BIPACKNING-SW                         
054300         END-IF                                                           
054400       END-IF                                                             
054500     END-IF                                                               
054600     .                                                                    
054700     EJECT                                                                
054800                                                                          
054900 CAB-KONTR-SPARRAR    SECTION.                                            
055000                                                                          
055100     PERFORM CABA-FLYTTA-TILL-AREG-LANKAREA                               
055200     PERFORM S01-CALL-W411AREG                                            
055300     IF AREG-ADLAGOMR = +0                                                
055400        MOVE +1                      TO AREG-ADLAGOMR                     
055500     END-IF                                                               
055600     MOVE AREG-ADLAGOMR              TO ADLAGOMR-IX                       
055700                                                                          
055800     PERFORM CABB-FLYTTA-TILL-SPAR-LANKAREA                               
055900     PERFORM S02-CALL-W411SPAR                                            
056000     IF SPAR-KDORDBEK > ZERO                                              
056100         MOVE NEJ                    TO BIPACKNING-SW                     
056200     END-IF                                                               
056300                                                                          
056400     PERFORM CABC-KONTROLL-FOR-SYSTEM-W480                                
056500     .                                                                    
056600     EJECT                                                                
056700 CABA-FLYTTA-TILL-AREG-LANKAREA    SECTION.                               
056800     IF BIPA-KDORDBEH = 7                                                 
056900       MOVE RAD-IDDC TO AREG-IDDC                                         
057000     END-IF                                                               
057100                                                                          
057200     MOVE RAD-IDARTNR          TO AREG-IDARTNR                            
057300     .                                                                    
057400     EJECT                                                                
057500 CABB-FLYTTA-TILL-SPAR-LANKAREA    SECTION.                               
057600                                                                          
057700     MOVE +6                   TO SPAR-KDORDBEH                           
057800     MOVE RAD-IDDC             TO SPAR-IDDC                               
057900     MOVE RAD-IDSYSTEM         TO SPAR-IDSYSTEM                           
058000     MOVE BIPA-IDDISTR         TO SPAR-IDDISTR                            
058100                                  TEST-IDDISTR                            
058200     MOVE BIPA-IDKUNDNR        TO SPAR-IDKUNDNR                           
058300     IF DIST20-EMBALLAGE                                                  
058400        MOVE JA                TO SPAR-FLEMBORD                           
058500     ELSE                                                                 
058600        MOVE NEJ               TO SPAR-FLEMBORD                           
058700     END-IF                                                               
058800     MOVE BIPA-FLORDSPE        TO SPAR-FLORDSPE                           
058900     MOVE BIPA-FLOVRLEV        TO SPAR-FLOVRLEV                           
059000     MOVE RAD-IDARTNR          TO SPAR-IDARTNR                            
059100     MOVE '0000000   '         TO SPAR-IDKUNDRF-RO                        
059200     MOVE RAD-IDORDNR5         TO SPAR-IDKUNDRF-RO(3:5)                   
059300     MOVE JA                   TO SPAR-FLRESTN                            
059400     MOVE ZERO                 TO SPAR-TITPO                              
059500     MOVE NEJ                  TO SPAR-FLFORBI                            
059600     MOVE BIPA-KDORDKL         TO SPAR-KDORDKL                            
059700     MOVE AREG-KDPRODSL        TO SPAR-KDPRODSL                           
059800     MOVE RAD-KDFAKTYP         TO SPAR-KDFAKTYP                           
059900     MOVE AREG-KDUART          TO SPAR-KDUART                             
060000     MOVE AREG-KDERS-UTG       TO SPAR-KDERS-UTG                          
060100     MOVE AREG-KDERS           TO SPAR-KDERS                              
060200     MOVE AREG-KDSORT          TO SPAR-KDSORT                             
060300     MOVE AREG-FLLSRDEL        TO SPAR-FLLSRDEL                           
060400     MOVE AREG-TIFINLV         TO SPAR-TIFINLV                            
060500     MOVE AREG-FLIART          TO SPAR-FLIART                             
060600     MOVE AREG-FLAVRART        TO SPAR-FLAVRART                           
060700     MOVE AREG-FLMARKSP        TO SPAR-FLMARKSP                           
060800     MOVE AREG-KDLEVSP         TO SPAR-KDLEVSP                            
060900     MOVE AREG-PRARTSTD        TO SPAR-PRARTSTD                           
061000     MOVE AREG-FLRADREF        TO SPAR-FLRADREF                           
061100     MOVE RAD-BERADREF         TO SPAR-BERADREF                           
061200     MOVE RAD-BEKUNDRF         TO SPAR-BEKUNDRF                           
061300     MOVE RAD-KDPRTYP          TO SPAR-KDPRTYP                            
061400     MOVE RAD-KDTPOTYP         TO SPAR-KDTPOTYP                           
061500     MOVE RAD-DARODAT (3:6)    TO SPAR-TIRODAT                            
061600     MOVE NEJ                  TO SPAR-FLSDCLEV                           
061700     MOVE ZERO                 TO SPAR-TIREPDAT                           
061800     .                                                                    
061900     EJECT                                                                
062000 CABC-KONTROLL-FOR-SYSTEM-W480     SECTION.                               
062100                                                                          
062200*****************************************************************         
062300*                                                               *         
062400* VÄND DIG TILL ANSVARIG FÖR SYSTEM W480 FÖR ATT FÅ MER         *         
062500* INFORMATION OM DENNA SEKTION OCH VAD DEN ÄR TILL FÖR.         *         
062600*                                                               *         
062700*****************************************************************         
062800                                                                          
062900     IF BIPA-BEVARREF = 'W480      '     OR                               
063000        RAD-BERADREF  = 'W480      '                                      
063100                                                                          
063200       IF BIPA-BEVARREF NOT = RAD-BERADREF                                
063300         MOVE NEJ                    TO BIPACKNING-SW                     
063400       END-IF                                                             
063500     END-IF                                                               
063600     .                                                                    
063700     EJECT                                                                
063800 CAC-KONTR-TPO-UTSKR-ORDERDEL  SECTION.                                   
063900                                                                          
064000     IF RAD-KDTPOTYP           > +0                                       
064100         PERFORM CACA-KONTROLLERA-TPO-RAD                                 
064200     END-IF                                                               
064300                                                                          
064400     IF BIPA-KDORDBEH          = +7                                       
064500         IF RAD-KDTPOTYP      > +0                                        
064600             IF BIPACKNING                                                
064700                 PERFORM CACB-KONTR-UTSKRIVEN-ORDERDEL                    
064800             END-IF                                                       
064900          ELSE                                                            
065000             PERFORM CACB-KONTR-UTSKRIVEN-ORDERDEL                        
065100         END-IF                                                           
065200     END-IF                                                               
065300     .                                                                    
065400     EJECT                                                                
065500 CACA-KONTROLLERA-TPO-RAD  SECTION.                                       
065600                                                                          
065700     MOVE NEJ                  TO BIPACKNING-SW                           
065800     IF BIPA-KDORDBEH          = +2                                       
065900         IF  RAD-KDTPOTYP = +6 OR                                         
066000            (RAD-KDTPOTYP = +2 AND RAD-KDORDKL = +1 AND                   
066100             BIPA-KDORDKL = +1) OR                                        
066200             RAD-KDTPOTYP = +7                                            
066300             MOVE JA           TO BIPACKNING-SW                           
066400         ELSE                                                             
066500           IF BIPA-KDORDKL       = +2 OR +3 OR +4                         
066600               MOVE JA           TO  BIPACKNING-SW                        
066700            ELSE                                                          
066800              IF BIPA-KDORDKL    = +1 AND                                 
066900                 BIPA-KDROPACK = '5' OR 'L'                               
067000                  MOVE JA        TO  BIPACKNING-SW                        
067100              END-IF                                                      
067200           END-IF                                                         
067300         END-IF                                                           
067400     END-IF                                                               
067500                                                                          
067600     IF BIPA-KDORDBEH          = +7                                       
067700         IF RAD-KDTPOTYP       = +5 OR +6 OR +2 OR +7                     
067800           MOVE WS-IDDC TO W-IDDC-B6                                      
067900           PERFORM IMS-GU-WDB601                                          
068000           IF SEGMENT-FINNS AND                                           
068100             (DCS-CDC OR (DCS-SDC AND NOT DCS-CHINA))                     
068200             PERFORM CACAA-FLYTTA-TILL-DLEV-AREA                          
068300             PERFORM S03-CALL-W411DLEV                                    
068400             IF  DLEV-IDLEVNR-UT = SPACE                                  
068500             AND DLEV-KDORDBEK-UT NOT = 21                                
068600             AND DLEV-FLSDCLEV-UT = NEJ                                   
068700                MOVE JA   TO BIPACKNING-SW                                
068800             END-IF                                                       
068900           ELSE                                                           
069000             MOVE JA TO BIPACKNING-SW                                     
069100           END-IF                                                         
069200         END-IF                                                           
069300     END-IF                                                               
069400     .                                                                    
069500     EJECT                                                                
069600 CACAA-FLYTTA-TILL-DLEV-AREA  SECTION.                                    
069700                                                                          
069800     MOVE BIPA-IDDISTR         TO DLEV-IDDISTR-IN                         
069900     MOVE BIPA-IDKUNDNR        TO DLEV-IDKUNDNR-IN                        
070000     MOVE RAD-KDORDKL          TO DLEV-KDORDKL-IN                         
070100     MOVE RAD-IDARTNR          TO DLEV-IDARTNR-IN                         
070200     MOVE AREG-IDLEVNR         TO DLEV-IDLEVNR-IN                         
070300     MOVE RAD-KVART            TO DLEV-KVBEART-Q-IN                       
070400     MOVE AREG-REDIRLEV        TO DLEV-REDIRLEV-IN                        
070500     MOVE RAD-IDDC             TO DLEV-IDDC-IN                            
070600     MOVE RAD-IDDC             TO DLEV-IDDC-ORD-IN                        
070700     MOVE RAD-IDKAMPRF         TO DLEV-IDKAMPRF-IN                        
070800     MOVE RAD-KDTPOTYP         TO DLEV-KDTPOTYP-IN                        
070900     MOVE AREG-KDUART          TO DLEV-KDUART-IN                          
071000     MOVE BIPA-FLFORBI         TO DLEV-FLFORBI-IN                         
071100     MOVE AREG-FLREFILL        TO DLEV-FLREFILL-IN                        
071200     MOVE RAD-KDORDING         TO DLEV-KDORDING-IN                        
071300     MOVE SPACE                TO DLEV-CLEARGROUP                         
071400                                  DLEV-KDOI-UT                            
071510     MOVE +1 TO IX-DCCLEAR                                                
071600     PERFORM UNTIL IX-DCCLEAR > IX-DCCLEAR-MAX                            
071700        MOVE SPACE         TO DLEV-IDDC-CLEAR-IN(IX-DCCLEAR)              
071800        ADD +1 TO IX-DCCLEAR                                              
071900     END-PERFORM                                                          
072000     .                                                                    
072100     EJECT                                                                
072200 CACB-KONTR-UTSKRIVEN-ORDERDEL  SECTION.                                  
072300                                                                          
072400     MOVE NEJ TO BIPACKNING-SW                                            
072500     IF AREG-ADLAGOMR NOT = +0 AND                                        
072600        BIPA-IDPRC-RAD    = BIPA-IDPRC-LAGOMR(ADLAGOMR-IX)                
072700         MOVE JA TO  BIPACKNING-SW                                        
072800     END-IF                                                               
072900     .                                                                    
073000     EJECT                                                                
073100                                                                          
073200 CAD-UPPD-WDA5        SECTION.                                            
073300                                                                          
073400     MOVE '4'                  TO  RAD-KDSTARAD                           
073500     MOVE SPACE                TO  RAD-IDKUNDRF-LEV                       
073600     MOVE BIPA-IDKUNDRF (1:5)  TO  RAD-IDKUNDRF-LEV (1:5)                 
073700     IF BIPA-KDORDBEH          =   +7                                     
073800         IF RAD-KDTPOTYP        =   +2 OR +6                              
073900             MOVE +0           TO  RAD-TITPO                              
074000         END-IF                                                           
074100     END-IF                                                               
074200     ACCEPT RAD-TIAVBOKN       FROM DATE                                  
074300                                                                          
074400     PERFORM IMS-REPL-ORDP                                                
074500                                                                          
074600     MOVE RAD-IDKUNDRF          TO  BIPA-IDKUNDRF-UT(IX)                  
074700     MOVE RAD-IDARTNR           TO  BIPA-IDARTNR(IX)                      
074800     MOVE RAD-IDLOPNR           TO  BIPA-IDLOPNR(IX)                      
074900     MOVE RAD-BERADREF          TO  BIPA-BERADREF(IX)                     
075000     MOVE RAD-IDKONTO           TO  BIPA-IDKONTO-UT(IX)                   
075100     MOVE RAD-IDKST             TO  BIPA-IDKST-UT(IX)                     
075200     MOVE RAD-IDANALYS          TO  BIPA-IDANALYS-UT(IX)                  
075300     MOVE RAD-IDSYSTEM          TO  BIPA-IDSYSTEM-UT(IX)                  
075400     MOVE RAD-KDDSP             TO  BIPA-KDDSP(IX)                        
075500     MOVE RAD-KDKVBRYT          TO  BIPA-KDKVBRYT(IX)                     
075600     MOVE RAD-KDORDING          TO  BIPA-KDORDING(IX)                     
075700     MOVE RAD-KDORDKL           TO  BIPA-KDORDKL-UT(IX)                   
075800     MOVE RAD-KDOI              TO  BIPA-KDOI      (IX)                   
075900     MOVE RAD-CLEARGROUP        TO  BIPA-CLEARGROUP(IX)                   
076000     MOVE RAD-KDPRODSL          TO  BIPA-KDPRODSL(IX)                     
076100     MOVE RAD-KDVRINFO          TO  BIPA-KDVRINFO(IX)                     
076200     MOVE RAD-KDTPOTYP          TO  BIPA-KDTPOTYP-UT(IX)                  
076300     MOVE RAD-KDFRAKT           TO  BIPA-KDFRAKT-UT(IX)                   
076400     MOVE RAD-KVART             TO  BIPA-KVART(IX)                        
076500     MOVE RAD-PRARTNTO          TO  BIPA-PRARTNTO(IX)                     
076600     MOVE RAD-PRAVCOST          TO  BIPA-PRAVCOST(IX)                     
076700     MOVE RAD-DEAL-PR-LINE      TO  BIPA-DEAL-PR-LINE(IX)                 
076800     MOVE RAD-FLPRTILL          TO  BIPA-FLPRTILL(IX)                     
076900     MOVE RAD-KDPRTYP           TO  BIPA-KDPRTYP(IX)                      
077000     MOVE RAD-REKSIFFR          TO  BIPA-REKSIFFR(IX)                     
077100     MOVE RAD-DARODAT (3:6)     TO  BIPA-TIRODAT(IX)                      
077200     MOVE RAD-FLINVEST          TO  BIPA-FLINVEST(IX)                     
077300     MOVE RAD-BEVOLREF          TO  BIPA-BEVOLREF(IX)                     
077400     MOVE RAD-IDLEVNR           TO  BIPA-IDLEVNR(IX)                      
077500     MOVE RAD-IDDC              TO  BIPA-IDDC-UT(IX)                      
077600     MOVE RAD-IDDC-RO           TO  BIPA-IDDC-RO(IX)                      
077700     MOVE RAD-TITPO             TO  BIPA-TITPO(IX)                        
077800     MOVE RAD-FLERS             TO  BIPA-FLERS(IX)                        
077900     MOVE RAD-IDKAMPRF          TO  BIPA-IDKAMPRF-UT(IX)                  
078000     MOVE ZERO                  TO  BIPA-KVBEART-Q(IX)                    
078100     MOVE RAD-TIREGDAT          TO  BIPA-TIREGDAT(IX)                     
078200     MOVE RAD-KDVALISO          TO  BIPA-KDVALISO(IX)                     
078300                                                                          
078400* FIX FIX FIX FIX FIX FIX                                                 
078500* NOLLNING OCH SPACE-NING PGA FEL                                         
078600*   I DESSA FLT PÅ WDA5                                                   
078700*    MOVE SPACE                 TO  BIPA-KDORDTYP-LDC(IX)                 
078800*    MOVE ZERO                  TO  BIPA-TIREPDAT(IX)                     
078900*    MOVE SPACE                 TO  BIPA-IDKUNDRF-WIP(IX)                 
079000* END FIX. ÖPPNA RADERNA NEDAN NÄR FIX TAS BORT                           
079100*    LDCN GK START *                                                      
079200     MOVE RAD-KDORDTYP-LDC      TO  BIPA-KDORDTYP-LDC(IX)                 
079300     MOVE RAD-TIREPDAT          TO  BIPA-TIREPDAT(IX)                     
079400     MOVE RAD-IDKUNDRF-WIP      TO  BIPA-IDKUNDRF-WIP(IX)                 
079500*    LDCN GK END   *                                                      
079600     .                                                                    
079700                                                                          
079800 S01-CALL-W411AREG    SECTION.                                            
079900                                                                          
080000     CALL W411AREG USING AREG-W411AREG                                    
080100                         WDK6-PCB                                         
080200                         WDK7-PCB                                         
080300     .                                                                    
080400     EJECT                                                                
080500 S02-CALL-W411SPAR    SECTION.                                            
080600                                                                          
080700     CALL W411SPAR USING SPAR-W411SPAR WDF8-PCB                           
080800                                       WDF8A-PCB                          
080900                                       WDK6-PCB                           
081000     .                                                                    
081100     EJECT                                                                
081200 S03-CALL-W411DLEV    SECTION.                                            
081300                                                                          
081400     MOVE ZERO     TO DLEV-KDCALL                                         
081500                      DLEV-IDKUNDRF-IN                                    
081600                                                                          
081700     CALL W411DLEV USING DLEV-W411DLEV LEVF-PCB                           
081800                                       LEVG-PCB                           
081900                                       LEVA-PCB                           
082000                                       ARTS2-PCB                          
082100                                       WDB6-PCB                           
082200                                       DUMMY-PCB                          
082300     .                                                                    
082400     EJECT                                                                
082500 S11-LAES-UNIK-WDA5 SECTION.                                              
082600                                                                          
082700     PERFORM IMS-GHN-UNIK-WDA5                                            
082800     MOVE +1                   TO IX                                      
082900     PERFORM UNTIL SEGMENT-SAKNAS     OR                                  
083000                   SEGMENT-SLUT       OR                                  
083100                   IX          > BIPA-KVBIPACK                            
083200         MOVE NEJ              TO BIPACKNING-SW                           
083300                                                                          
083400         PERFORM CA-SKAPA-UTAREA                                          
083500         IF IX < +14                                                      
083600            PERFORM IMS-GHN-UNIK-WDA5                                     
083700         END-IF                                                           
083800     END-PERFORM                                                          
083900     .                                                                    
084000     EJECT                                                                
084100                                                                          
084200 S12-LAES-ALL-WDA5 SECTION.                                               
084300                                                                          
084400     PERFORM IMS-GHN-ALL-WDA5                                             
084500     MOVE +1                   TO IX                                      
084600     PERFORM UNTIL SEGMENT-SAKNAS     OR                                  
084700                   SEGMENT-SLUT       OR                                  
084800                   IX          > BIPA-KVBIPACK                            
084900         MOVE NEJ              TO BIPACKNING-SW                           
085000         PERFORM CA-SKAPA-UTAREA                                          
085100         IF IX < +14                                                      
085200            PERFORM IMS-GHN-ALL-WDA5                                      
085300         END-IF                                                           
085400     END-PERFORM                                                          
085500     .                                                                    
085600     EJECT                                                                
085700                                                                          
085800 IMS-GHN-UNIK-WDA5 SECTION.                                               
085900                                                                          
086000     STRING 'WLORDP01(WDA5BSEQ =' W-WDA5BSEQ-X ')'                        
086100            DELIMITED BY SIZE INTO SSA1                                   
086200     MOVE '  GEGB'               TO GODK-STATUSKODER                      
086300     CALL CBLTDLI USING GHN ORDP-PCB DLI-IO-AREA SSA1                     
086400     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
086500     PERFORM IMS-STATUSKONTROLL                                           
086600     .                                                                    
086700     SKIP2                                                                
086800 IMS-GHN-ALL-WDA5 SECTION.                                                
086900                                                                          
087000     STRING 'WLORDP01(WDA5BSEQ>=' W-WDA5BSEQ-MIN-X                        
087100                    '&WDA5BSEQ<=' W-WDA5BSEQ-MAX-X ')'                    
087200            DELIMITED BY SIZE INTO SSA1                                   
087300     MOVE '  GEGB'               TO GODK-STATUSKODER                      
087400     CALL CBLTDLI USING GHN ORDP-PCB DLI-IO-AREA SSA1                     
087500     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
087600     PERFORM IMS-STATUSKONTROLL                                           
087700     .                                                                    
087800     SKIP2                                                                
087900 IMS-REPL-ORDP        SECTION.                                            
088000                                                                          
088100     MOVE '  '                 TO GODK-STATUSKODER                        
088200     CALL CBLTDLI USING REPL ORDP-PCB DLI-IO-AREA                         
088300     MOVE ORDP-STATUS-CODE     TO STATUS-WS                               
088400     PERFORM IMS-STATUSKONTROLL                                           
088500     .                                                                    
088600                                                                          
088700 IMS-GU-WDB601    SECTION.                                                
088800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
088900          DELIMITED BY SIZE INTO SSA1                                     
089000     MOVE '  GE' TO GODK-STATUSKODER                                      
089100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
089200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
089300     PERFORM IMS-STATUSKONTROLL                                           
089400     .                                                                    
089500     EJECT                                                                
089600 IMS-STATUSKONTROLL SECTION.                                              
089700                                                                          
089800     SET STATUS-IX TO 1                                                   
089900     SEARCH GODK-STATUS                                                   
090000       AT END CALL FELLOG                                                 
090100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
090200     END-SEARCH                                                           
090300     .                                                                    

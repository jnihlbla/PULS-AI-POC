000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4041110.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   96/04/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KUNDREGISTER.GODSMOTTAGARE INFORMATION 1.                        
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDB2                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        REQU:        W40411I1                                            
001400*                                                                         
001500*    UTDATA.                                                              
001600*        RESP:        W40411O1                                            
001700*                                                                         
001800*    E-TRACKER: 2913019 DATE 20060210                                     
001900*                                                                         
002000*    E-TRACKER: 7450328  HÖST -08   VOHF                                  
002100*                                                                         
002200                                                                          
002300*    E-TRACKER: 8081720  ADD FLVOHF-KL(*) FOR EACH ORDER CLASS            
002400*                                                                         
002500*    E-TRACKER: 10254592 2015       DECOMISSION VOHF                      
002600*                                                                         
002700*    STORY 2373879 / REPLACE DIST35- 88 LEVELS OF IN,KR,                  
002800*               AE,CN CDC RETURNS TO DIST35-CDC-RETURNS-NON-VCC           
002900*                                                                         
003000*    STORY 3101308/MAKE PARTNER ID IN LINE 5 OF 4411 EDITABLE             
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700*    -COPY WY2000W1                                                       
003800     SKIP3                                                                
003900 77  IDPGM                       PIC X(08)   VALUE 'W4041110'.            
004000                                                                          
004100*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004200 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004300                                                                          
004400 77  YES                         PIC X       VALUE 'Y'.                   
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004800                                                                          
004900 01  ALL-PLUS.                                                            
005000     03  FILLER                  PIC X(80)   VALUE ALL '+'.               
005100 01  ALL-SPACE.                                                           
005200     03  FILLER                  PIC X(80)   VALUE ALL SPACE.             
005300                                                                          
005400 77  WS-IDDISTR                  PIC S9(5)   VALUE ZERO COMP-3.           
005500 77  WS-IDKUNDNR                 PIC S9(7)   VALUE ZERO COMP-3.           
005600 77  WS-ADPOSTNR                 PIC X(10).                               
005700 77  WS-ADCITY                   PIC X(25).                               
005800 77  TEST-IDKUNDNR               PIC X(6)    VALUE SPACE.                 
005900                                                                          
006000 77  IDEX                        PIC S9(3) COMP-3 VALUE ZERO.             
006100 77  IDEX-TVSVOR-MAX             PIC S9(2) COMP-3 VALUE +16.              
006200 77  IDEX-PREPLAN-MAX            PIC S9(2) COMP-3 VALUE +8.               
006300 77  IX-DCCLEAR-MAX              PIC S9(3) VALUE +99  COMP SYNC.          
006400                                                                          
006500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006600                                                                          
006700 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006800     88  ALLT-OK                             VALUE 'J'.                   
006900                                                                          
007000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007100     88  INDATA-OK                           VALUE 'J'.                   
007200     88  INDATA-FEL                          VALUE 'N'.                   
007300                                                                          
007400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007500     88  NYCKLAR-OK                          VALUE 'J'.                   
007600     88  NYCKLAR-FEL                         VALUE 'N'.                   
007700                                                                          
007800 77  SW-DISTRIKT-OK              PIC X       VALUE 'J'.                   
007900     88  DISTRIKT-OK                         VALUE 'J'.                   
008000     88  DISTRIKT-FEL                        VALUE 'N'.                   
008100                                                                          
008200 77  SW-KDKUNDKAT                PIC X       VALUE 'N'.                   
008300     88  KDKUNDKAT-MODIFY-Y                  VALUE 'J'.                   
008400     88  KDKUNDKAT-MODIFY-N                  VALUE 'N'.                   
008500                                                                          
008600 01  FILLER                      PIC X(16)  VALUE 'CUSTCAT-AREA'.         
008700     SKIP3                                                                
008800*01 -COPY WWTEXT02                                                        
008900     EJECT                                                                
009000 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
009100                                                                          
009200*    *** USA/CANADA DISTRIKT ******                                       
009300 01  FILLER REDEFINES TEST-IDDISTR.                                       
009400*    03     -COPY WWDIST07.                                               
009500                                                                          
009600*    *** SKROTDISTRIKT  ***********                                       
009700 01  FILLER REDEFINES TEST-IDDISTR.                                       
009800*    03     -COPY WWDIST18.                                               
009900                                                                          
010000*    *** SATSDISTRIKT   ***********                                       
010100 01  FILLER REDEFINES TEST-IDDISTR.                                       
010200*    03     -COPY WWDIST19.                                               
010300                                                                          
010400*    *** RETUR- REFILLDISTRIKT ****                                       
010500 01  FILLER REDEFINES TEST-IDDISTR.                                       
010600*    03     -COPY WWDIST35.                                               
010700                                                                          
010800*    *** GODKÄNDA FÖRETAG      ****                                       
010900 01  -COPY WWIDFTG -PRE CHK-                                              
011000                                                                          
011100     EJECT                                                                
011200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011300 01  GENERELLA-SUBPROGRAM.                                                
011400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011600     03  WISOLAND                PIC X(8)    VALUE 'WISOLAND'.            
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011900*01 -COPY WMEDAREA                                                        
012000*    --- PARAMETRAR TILL SUBPROGRAM WISOLAND                              
012100 01  FILLER                      PIC X(16)   VALUE 'WISOLAND'.            
012200*01 -COPY WISOLAND                                                        
012300     EJECT                                                                
012400     SKIP3                                                                
012500 01  MESSAGE-CODES.                                                       
012600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '020'.                 
012700     03  INF-PRESS-PF11          PIC X(3)    VALUE '013'.                 
012800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '014'.                 
012900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '001'.                 
013000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
013100     03  ERR-GOODS-RCVR-MISS     PIC X(3)    VALUE '025'.                 
013200     EJECT                                                                
013300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013600     SKIP3                                                                
013700*01  -COPY WMFSAREA                                                       
013800     EJECT                                                                
013900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014000*                                                                         
014100                                                                          
014200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014300     SKIP3                                                                
014400 01  NYCKLAR-TILL-DLI.                                                    
014500     03  W-IDGMT-X.                                                       
014600         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
014700         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
014800     SKIP2                                                                
014900     03  W-IDGMT-COPY-X.                                                  
015000         05  W-IDDISTR-COPY      PIC S9(5)   VALUE ZERO COMP-3.           
015100         05  W-IDKUNDNR-COPY     PIC S9(7)   VALUE ZERO COMP-3.           
015200     SKIP2                                                                
015300     03  W-IDPARTNER-X.                                                   
015400         05  W-IDPARTNER         PIC X(09)   VALUE SPACES.                
015500     SKIP2                                                                
015600     03  W-IDGMT-MIN-X.                                                   
015700         05  FILLER              PIC X(7) VALUE LOW-VALUES.               
015800     SKIP2                                                                
015900     03  W-IDGMT-MAX-X.                                                   
016000         05  FILLER              PIC X(7) VALUE HIGH-VALUES.              
016100     SKIP2                                                                
016200*    --- STATUS-KOD FRÅN IMS                                              
016300 01  STATUS-WS                   PIC XX.                                  
016400     88  SEGMENT-FINNS                       VALUE '  '.                  
016500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016700     SKIP2                                                                
016800 01  GODK-STATUSKODER.                                                    
016900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017000     SKIP3                                                                
017100 01  SSA1                        PIC X(96).                               
017200 01  SSA2                        PIC X(64).                               
017300     EJECT                                                                
017400*    --- IMS FUNKTIONSKODER                                               
017500*01  -COPY W0003                                                          
017600     EJECT                                                                
017700*    ---  DLI INPUT-OUTPUT AREA                                           
017800 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
017900     SKIP3                                                                
018000 01  DLI-IO-WDB2.                                                         
018100*    03  -COPY WDB201  -PRE WDB2-                                         
018200     EJECT                                                                
018300 01  FILLER                      PIC X(16)   VALUE                        
018400                                             'COPY-WDB201-AREA'.          
018500 01  DLI-IO-WDB2-2.                                                       
018600*    03  -COPY WDB201  -PRE COPY-                                         
018700                                                                          
018800                                                                          
018900 LINKAGE SECTION.                                                         
019000 01  REQU-AREA.                                                           
019100*    03 -COPY WZ01REQU                                                    
019200*    03 -COPY W40411I1                                                    
019300     EJECT                                                                
019400 01  RESP-AREA.                                                           
019500*    03 -COPY WZ01RESP                                                    
019600*    03 -COPY W40411O1                                                    
019700     EJECT                                                                
019800*01  -COPY W0008  -PRE WDB2-                                              
019900     05  FILLER                  PIC X.                                   
020000     EJECT                                                                
020100 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA WDB2-PCB.                  
020200 MAIN SECTION.                                                            
020300     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA WDB2-PCB.                  
020400                                                                          
020500     PERFORM A-INIT                                                       
020600     PERFORM B-KOLLA-NYCKLAR                                              
020700     IF NYCKLAR-OK                                                        
020800       IF REQU-UPDATE                                                     
020900         PERFORM G-KOLLA-INPUT                                            
021000         IF INDATA-OK                                                     
021100           PERFORM H-UPPDATERA                                            
021200         END-IF                                                           
021300       ELSE                                                               
021400         IF REQU-FIRST                                                    
021500           PERFORM C-FOERSTA-SIDA                                         
021600         ELSE                                                             
021700           PERFORM E-SAMMA-SIDA                                           
021800         END-IF                                                           
021900       END-IF                                                             
022000       IF ALLT-OK                                                         
022100         PERFORM F-LAES-VISA-INFO                                         
022200       END-IF                                                             
022300     END-IF                                                               
022400     GOBACK                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 A-INIT SECTION.                                                          
022800                                                                          
022900     MOVE ALL '+'                TO RESP-W40411O1                         
023000                                                                          
023100     PERFORM MFS-FORM-ATTR                                                
023200                                                                          
023300     MOVE 001                    TO RESP-IDMSGVER                         
023400     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
023500                                    RESP-IDMSG-INFO                       
023600                                    RESP-IDELMT-ERROR                     
023700                                                                          
023800     ACCEPT DAGENS-DATUM       FROM DATE                                  
023900     .                                                                    
024000     EJECT                                                                
024100 B-KOLLA-NYCKLAR SECTION.                                                 
024200                                                                          
024300     MOVE JA                     TO NYCKLAR-SW                            
024400                                                                          
024500                                                                          
024600*    -- KONTROLL AV IDDISTR                                               
024700     IF REQU-IDDISTR-KEY NUMERIC                                          
024800       MOVE REQU-IDDISTR-KEY     TO W-IDDISTR                             
024900                                    W-IDDISTR-COPY                        
025000                                    WS-IDDISTR                            
025100     ELSE                                                                 
025200       MOVE NEJ                  TO NYCKLAR-SW                            
025300     END-IF                                                               
025400                                                                          
025500*    -- KONTROLL AV IDKUNDNR                                              
025600                                                                          
025700     IF REQU-IDKUNDNR-KEY NUMERIC                                         
025800       MOVE REQU-IDKUNDNR-KEY    TO W-IDKUNDNR                            
025900                                    WS-IDKUNDNR                           
026000                                    TEST-IDKUNDNR                         
026100     ELSE                                                                 
026200       MOVE NEJ                  TO NYCKLAR-SW                            
026300     END-IF                                                               
026400                                                                          
026500                                                                          
026600     IF NYCKLAR-FEL                                                       
026700       MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                      
026800       PERFORM MFS-RENSA-FAELT-IN                                         
026900       PERFORM MFS-RENSA-FAELT-UT                                         
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300 C-FOERSTA-SIDA SECTION.                                                  
027400                                                                          
027500     MOVE JA TO ALLT-SW                                                   
027600     PERFORM MFS-RENSA-FAELT-IN                                           
027700     .                                                                    
027800     EJECT                                                                
027900 E-SAMMA-SIDA SECTION.                                                    
028000                                                                          
028100     IF REQU-INPUT = ALL '+'                                              
028200       PERFORM MFS-RENSA-FAELT-IN                                         
028300       MOVE JA                   TO ALLT-SW                               
028400     ELSE                                                                 
028500       MOVE NEJ                  TO ALLT-SW                               
028600       MOVE INF-PRESS-PF11       TO RESP-IDMSG-INFO                       
028700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
028800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
028900       PERFORM EA-MID-INDATA-TILL-MOD                                     
029000     END-IF                                                               
029100     .                                                                    
029200     EJECT                                                                
029300 EA-MID-INDATA-TILL-MOD SECTION.                                          
029400                                                                          
029500     IF REQU-IDKUNDNR-COPY NOT = ALL '+'                                  
029600       MOVE REQU-IDKUNDNR-COPY    TO RESP-IDKUNDNR-COPY                   
029700       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDKUNDNR-COPY-ATTR              
029800     ELSE                                                                 
029900       MOVE ALL-PLUS              TO RESP-IDKUNDNR-COPY                   
030000     END-IF                                                               
030100                                                                          
030200     IF REQU-BEGMT-RAD1 NOT = ALL '+'                                     
030300       MOVE REQU-BEGMT-RAD1       TO RESP-BEGMT-RAD1                      
030400       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-BEGMT-RAD1-ATTR                 
030500     ELSE                                                                 
030600       MOVE ALL-PLUS              TO RESP-BEGMT-RAD1                      
030700     END-IF                                                               
030800                                                                          
030900     IF REQU-BEGMT-RAD2 NOT = ALL '+'                                     
031000       MOVE REQU-BEGMT-RAD2       TO RESP-BEGMT-RAD2                      
031100       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-BEGMT-RAD2-ATTR                 
031200     ELSE                                                                 
031300       MOVE ALL-PLUS              TO RESP-BEGMT-RAD2                      
031400     END-IF                                                               
031500                                                                          
031600     IF REQU-IDDEALER-VIPS NOT = ALL '+'                                  
031700       MOVE REQU-IDDEALER-VIPS                                            
031800                                  TO RESP-IDDEALER-VIPS                   
031900       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDDEALER-VIPS-ATTR              
032000     ELSE                                                                 
032100       MOVE ALL-PLUS              TO RESP-IDDEALER-VIPS                   
032200     END-IF                                                               
032300                                                                          
032400     IF REQU-ADGMT-GATA NOT = ALL '+'                                     
032500       MOVE REQU-ADGMT-GATA       TO RESP-ADGMT-GATA                      
032600       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-ADGMT-GATA-ATTR                 
032700     ELSE                                                                 
032800       MOVE ALL-PLUS              TO RESP-ADGMT-GATA                      
032900     END-IF                                                               
033000                                                                          
033100     IF REQU-IDLANDX2 NOT = ALL '+'                                       
033200       MOVE REQU-IDLANDX2         TO RESP-IDLANDX2                        
033300       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDLANDX2-ATTR                   
033400     ELSE                                                                 
033500       MOVE ALL-PLUS              TO RESP-IDLANDX2                        
033600     END-IF                                                               
033700                                                                          
033800     IF REQU-ADPOSTNR  NOT = ALL '+'                                      
033900       MOVE REQU-ADPOSTNR         TO RESP-ADPOSTNR                        
034000       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-ADPOSTNR-ATTR                   
034100     ELSE                                                                 
034200       MOVE ALL-PLUS              TO RESP-ADPOSTNR                        
034300     END-IF                                                               
034400                                                                          
034500     IF REQU-KDPOSTNR  NOT = ALL '+'                                      
034600       MOVE REQU-KDPOSTNR         TO RESP-KDPOSTNR                        
034700       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KDPOSTNR-ATTR                   
034800     ELSE                                                                 
034900       MOVE ALL-PLUS              TO RESP-KDPOSTNR                        
035000     END-IF                                                               
035100                                                                          
035200     IF REQU-KDKUNDKAT  NOT = ALL '+'                                     
035300       MOVE REQU-KDKUNDKAT        TO RESP-KDKUNDKAT                       
035400       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KDKUNDKAT-ATTR                  
035500     ELSE                                                                 
035600       MOVE ALL-PLUS              TO RESP-KDKUNDKAT                       
035700     END-IF                                                               
035800                                                                          
035900     IF REQU-ADCITY    NOT = ALL '+'                                      
036000       MOVE REQU-ADCITY           TO RESP-ADCITY                          
036100       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-ADCITY-ATTR                     
036200     ELSE                                                                 
036300       MOVE ALL-PLUS              TO RESP-ADCITY                          
036400     END-IF                                                               
036500                                                                          
036600     IF REQU-IDLONGITUDE NOT = ALL '+'                                    
036700       MOVE REQU-IDLONGITUDE      TO RESP-IDLONGITUDE                     
036800       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDLONGITUDE-ATTR                
036900     ELSE                                                                 
037000       MOVE ALL-PLUS              TO RESP-IDLONGITUDE                     
037100     END-IF                                                               
037200                                                                          
037300     IF REQU-ADGMT-LAND NOT = ALL '+'                                     
037400       MOVE REQU-ADGMT-LAND       TO RESP-ADGMT-LAND                      
037500       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-ADGMT-LAND-ATTR                 
037600     ELSE                                                                 
037700       MOVE ALL-PLUS              TO RESP-ADGMT-LAND                      
037800     END-IF                                                               
037900                                                                          
038000     IF REQU-IDLATITUDE NOT = ALL '+'                                     
038100       MOVE REQU-IDLATITUDE       TO RESP-IDLATITUDE                      
038200       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDLATITUDE-ATTR                 
038300     ELSE                                                                 
038400       MOVE ALL-PLUS              TO RESP-IDLATITUDE                      
038500     END-IF                                                               
038600                                                                          
038700     IF REQU-IDTFN NOT = ALL '+'                                          
038800       MOVE REQU-IDTFN            TO RESP-IDTFN                           
038900       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDTFN-ATTR                      
039000     ELSE                                                                 
039100       MOVE ALL-PLUS              TO RESP-IDTFN                           
039200     END-IF                                                               
039300                                                                          
039400     IF REQU-BETEXT NOT = ALL '+'                                         
039500       MOVE REQU-BETEXT           TO RESP-BETEXT                          
039600       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-BETEXT-ATTR                     
039700     ELSE                                                                 
039800       MOVE ALL-PLUS              TO RESP-BETEXT                          
039900     END-IF                                                               
040000                                                                          
040100     IF REQU-FLRESTN NOT = ALL '+'                                        
040200       IF  REQU-FLRESTN = 'J'                                             
040300           MOVE 'Y'                   TO RESP-FLRESTN                     
040400       ELSE                                                               
040500           MOVE REQU-FLRESTN          TO RESP-FLRESTN                     
040600       END-IF                                                             
040700       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLRESTN-ATTR                    
040800     ELSE                                                                 
040900       MOVE ALL-PLUS              TO RESP-FLRESTN                         
041000     END-IF                                                               
041100                                                                          
041200     IF REQU-FLPRELRO NOT = ALL '+'                                       
041300       IF  REQU-FLPRELRO = 'J'                                            
041400           MOVE 'Y'                   TO RESP-FLPRELRO                    
041500       ELSE                                                               
041600           MOVE REQU-FLPRELRO         TO RESP-FLPRELRO                    
041700       END-IF                                                             
041800       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLPRELRO-ATTR                   
041900     ELSE                                                                 
042000       MOVE ALL-PLUS              TO RESP-FLPRELRO                        
042100     END-IF                                                               
042200                                                                          
042300     IF REQU-RESLATT-UPD NOT = ALL '+'                                    
042400       MOVE ALL-PLUS              TO RESP-RESLATT-UPD                     
042500       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-RESLATT-UPD-ATTR                
042600     ELSE                                                                 
042700       MOVE ALL-SPACE             TO RESP-RESLATT-UPD                     
042800     END-IF                                                               
042900                                                                          
043000     IF REQU-IDKUNDNR-H-UPD NOT = ALL '+'                                 
043100       MOVE REQU-IDKUNDNR-H-UPD   TO RESP-IDKUNDNR-H-UPD                  
043200       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDKUNDNR-H-UPD-ATTR             
043300     ELSE                                                                 
043400       MOVE ALL-PLUS              TO RESP-IDKUNDNR-H-UPD                  
043500     END-IF                                                               
043600                                                                          
043700     IF REQU-KDBEKALT-UPD NOT = ALL '+'                                   
043800       MOVE REQU-KDBEKALT-UPD     TO RESP-KDBEKALT-UPD                    
043900       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-KDBEKALT-UPD-ATTR               
044000     ELSE                                                                 
044100       MOVE ALL-PLUS              TO RESP-KDBEKALT-UPD                    
044200     END-IF                                                               
044300                                                                          
044400     IF REQU-FLOBKR-TACD-UPD NOT = ALL '+'                                
044500       IF  REQU-FLOBKR-TACD-UPD = 'J'                                     
044600           MOVE 'Y'                   TO RESP-FLOBKR-TACD-UPD             
044700       ELSE                                                               
044800           MOVE REQU-FLOBKR-TACD-UPD  TO RESP-FLOBKR-TACD-UPD             
044900       END-IF                                                             
045000       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLOBKR-TACD-UPD-ATTR            
045100     ELSE                                                                 
045200       MOVE ALL-PLUS              TO RESP-FLOBKR-TACD-UPD                 
045300     END-IF                                                               
045400                                                                          
045500     IF REQU-FLDNDAP-UPD NOT = ALL '+'                                    
045600       IF  REQU-FLDNDAP-UPD = 'J'                                         
045700           MOVE 'Y'                   TO RESP-FLDNDAP-UPD                 
045800       ELSE                                                               
045900           MOVE REQU-FLDNDAP-UPD      TO RESP-FLDNDAP-UPD                 
046000       END-IF                                                             
046100       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLDNDAP-UPD-ATTR                
046200     ELSE                                                                 
046300       MOVE ALL-PLUS              TO RESP-FLDNDAP-UPD                     
046400     END-IF                                                               
046500                                                                          
046600     IF REQU-FLORDTIL-KL1 NOT = ALL '+'                                   
046700       IF  REQU-FLORDTIL-KL1 = 'J'                                        
046800           MOVE 'Y'                   TO RESP-FLORDTIL-KL1                
046900       ELSE                                                               
047000           MOVE REQU-FLORDTIL-KL1     TO RESP-FLORDTIL-KL1                
047100       END-IF                                                             
047200       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLORDTIL-KL1-ATTR               
047300     ELSE                                                                 
047400       MOVE ALL-PLUS              TO RESP-FLORDTIL-KL1                    
047500     END-IF                                                               
047600                                                                          
047700     IF REQU-FLORDTIL-KL2 NOT = ALL '+'                                   
047800       IF  REQU-FLORDTIL-KL2 = 'J'                                        
047900           MOVE 'Y'                   TO RESP-FLORDTIL-KL2                
048000       ELSE                                                               
048100           MOVE REQU-FLORDTIL-KL2     TO RESP-FLORDTIL-KL2                
048200       END-IF                                                             
048300       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLORDTIL-KL2-ATTR               
048400     ELSE                                                                 
048500       MOVE ALL-PLUS              TO RESP-FLORDTIL-KL2                    
048600     END-IF                                                               
048700                                                                          
048800     IF REQU-FLORDTIL-KL3 NOT = ALL '+'                                   
048900       IF  REQU-FLORDTIL-KL3 = 'J'                                        
049000           MOVE 'Y'                   TO RESP-FLORDTIL-KL3                
049100       ELSE                                                               
049200           MOVE REQU-FLORDTIL-KL3     TO RESP-FLORDTIL-KL3                
049300       END-IF                                                             
049400       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLORDTIL-KL3-ATTR               
049500     ELSE                                                                 
049600       MOVE ALL-PLUS              TO RESP-FLORDTIL-KL3                    
049700     END-IF                                                               
049800                                                                          
049900     IF REQU-FLORDTIL-KL4 NOT = ALL '+'                                   
050000       IF  REQU-FLORDTIL-KL4 = 'J'                                        
050100           MOVE 'Y'                   TO RESP-FLORDTIL-KL4                
050200       ELSE                                                               
050300           MOVE REQU-FLORDTIL-KL4     TO RESP-FLORDTIL-KL4                
050400       END-IF                                                             
050500       MOVE MFS-ADD-LAES-IN-FAELT TO RESP-FLORDTIL-KL4-ATTR               
050600     ELSE                                                                 
050700       MOVE ALL-PLUS              TO RESP-FLORDTIL-KL4                    
050800     END-IF                                                               
050900                                                                          
051000     .                                                                    
051100     EJECT                                                                
051200 F-LAES-VISA-INFO SECTION.                                                
051300                                                                          
051400     PERFORM IMS-GHU-WDB201                                               
051500                                                                          
051600     IF SEGMENT-SAKNAS                                                    
051700       MOVE ERR-GOODS-RCVR-MISS  TO RESP-IDMSG-ERROR                      
051800       MOVE 'IDGMT'              TO RESP-IDELMT-ERROR                     
051900       PERFORM MFS-RENSA-FAELT-UT                                         
052000     ELSE                                                                 
052100       MOVE WDB2-GMT-BEGMT-RAD1          TO RESP-BEGMT-RAD1               
052200       MOVE WDB2-GMT-IDPARTNER           TO RESP-IDPARTNER                
052300       MOVE WDB2-GMT-BEGMT-RAD2          TO RESP-BEGMT-RAD2               
052400       MOVE WDB2-GMT-IDDEALER-VIPS       TO RESP-IDDEALER-VIPS            
052500       MOVE WDB2-GMT-ADGMT-GATA          TO RESP-ADGMT-GATA               
052600       MOVE WDB2-GMT-IDLANDX2            TO RESP-IDLANDX2                 
052700       MOVE WDB2-GMT-KDPOSTNR            TO RESP-KDPOSTNR                 
052800       MOVE WDB2-GMT-KDKUNDKAT           TO RESP-KDKUNDKAT                
052900       IF WDB2-GMT-KDPOSTNR = 'L'                                         
053000          MOVE WDB2-GMT-ADPOSTNR IN WDB2-GMT-ADPOST-PNRORT                
053100                                         TO RESP-ADPOSTNR                 
053200          MOVE WDB2-GMT-ADCITY   IN WDB2-GMT-ADPOST-PNRORT                
053300                                         TO RESP-ADCITY                   
053400       ELSE                                                               
053500          MOVE WDB2-GMT-ADPOSTNR IN WDB2-GMT-ADPOST-ORTPNR                
053600                                         TO RESP-ADPOSTNR                 
053700          MOVE WDB2-GMT-ADCITY   IN WDB2-GMT-ADPOST-ORTPNR                
053800                                         TO RESP-ADCITY                   
053900       END-IF                                                             
054000       MOVE WDB2-GMT-IDLONGITUDE         TO RESP-IDLONGITUDE              
054100       MOVE WDB2-GMT-ADGMT-LAND          TO RESP-ADGMT-LAND               
054200       MOVE WDB2-GMT-IDLATITUDE          TO RESP-IDLATITUDE               
054300       MOVE WDB2-GMT-IDTFN               TO RESP-IDTFN                    
054400       MOVE WDB2-GMT-BETEXT              TO RESP-BETEXT                   
054500       IF  WDB2-GMT-FLRESTN = 'J'                                         
054600           MOVE 'Y'                      TO RESP-FLRESTN                  
054700       ELSE                                                               
054800           MOVE WDB2-GMT-FLRESTN         TO RESP-FLRESTN                  
054900       END-IF                                                             
055000       IF  WDB2-GMT-FLOBKR-TACD = 'J'                                     
055100           MOVE 'Y'                      TO RESP-FLOBKR-TACD-UT           
055200       ELSE                                                               
055300           MOVE WDB2-GMT-FLOBKR-TACD     TO RESP-FLOBKR-TACD-UT           
055400       END-IF                                                             
055500       IF  WDB2-GMT-FLDNDAP = 'J'                                         
055600           MOVE 'Y'                      TO RESP-FLDNDAP-UT               
055700       ELSE                                                               
055800           MOVE WDB2-GMT-FLDNDAP         TO RESP-FLDNDAP-UT               
055900       END-IF                                                             
056000       MOVE WDB2-GMT-IDZON               TO RESP-IDZON                    
056100       IF  WDB2-GMT-FLPRELRO = 'J'                                        
056200           MOVE 'Y'                      TO RESP-FLPRELRO                 
056300       ELSE                                                               
056400           MOVE WDB2-GMT-FLPRELRO        TO RESP-FLPRELRO                 
056500       END-IF                                                             
056600       MOVE WDB2-GMT-IDDEPOT             TO RESP-IDDEPOT                  
056700       MOVE WDB2-GMT-RESLATT             TO RESP-RESLATT-UT               
056800       MOVE WDB2-GMT-IDROUTE             TO RESP-IDROUTE                  
056900       MOVE WDB2-GMT-IDKUNDNR-HEAD       TO RESP-IDKUNDNR-H-UT            
057000       MOVE WDB2-GMT-KDBEKALT            TO RESP-KDBEKALT-UT              
057100       IF  WDB2-GMT-FLORDTIL-KL1 = 'J'                                    
057200           MOVE 'Y'                      TO RESP-FLORDTIL-KL1             
057300       ELSE                                                               
057400           MOVE WDB2-GMT-FLORDTIL-KL1    TO RESP-FLORDTIL-KL1             
057500       END-IF                                                             
057600       IF  WDB2-GMT-FLORDTIL-KL2 = 'J'                                    
057700           MOVE 'Y'                      TO RESP-FLORDTIL-KL2             
057800       ELSE                                                               
057900           MOVE WDB2-GMT-FLORDTIL-KL2    TO RESP-FLORDTIL-KL2             
058000       END-IF                                                             
058100       IF  WDB2-GMT-FLORDTIL-KL3 = 'J'                                    
058200           MOVE 'Y'                      TO RESP-FLORDTIL-KL3             
058300       ELSE                                                               
058400           MOVE WDB2-GMT-FLORDTIL-KL3    TO RESP-FLORDTIL-KL3             
058500       END-IF                                                             
058600       IF  WDB2-GMT-FLORDTIL-KL4 = 'J'                                    
058700           MOVE 'Y'                      TO RESP-FLORDTIL-KL4             
058800       ELSE                                                               
058900           MOVE WDB2-GMT-FLORDTIL-KL4    TO RESP-FLORDTIL-KL4             
059000       END-IF                                                             
059100     END-IF                                                               
059200     .                                                                    
059300     EJECT                                                                
059400 G-KOLLA-INPUT SECTION.                                                   
059500                                                                          
059600     MOVE JA  TO INDATA-SW                                                
059700                                                                          
059800     PERFORM IMS-GHU-WDB201                                               
059900     IF SEGMENT-SAKNAS                                                    
060000       IF REQU-IDKUNDNR-COPY NOT = ALL '+'                                
060100         MOVE WS-IDDISTR TO TEST-IDDISTR                                  
060200         IF DIST18-SCRAP-NDC OR DIST18-SCRAP-NDC-SC-LOCAL                 
060300            MOVE MFS-NUM-FAELT-FEL TO RESP-IDKUNDNR-COPY-ATTR             
060400            MOVE NEJ TO INDATA-SW                                         
060500         ELSE                                                             
060600           IF REQU-IDKUNDNR-COPY NUMERIC                                  
060700             MOVE REQU-IDKUNDNR-COPY TO W-IDKUNDNR-COPY                   
060800             PERFORM IMS-GU-WDB201-COPY                                   
060900             IF SEGMENT-FINNS                                             
061000               IF COPY-GMT-KDKUNDKAT = '00'                               
061100                 IF REQU-KDKUNDKAT = ALL '+'                              
061200                   MOVE COPY-GMT-KDKUNDKAT  TO RESP-KDKUNDKAT             
061300                   MOVE MFS-ALFA-FAELT-FEL  TO RESP-KDKUNDKAT-ATTR        
061400                   MOVE NEJ                 TO INDATA-SW                  
061500                 ELSE                                                     
061600                   INSPECT REQU-KDKUNDKAT REPLACING                       
061700                                            LEADING SPACE BY ZERO         
061800                   MOVE FUNCTION TRIM(REQU-KDKUNDKAT)                     
061900                                           TO TEXT02-KDKUNDKAT            
062000                   MOVE TEXT02-KDKUNDKAT   TO REQU-KDKUNDKAT              
062100                   SET KDKUNDKAT-MODIFY-Y  TO TRUE                        
062200                   IF GOOD-KDKUNDKAT                                      
062300                     MOVE MFS-ALFA-FAELT-RAETT                            
062400                                         TO RESP-KDKUNDKAT-ATTR           
062500                   ELSE                                                   
062600                     MOVE MFS-ALFA-FAELT-FEL                              
062700                                         TO RESP-KDKUNDKAT-ATTR           
062800                     MOVE NEJ            TO INDATA-SW                     
062900                   END-IF                                                 
063000                 END-IF                                                   
063100               END-IF                                                     
063200               IF COPY-GMT-TISTADAT > ZERO                                
063300                 IF COPY-GMT-TISTODAT > ZERO                              
063400                   MOVE COPY-GMT-TISTODAT TO TMP1-YYMMDD                  
063500                   MOVE DAGENS-DATUM      TO TMP2-YYMMDD                  
063600                   PERFORM WY2000P1                                       
063700                   IF TMP1-YYMMDD > TMP2-YYMMDD                           
063800                     MOVE MFS-NUM-FAELT-RAETT TO                          
063900                          RESP-IDKUNDNR-COPY-ATTR                         
064000                   ELSE                                                   
064100                     MOVE MFS-NUM-FAELT-FEL TO                            
064200                          RESP-IDKUNDNR-COPY-ATTR                         
064300                     MOVE NEJ TO INDATA-SW                                
064400                   END-IF                                                 
064500                 ELSE                                                     
064600                   MOVE MFS-NUM-FAELT-RAETT TO                            
064700                          RESP-IDKUNDNR-COPY-ATTR                         
064800                 END-IF                                                   
064900               ELSE                                                       
065000                 MOVE MFS-NUM-FAELT-FEL TO RESP-IDKUNDNR-COPY-ATTR        
065100                 MOVE NEJ TO INDATA-SW                                    
065200               END-IF                                                     
065300             ELSE                                                         
065400               MOVE MFS-NUM-FAELT-FEL TO RESP-IDKUNDNR-COPY-ATTR          
065500               MOVE NEJ TO INDATA-SW                                      
065600             END-IF                                                       
065700           ELSE                                                           
065800             MOVE MFS-NUM-FAELT-FEL TO RESP-IDKUNDNR-COPY-ATTR            
065900             MOVE NEJ TO INDATA-SW                                        
066000           END-IF                                                         
066100         END-IF                                                           
066200       ELSE                                                               
066300         PERFORM GA-KONTROLLERA-NEW                                       
066400       END-IF                                                             
066500       IF INDATA-FEL                                                      
066600         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
066700         MOVE NEJ TO ALLT-SW                                              
066800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
066900         PERFORM MFS-ROER-EJ-FAELT-IN                                     
067000       ELSE                                                               
067100         CONTINUE                                                         
067200       END-IF                                                             
067300     ELSE                                                                 
067400                                                                          
067500       IF REQU-INPUT = ALL '+'                                            
067600         MOVE ERR-PF11-AND-NO-DATA TO RESP-IDMSG-ERROR                    
067700         PERFORM MFS-ROER-EJ-FAELT-IN                                     
067800         PERFORM MFS-ROER-EJ-FAELT-UT                                     
067900         MOVE NEJ TO INDATA-SW                                            
068000       ELSE                                                               
068100         IF REQU-IDKUNDNR-COPY NOT = ALL '+'                              
068200           MOVE MFS-NUM-FAELT-FEL    TO RESP-IDKUNDNR-COPY-ATTR           
068300           MOVE NEJ TO  INDATA-SW                                         
068400         ELSE                                                             
068500           MOVE MFS-NUM-FAELT-RAETT  TO RESP-IDKUNDNR-COPY-ATTR           
068600         END-IF                                                           
068700                                                                          
068800         IF  REQU-BEGMT-RAD1 NOT = ALL '+'                                
068900           IF  REQU-BEGMT-RAD1 NOT = SPACE                                
069000             MOVE MFS-ALFA-FAELT-RAETT TO RESP-BEGMT-RAD1-ATTR            
069100           ELSE                                                           
069200             MOVE MFS-ALFA-FAELT-FEL   TO RESP-BEGMT-RAD1-ATTR            
069300             MOVE NEJ TO INDATA-SW                                        
069400           END-IF                                                         
069500         END-IF                                                           
069600                                                                          
069700         IF  REQU-IDPARTNER NOT = ALL '+'                                 
069800           IF  REQU-IDPARTNER NOT = SPACE                                 
069900             MOVE REQU-IDPARTNER       TO W-IDPARTNER                     
070000             PERFORM IMS-GU-WDB201-IDPARTNER                              
070100             IF SEGMENT-SAKNAS                                            
070200                MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDPARTNER-ATTR          
070300             ELSE                                                         
070400                MOVE MFS-ALFA-FAELT-FEL   TO RESP-IDPARTNER-ATTR          
070500                MOVE NEJ TO INDATA-SW                                     
070600             END-IF                                                       
070700           ELSE                                                           
070800             MOVE MFS-ALFA-FAELT-FEL   TO RESP-IDPARTNER-ATTR             
070900             MOVE NEJ TO INDATA-SW                                        
071000           END-IF                                                         
071100         END-IF                                                           
071200                                                                          
071300         IF REQU-BEGMT-RAD2 NOT = ALL '+'                                 
071400           MOVE MFS-ALFA-FAELT-RAETT TO RESP-BEGMT-RAD2-ATTR              
071500         END-IF                                                           
071600                                                                          
071700         IF REQU-IDDEALER-VIPS NOT = ALL '+'                              
071800           MOVE MFS-ALFA-FAELT-RAETT                                      
071900                                  TO RESP-IDDEALER-VIPS-ATTR              
072000         END-IF                                                           
072100                                                                          
072200         IF REQU-ADGMT-GATA NOT = ALL '+'                                 
072300           MOVE MFS-ALFA-FAELT-RAETT TO RESP-ADGMT-GATA-ATTR              
072400         END-IF                                                           
072500                                                                          
072600         IF REQU-IDLANDX2 NOT = ALL '+'                                   
072700           MOVE REQU-IDLANDX2        TO LAND-IDLANDX2                     
072800           MOVE SPACE                TO LAND-IDLANDX3                     
072900           CALL WISOLAND USING LAND-WISOLAND                              
073000           IF LAND-KDSVAR = SPACE                                         
073100              MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDLANDX2-ATTR             
073200              MOVE REQU-IDLANDX2        TO RESP-IDLANDX2                  
073300           ELSE                                                           
073400             MOVE MFS-ALFA-FAELT-FEL    TO RESP-IDLANDX2-ATTR             
073500             MOVE NEJ TO INDATA-SW                                        
073600           END-IF                                                         
073700         END-IF                                                           
073800                                                                          
073900         IF REQU-ADPOSTNR  NOT = ALL '+'                                  
074000           MOVE MFS-ALFA-FAELT-RAETT TO RESP-ADPOSTNR-ATTR                
074100         END-IF                                                           
074200                                                                          
074300         IF REQU-KDPOSTNR  NOT = ALL '+'                                  
074400            IF REQU-KDPOSTNR = 'L' OR 'R'                                 
074500               MOVE MFS-ALFA-FAELT-RAETT TO RESP-KDPOSTNR-ATTR            
074600            ELSE                                                          
074700               MOVE MFS-ALFA-FAELT-FEL   TO RESP-KDPOSTNR-ATTR            
074800               MOVE NEJ TO INDATA-SW                                      
074900            END-IF                                                        
075000         END-IF                                                           
075100                                                                          
075200         IF REQU-KDKUNDKAT NOT = ALL '+'                                  
075300           INSPECT REQU-KDKUNDKAT REPLACING LEADING SPACE BY ZERO         
075400           MOVE FUNCTION TRIM(REQU-KDKUNDKAT)                             
075500                                          TO TEXT02-KDKUNDKAT             
075600           MOVE TEXT02-KDKUNDKAT          TO REQU-KDKUNDKAT               
075700         ELSE                                                             
075800           IF WDB2-GMT-KDKUNDKAT NOT = ALL '0'                            
075900             MOVE WDB2-GMT-KDKUNDKAT      TO TEXT02-KDKUNDKAT             
076000           ELSE                                                           
076100             MOVE ZEROS                   TO TEXT02-KDKUNDKAT             
076200           END-IF                                                         
076300         END-IF                                                           
076400         IF GOOD-KDKUNDKAT                                                
076500           MOVE MFS-ALFA-FAELT-RAETT      TO RESP-KDKUNDKAT-ATTR          
076600         ELSE                                                             
076700           MOVE MFS-ALFA-FAELT-FEL        TO RESP-KDKUNDKAT-ATTR          
076800           MOVE NEJ TO INDATA-SW                                          
076900         END-IF                                                           
077000                                                                          
077100         IF REQU-ADCITY    NOT = ALL '+'                                  
077200           MOVE MFS-ALFA-FAELT-RAETT   TO RESP-ADCITY-ATTR                
077300         END-IF                                                           
077400                                                                          
077500         IF REQU-IDLONGITUDE NOT = ALL '+'                                
077600           MOVE MFS-ALFA-FAELT-RAETT   TO RESP-IDLONGITUDE-ATTR           
077700         END-IF                                                           
077800                                                                          
077900         IF REQU-ADGMT-LAND NOT = ALL '+'                                 
078000           IF  REQU-ADGMT-LAND NOT = SPACE                                
078100             MOVE MFS-ALFA-FAELT-RAETT TO RESP-ADGMT-LAND-ATTR            
078200           ELSE                                                           
078300             MOVE MFS-ALFA-FAELT-FEL   TO RESP-ADGMT-LAND-ATTR            
078400             MOVE NEJ TO INDATA-SW                                        
078500           END-IF                                                         
078600         END-IF                                                           
078700                                                                          
078800         IF REQU-IDLATITUDE NOT = ALL '+'                                 
078900           MOVE MFS-ALFA-FAELT-RAETT   TO RESP-IDLATITUDE-ATTR            
079000         END-IF                                                           
079100                                                                          
079200         IF REQU-IDTFN NOT = ALL '+'                                      
079300           MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDTFN-ATTR                   
079400         END-IF                                                           
079500                                                                          
079600         IF REQU-BETEXT NOT = ALL '+'                                     
079700           MOVE MFS-ALFA-FAELT-RAETT TO RESP-BETEXT                       
079800         END-IF                                                           
079900                                                                          
080000         IF REQU-FLRESTN NOT = ALL '+'                                    
080100           IF REQU-FLRESTN = 'Y' OR 'J' OR 'N'                            
080200             MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLRESTN-ATTR               
080300           ELSE                                                           
080400             MOVE MFS-ALFA-FAELT-FEL TO RESP-FLRESTN-ATTR                 
080500             MOVE NEJ TO INDATA-SW                                        
080600           END-IF                                                         
080700         END-IF                                                           
080800                                                                          
080900         IF REQU-FLPRELRO NOT = ALL '+'                                   
081000           IF REQU-FLPRELRO = 'Y' OR 'J' OR 'N'                           
081100             MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLPRELRO-ATTR              
081200           ELSE                                                           
081300             MOVE MFS-ALFA-FAELT-FEL TO RESP-FLPRELRO-ATTR                
081400             MOVE NEJ TO INDATA-SW                                        
081500           END-IF                                                         
081600         END-IF                                                           
081700                                                                          
081800         IF REQU-RESLATT-UPD NOT = ALL '+'                                
081900           IF REQU-RESLATT-UPD NUMERIC                                    
082000             IF REQU-RESLATT-UPD > 0 AND <= 99                            
082100               MOVE MFS-NUM-FAELT-RAETT TO RESP-RESLATT-UPD-ATTR          
082200             ELSE                                                         
082300               MOVE MFS-NUM-FAELT-FEL TO RESP-RESLATT-UPD-ATTR            
082400               MOVE NEJ TO INDATA-SW                                      
082500             END-IF                                                       
082600           ELSE                                                           
082700             MOVE MFS-NUM-FAELT-FEL TO RESP-RESLATT-UPD-ATTR              
082800             MOVE NEJ TO INDATA-SW                                        
082900           END-IF                                                         
083000         END-IF                                                           
083100                                                                          
083200         IF REQU-IDKUNDNR-H-UPD NOT = ALL '+'                             
083300           IF REQU-IDKUNDNR-H-UPD NUMERIC                                 
083400             IF REQU-IDKUNDNR-H-UPD > ZERO                                
083500               MOVE REQU-IDKUNDNR-H-UPD TO W-IDKUNDNR-COPY                
083600               PERFORM IMS-GU-WDB201-COPY                                 
083700               IF SEGMENT-FINNS                                           
083800                 IF COPY-GMT-TISTADAT > ZERO AND                          
083900                    REQU-IDKUNDNR-H-UPD NOT = TEST-IDKUNDNR               
084000                   MOVE MFS-NUM-FAELT-RAETT TO                            
084100                         RESP-IDKUNDNR-H-UPD-ATTR                         
084200                 ELSE                                                     
084300                   MOVE MFS-NUM-FAELT-FEL TO                              
084400                        RESP-IDKUNDNR-H-UPD-ATTR                          
084500                   MOVE NEJ TO INDATA-SW                                  
084600                 END-IF                                                   
084700               ELSE                                                       
084800                 MOVE MFS-NUM-FAELT-FEL                                   
084900                                 TO RESP-IDKUNDNR-H-UPD-ATTR              
085000                 MOVE NEJ TO INDATA-SW                                    
085100               END-IF                                                     
085200             ELSE                                                         
085300               MOVE MFS-NUM-FAELT-RAETT                                   
085400                                 TO RESP-IDKUNDNR-H-UPD-ATTR              
085500             END-IF                                                       
085600           ELSE                                                           
085700             MOVE MFS-NUM-FAELT-FEL                                       
085800                                 TO RESP-IDKUNDNR-H-UPD-ATTR              
085900             MOVE NEJ TO INDATA-SW                                        
086000           END-IF                                                         
086100         END-IF                                                           
086200                                                                          
086300         IF REQU-KDBEKALT-UPD NOT = ALL '+'                               
086400           IF REQU-KDBEKALT-UPD NUMERIC                                   
086500             IF REQU-KDBEKALT-UPD >= 0 AND <= 5                           
086600               MOVE MFS-NUM-FAELT-RAETT TO RESP-KDBEKALT-UPD-ATTR         
086700             ELSE                                                         
086800               MOVE MFS-NUM-FAELT-FEL   TO RESP-KDBEKALT-UPD-ATTR         
086900               MOVE NEJ TO INDATA-SW                                      
087000             END-IF                                                       
087100           ELSE                                                           
087200             MOVE MFS-NUM-FAELT-FEL     TO RESP-KDBEKALT-UPD-ATTR         
087300             MOVE NEJ TO INDATA-SW                                        
087400           END-IF                                                         
087500         END-IF                                                           
087600                                                                          
087700         IF REQU-FLOBKR-TACD-UPD NOT = ALL '+'                            
087800            IF REQU-FLOBKR-TACD-UPD = 'Y' OR 'J' OR 'N'                   
087900               MOVE MFS-ALFA-FAELT-RAETT TO                               
088000                                   RESP-FLOBKR-TACD-UPD-ATTR              
088100            ELSE                                                          
088200               MOVE MFS-ALFA-FAELT-FEL   TO                               
088300                                   RESP-FLOBKR-TACD-UPD-ATTR              
088400               MOVE NEJ TO INDATA-SW                                      
088500            END-IF                                                        
088600         END-IF                                                           
088700                                                                          
088800         IF REQU-FLDNDAP-UPD NOT = ALL '+'                                
088900            IF REQU-FLDNDAP-UPD = 'Y' OR 'J' OR 'N'                       
089000               MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLDNDAP-UPD-ATTR         
089100            ELSE                                                          
089200               MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLDNDAP-UPD-ATTR         
089300               MOVE NEJ TO INDATA-SW                                      
089400            END-IF                                                        
089500         END-IF                                                           
089600                                                                          
089700         PERFORM GB-FLORDTPO-DISTRIKT                                     
089800                                                                          
089900         IF ((REQU-FLORDTIL-KL1 = 'Y' OR 'J') AND DISTRIKT-OK) OR         
090000              REQU-FLORDTIL-KL1 = 'N'                                     
090100           MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLORDTIL-KL1-ATTR            
090200         ELSE                                                             
090300           MOVE MFS-ALFA-FAELT-FEL TO RESP-FLORDTIL-KL1-ATTR              
090400           MOVE NEJ TO INDATA-SW                                          
090500         END-IF                                                           
090600                                                                          
090700         IF ((REQU-FLORDTIL-KL2 = 'Y' OR 'J') AND DISTRIKT-OK) OR         
090800              REQU-FLORDTIL-KL2 = 'N'                                     
090900           MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLORDTIL-KL2-ATTR            
091000         ELSE                                                             
091100           MOVE MFS-ALFA-FAELT-FEL TO RESP-FLORDTIL-KL2-ATTR              
091200           MOVE NEJ TO INDATA-SW                                          
091300         END-IF                                                           
091400                                                                          
091500         IF ((REQU-FLORDTIL-KL3 = 'Y' OR 'J') AND DISTRIKT-OK) OR         
091600              REQU-FLORDTIL-KL3 = 'N'                                     
091700           MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLORDTIL-KL3-ATTR            
091800         ELSE                                                             
091900           MOVE MFS-ALFA-FAELT-FEL TO RESP-FLORDTIL-KL3-ATTR              
092000           MOVE NEJ TO INDATA-SW                                          
092100         END-IF                                                           
092200                                                                          
092300         IF ((REQU-FLORDTIL-KL4 = 'Y' OR 'J') AND DISTRIKT-OK) OR         
092400              REQU-FLORDTIL-KL4 = 'N'                                     
092500           MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLORDTIL-KL4-ATTR            
092600         ELSE                                                             
092700           MOVE MFS-ALFA-FAELT-FEL TO RESP-FLORDTIL-KL4-ATTR              
092800           MOVE NEJ TO INDATA-SW                                          
092900         END-IF                                                           
093000                                                                          
093100         IF INDATA-FEL                                                    
093200           MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                  
093300           MOVE NEJ TO ALLT-SW                                            
093400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
093500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
093600         ELSE                                                             
093700           CONTINUE                                                       
093800         END-IF                                                           
093900       END-IF                                                             
094000     END-IF                                                               
094100     .                                                                    
094200     EJECT                                                                
094300                                                                          
094400 GA-KONTROLLERA-NEW SECTION.                                              
094500                                                                          
094600     IF  REQU-BEGMT-RAD1 NOT = ALL '+'                                    
094700     AND REQU-BEGMT-RAD1 NOT = SPACE                                      
094800       MOVE MFS-ALFA-FAELT-RAETT TO RESP-BEGMT-RAD1-ATTR                  
094900     ELSE                                                                 
095000       MOVE MFS-ALFA-FAELT-FEL   TO RESP-BEGMT-RAD1-ATTR                  
095100       MOVE NEJ TO INDATA-SW                                              
095200     END-IF                                                               
095300                                                                          
095400     IF  REQU-IDPARTNER NOT = ALL '+'                                     
095500     AND REQU-IDPARTNER NOT = SPACE                                       
095600       MOVE REQU-IDPARTNER       TO W-IDPARTNER                           
095700       PERFORM IMS-GU-WDB201-IDPARTNER                                    
095800       IF SEGMENT-SAKNAS                                                  
095900          MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDPARTNER-ATTR                
096000       ELSE                                                               
096100          MOVE MFS-ALFA-FAELT-FEL   TO RESP-IDPARTNER-ATTR                
096200          MOVE NEJ TO INDATA-SW                                           
096300       END-IF                                                             
096400     END-IF                                                               
096500                                                                          
096600*    IF REQU-BEGMT-RAD2 NOT = ALL '+'                                     
096700*      MOVE MFS-ALFA-FAELT-RAETT TO RESP-BEGMT-RAD2-ATTR                  
096800*    ELSE                                                                 
096900*      MOVE MFS-ALFA-FAELT-FEL   TO RESP-BEGMT-RAD2-ATTR                  
097000*      MOVE NEJ TO INDATA-SW                                              
097100*    END-IF                                                               
097200                                                                          
097300*    IF REQU-ADGMT-GATA NOT = ALL '+'                                     
097400*      MOVE MFS-ALFA-FAELT-RAETT TO RESP-ADGMT-GATA-ATTR                  
097500*    ELSE                                                                 
097600*      MOVE MFS-ALFA-FAELT-FEL   TO RESP-ADGMT-GATA-ATTR                  
097700*      MOVE NEJ TO INDATA-SW                                              
097800*    END-IF                                                               
097900                                                                          
098000*    IF REQU-ADGMT-PADR NOT = ALL '+'                                     
098100*      MOVE MFS-ALFA-FAELT-RAETT TO RESP-ADGMT-PADR-ATTR                  
098200*    ELSE                                                                 
098300*      MOVE MFS-ALFA-FAELT-FEL   TO RESP-ADGMT-PADR-ATTR                  
098400*      MOVE NEJ TO INDATA-SW                                              
098500*    END-IF                                                               
098600                                                                          
098700     IF  REQU-IDLANDX2 NOT = ALL '+'                                      
098800     AND REQU-IDLANDX2 NOT = SPACE                                        
098900       MOVE REQU-IDLANDX2          TO LAND-IDLANDX2                       
099000       MOVE SPACE                  TO LAND-IDLANDX3                       
099100       CALL WISOLAND USING LAND-WISOLAND                                  
099200       IF LAND-KDSVAR = SPACE                                             
099300         MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDLANDX2-ATTR                  
099400       ELSE                                                               
099500         MOVE MFS-ALFA-FAELT-FEL   TO RESP-IDLANDX2-ATTR                  
099600         MOVE NEJ TO INDATA-SW                                            
099700       END-IF                                                             
099800     ELSE                                                                 
099900       MOVE MFS-ALFA-FAELT-FEL     TO RESP-IDLANDX2-ATTR                  
100000       MOVE NEJ TO INDATA-SW                                              
100100     END-IF                                                               
100200                                                                          
100300     IF  REQU-KDKUNDKAT NOT = ALL '+'                                     
100400     AND REQU-KDKUNDKAT NOT = SPACE                                       
100500       INSPECT REQU-KDKUNDKAT REPLACING LEADING SPACE BY ZERO             
100600       MOVE FUNCTION TRIM(REQU-KDKUNDKAT)                                 
100700                                  TO TEXT02-KDKUNDKAT                     
100800       MOVE TEXT02-KDKUNDKAT      TO REQU-KDKUNDKAT                       
100900       IF GOOD-KDKUNDKAT                                                  
101000         MOVE MFS-ALFA-FAELT-RAETT TO RESP-KDKUNDKAT-ATTR                 
101100       ELSE                                                               
101200         MOVE MFS-ALFA-FAELT-FEL   TO RESP-KDKUNDKAT-ATTR                 
101300         MOVE NEJ TO INDATA-SW                                            
101400       END-IF                                                             
101500     ELSE                                                                 
101600       MOVE MFS-ALFA-FAELT-FEL     TO RESP-KDKUNDKAT-ATTR                 
101700       MOVE NEJ TO INDATA-SW                                              
101800     END-IF                                                               
101900                                                                          
102000     IF  REQU-ADGMT-LAND NOT = ALL '+'                                    
102100     AND REQU-ADGMT-LAND NOT = SPACE                                      
102200       MOVE MFS-ALFA-FAELT-RAETT TO RESP-ADGMT-LAND-ATTR                  
102300     ELSE                                                                 
102400       MOVE MFS-ALFA-FAELT-FEL   TO RESP-ADGMT-LAND-ATTR                  
102500       MOVE NEJ TO INDATA-SW                                              
102600     END-IF                                                               
102700                                                                          
102800     IF REQU-FLRESTN NOT = ALL '+'                                        
102900       IF REQU-FLRESTN = 'Y' OR 'J' OR 'N'                                
103000         MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLRESTN-ATTR                   
103100       ELSE                                                               
103200         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLRESTN-ATTR                     
103300         MOVE NEJ TO INDATA-SW                                            
103400       END-IF                                                             
103500     ELSE                                                                 
103600       MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLRESTN-ATTR                     
103700       MOVE NEJ TO INDATA-SW                                              
103800     END-IF                                                               
103900                                                                          
104000     IF REQU-FLPRELRO NOT = ALL '+'                                       
104100       IF REQU-FLPRELRO = 'Y' OR 'J' OR 'N'                               
104200         MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLPRELRO-ATTR                  
104300       ELSE                                                               
104400         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLPRELRO-ATTR                    
104500         MOVE NEJ TO INDATA-SW                                            
104600       END-IF                                                             
104700     ELSE                                                                 
104800       MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLPRELRO-ATTR                    
104900       MOVE NEJ TO INDATA-SW                                              
105000     END-IF                                                               
105100                                                                          
105200     IF REQU-RESLATT-UPD NOT = ALL '+'                                    
105300       IF REQU-RESLATT-UPD NUMERIC                                        
105400         IF REQU-RESLATT-UPD > 0 AND <= 99                                
105500           MOVE MFS-NUM-FAELT-RAETT TO RESP-RESLATT-UPD-ATTR              
105600         ELSE                                                             
105700           MOVE MFS-NUM-FAELT-FEL TO RESP-RESLATT-UPD-ATTR                
105800           MOVE NEJ TO INDATA-SW                                          
105900         END-IF                                                           
106000       ELSE                                                               
106100         MOVE MFS-NUM-FAELT-FEL TO RESP-RESLATT-UPD-ATTR                  
106200         MOVE NEJ TO INDATA-SW                                            
106300       END-IF                                                             
106400     ELSE                                                                 
106500       MOVE MFS-NUM-FAELT-FEL   TO RESP-RESLATT-UPD-ATTR                  
106600       MOVE NEJ TO INDATA-SW                                              
106700     END-IF                                                               
106800                                                                          
106900     IF REQU-IDKUNDNR-H-UPD NOT = ALL '+'                                 
107000       IF REQU-IDKUNDNR-H-UPD NUMERIC                                     
107100         IF REQU-IDKUNDNR-H-UPD > ZERO                                    
107200           MOVE REQU-IDKUNDNR-H-UPD TO W-IDKUNDNR-COPY                    
107300           PERFORM IMS-GU-WDB201-COPY                                     
107400           IF SEGMENT-FINNS                                               
107500             IF COPY-GMT-TISTADAT > ZERO                                  
107600               IF COPY-GMT-TISTODAT > ZERO                                
107700                 MOVE COPY-GMT-TISTODAT   TO TMP1-YYMMDD                  
107800                 MOVE COPY-GMT-TISTADAT   TO TMP2-YYMMDD                  
107900                 PERFORM WY2000P1                                         
108000                 IF TMP1-YYMMDD > TMP2-YYMMDD                             
108100                   MOVE MFS-NUM-FAELT-RAETT TO                            
108200                        RESP-IDKUNDNR-H-UPD-ATTR                          
108300                 ELSE                                                     
108400                   MOVE MFS-NUM-FAELT-FEL TO                              
108500                        RESP-IDKUNDNR-H-UPD-ATTR                          
108600                   MOVE NEJ TO INDATA-SW                                  
108700                 END-IF                                                   
108800               ELSE                                                       
108900                 MOVE MFS-NUM-FAELT-RAETT TO                              
109000                      RESP-IDKUNDNR-H-UPD-ATTR                            
109100                 MOVE NEJ TO INDATA-SW                                    
109200               END-IF                                                     
109300             ELSE                                                         
109400               MOVE MFS-NUM-FAELT-RAETT TO                                
109500                    RESP-IDKUNDNR-H-UPD-ATTR                              
109600               MOVE NEJ TO INDATA-SW                                      
109700             END-IF                                                       
109800           ELSE                                                           
109900             MOVE MFS-NUM-FAELT-FEL TO RESP-IDKUNDNR-H-UPD-ATTR           
110000             MOVE NEJ TO INDATA-SW                                        
110100           END-IF                                                         
110200         ELSE                                                             
110300           MOVE MFS-NUM-FAELT-RAETT TO RESP-IDKUNDNR-H-UPD-ATTR           
110400         END-IF                                                           
110500       ELSE                                                               
110600         MOVE MFS-NUM-FAELT-FEL TO RESP-IDKUNDNR-H-UPD-ATTR               
110700         MOVE NEJ TO INDATA-SW                                            
110800       END-IF                                                             
110900     END-IF                                                               
111000                                                                          
111100     IF REQU-KDBEKALT-UPD NOT = ALL '+'                                   
111200       IF REQU-KDBEKALT-UPD NUMERIC                                       
111300         IF REQU-KDBEKALT-UPD >= 0 AND <= 5                               
111400           MOVE MFS-NUM-FAELT-RAETT TO RESP-KDBEKALT-UPD-ATTR             
111500         ELSE                                                             
111600           MOVE MFS-NUM-FAELT-FEL   TO RESP-KDBEKALT-UPD-ATTR             
111700           MOVE NEJ TO INDATA-SW                                          
111800         END-IF                                                           
111900       ELSE                                                               
112000         MOVE MFS-NUM-FAELT-FEL     TO RESP-KDBEKALT-UPD-ATTR             
112100         MOVE NEJ TO INDATA-SW                                            
112200       END-IF                                                             
112300     ELSE                                                                 
112400       MOVE MFS-NUM-FAELT-FEL       TO RESP-KDBEKALT-UPD-ATTR             
112500       MOVE NEJ TO INDATA-SW                                              
112600     END-IF                                                               
112700                                                                          
112800     IF REQU-FLOBKR-TACD-UPD NOT = ALL '+'                                
112900        IF REQU-FLOBKR-TACD-UPD = 'Y' OR 'J' OR 'N'                       
113000           MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLOBKR-TACD-UPD-ATTR         
113100        ELSE                                                              
113200           MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLOBKR-TACD-UPD-ATTR         
113300           MOVE NEJ TO INDATA-SW                                          
113400        END-IF                                                            
113500     ELSE                                                                 
113600       MOVE MFS-ALFA-FAELT-FEL      TO RESP-FLOBKR-TACD-UPD-ATTR          
113700       MOVE NEJ TO INDATA-SW                                              
113800     END-IF                                                               
113900                                                                          
114000     IF REQU-FLDNDAP-UPD NOT = ALL '+'                                    
114100        IF REQU-FLDNDAP-UPD = 'Y' OR 'J' OR 'N'                           
114200           MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLDNDAP-UPD-ATTR             
114300        ELSE                                                              
114400           MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLDNDAP-UPD-ATTR             
114500           MOVE NEJ TO INDATA-SW                                          
114600        END-IF                                                            
114700     ELSE                                                                 
114800       MOVE MFS-ALFA-FAELT-FEL      TO RESP-FLDNDAP-UPD-ATTR              
114900       MOVE NEJ TO INDATA-SW                                              
115000     END-IF                                                               
115100                                                                          
115200     IF REQU-FLORDTIL-KL1 NOT = ALL '+'                                   
115300       IF REQU-FLORDTIL-KL1 = 'Y' OR 'J' OR 'N'                           
115400         MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLORDTIL-KL1-ATTR              
115500       ELSE                                                               
115600         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLORDTIL-KL1-ATTR                
115700         MOVE NEJ TO INDATA-SW                                            
115800       END-IF                                                             
115900     ELSE                                                                 
116000       MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLORDTIL-KL1-ATTR                
116100       MOVE NEJ TO INDATA-SW                                              
116200     END-IF                                                               
116300                                                                          
116400     IF REQU-FLORDTIL-KL2 NOT = ALL '+'                                   
116500       IF REQU-FLORDTIL-KL2 = 'Y' OR 'J' OR 'N'                           
116600         MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLORDTIL-KL2-ATTR              
116700       ELSE                                                               
116800         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLORDTIL-KL2-ATTR                
116900         MOVE NEJ TO INDATA-SW                                            
117000       END-IF                                                             
117100     ELSE                                                                 
117200       MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLORDTIL-KL2-ATTR                
117300       MOVE NEJ TO INDATA-SW                                              
117400     END-IF                                                               
117500                                                                          
117600     IF REQU-FLORDTIL-KL3 NOT = ALL '+'                                   
117700       IF REQU-FLORDTIL-KL3 = 'Y' OR 'J' OR 'N'                           
117800         MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLORDTIL-KL3-ATTR              
117900       ELSE                                                               
118000         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLORDTIL-KL3-ATTR                
118100         MOVE NEJ TO INDATA-SW                                            
118200       END-IF                                                             
118300     ELSE                                                                 
118400       MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLORDTIL-KL3-ATTR                
118500       MOVE NEJ TO INDATA-SW                                              
118600     END-IF                                                               
118700                                                                          
118800     IF REQU-FLORDTIL-KL4 NOT = ALL '+'                                   
118900       IF REQU-FLORDTIL-KL4 = 'Y' OR 'J' OR 'N'                           
119000         MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLORDTIL-KL4-ATTR              
119100       ELSE                                                               
119200         MOVE MFS-ALFA-FAELT-FEL TO RESP-FLORDTIL-KL4-ATTR                
119300         MOVE NEJ TO INDATA-SW                                            
119400       END-IF                                                             
119500     ELSE                                                                 
119600       MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLORDTIL-KL4-ATTR                
119700       MOVE NEJ TO INDATA-SW                                              
119800     END-IF                                                               
119900                                                                          
120000     .                                                                    
120100     EJECT                                                                
120200                                                                          
120300 GB-FLORDTPO-DISTRIKT SECTION.                                            
120400                                                                          
120500     MOVE WS-IDDISTR TO TEST-IDDISTR                                      
120600                                                                          
120700     IF DIST18-SKROT             OR                                       
120800        DIST18-SCRAP-NDC         OR                                       
120900        DIST19-SATS              OR                                       
121000        DIST35-RETUR             OR                                       
121100        DIST35-NA-CDC-RETURN     OR                                       
121200        DIST35-CN-NDC-RETURNS    OR                                       
121300        DIST35-CDC-RETURNS-NON-VCC OR                                     
121400        DIST35-REFILL            OR                                       
121500        DIST35-REFILL-INOM-NDC   OR                                       
121600        DIST35-NONVCC-REFILL     OR                                       
121700        DIST35-NONVCC-VCC-TRANSFER OR                                     
121800        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
121900        DIST35-REFILL-NA-JAP     OR                                       
122000        DIST35-NA-TRANSFER       OR                                       
122100        DIST35-PACIFIC-TRANSFER  OR                                       
122200        DIST35-REFILL-INOM-JP    OR                                       
122300        DIST35-CN-TRANSFER       OR                                       
122400        DIST35-ST-CDC            OR                                       
122500        DIST35-NL-SITTARD-OBJEKT                                          
122600                                                                          
122700        MOVE NEJ TO SW-DISTRIKT-OK                                        
122800     ELSE                                                                 
122900        MOVE JA  TO SW-DISTRIKT-OK                                        
123000     END-IF                                                               
123100     .                                                                    
123200     EJECT                                                                
123300                                                                          
123400 H-UPPDATERA SECTION.                                                     
123500                                                                          
123600     PERFORM IMS-GHU-WDB201                                               
123700     IF SEGMENT-SAKNAS                                                    
123800       IF REQU-IDKUNDNR-COPY = ALL '+'                                    
123900         PERFORM HA-ISRT-CUSTOMER                                         
124000       ELSE                                                               
124100         PERFORM HB-COPY-CUSTOMER                                         
124200       END-IF                                                             
124300     ELSE                                                                 
124400                                                                          
124500       IF REQU-BEGMT-RAD1 NOT = ALL '+'                                   
124600         MOVE REQU-BEGMT-RAD1 TO WDB2-GMT-BEGMT-RAD1                      
124700         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-BEGMT-RAD1-ATTR               
124800       ELSE                                                               
124900         MOVE ALL-PLUS TO RESP-BEGMT-RAD1-ATTR                            
125000       END-IF                                                             
125100                                                                          
125200       IF REQU-IDPARTNER NOT = ALL '+'                                    
125300         MOVE REQU-IDPARTNER  TO WDB2-GMT-IDPARTNER                       
125400         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-IDPARTNER-ATTR                
125500       ELSE                                                               
125600         MOVE ALL-PLUS TO RESP-IDPARTNER-ATTR                             
125700       END-IF                                                             
125800                                                                          
125900       IF REQU-BEGMT-RAD2 NOT = ALL '+'                                   
126000         MOVE REQU-BEGMT-RAD2 TO WDB2-GMT-BEGMT-RAD2                      
126100         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-BEGMT-RAD2-ATTR               
126200       ELSE                                                               
126300         MOVE ALL-PLUS TO RESP-BEGMT-RAD2-ATTR                            
126400       END-IF                                                             
126500                                                                          
126600       IF REQU-IDDEALER-VIPS NOT = ALL '+'                                
126700         MOVE REQU-IDDEALER-VIPS    TO WDB2-GMT-IDDEALER-VIPS             
126800         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-IDDEALER-VIPS-ATTR            
126900       ELSE                                                               
127000         MOVE ALL-PLUS TO RESP-IDDEALER-VIPS-ATTR                         
127100       END-IF                                                             
127200                                                                          
127300       IF REQU-ADGMT-GATA NOT = ALL '+'                                   
127400         MOVE REQU-ADGMT-GATA TO WDB2-GMT-ADGMT-GATA                      
127500         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-ADGMT-GATA-ATTR               
127600       ELSE                                                               
127700         MOVE ALL-PLUS TO RESP-ADGMT-GATA-ATTR                            
127800       END-IF                                                             
127900                                                                          
128000       IF REQU-IDLANDX2 NOT = ALL '+'                                     
128100         MOVE REQU-IDLANDX2         TO WDB2-GMT-IDLANDX2                  
128200         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-IDLANDX2-ATTR                 
128300       ELSE                                                               
128400         MOVE ALL-PLUS              TO RESP-IDLANDX2-ATTR                 
128500       END-IF                                                             
128600                                                                          
128700       IF REQU-KDPOSTNR  NOT = ALL '+'                                    
128800         IF REQU-KDPOSTNR NOT = WDB2-GMT-KDPOSTNR                         
128900            IF WDB2-GMT-KDPOSTNR = 'L'                                    
129000*  VI BYTER FRÅN 'L' TILL 'R'                                             
129100               MOVE WDB2-GMT-ADPOSTNR IN WDB2-GMT-ADPOST-PNRORT           
129200                             TO WS-ADPOSTNR                               
129300               MOVE WDB2-GMT-ADCITY   IN WDB2-GMT-ADPOST-PNRORT           
129400                             TO WS-ADCITY                                 
129500               MOVE WS-ADPOSTNR TO WDB2-GMT-ADPOSTNR                      
129600                              IN WDB2-GMT-ADPOST-ORTPNR                   
129700               MOVE WS-ADCITY   TO WDB2-GMT-ADCITY                        
129800                              IN WDB2-GMT-ADPOST-ORTPNR                   
129900            ELSE                                                          
130000*  VI BYTER FRÅN 'R' TILL 'L'                                             
130100               MOVE WDB2-GMT-ADPOSTNR IN WDB2-GMT-ADPOST-ORTPNR           
130200                             TO WS-ADPOSTNR                               
130300               MOVE WDB2-GMT-ADCITY   IN WDB2-GMT-ADPOST-ORTPNR           
130400                             TO WS-ADCITY                                 
130500               MOVE WS-ADPOSTNR TO WDB2-GMT-ADPOSTNR                      
130600                              IN WDB2-GMT-ADPOST-PNRORT                   
130700               MOVE WS-ADCITY   TO WDB2-GMT-ADCITY                        
130800                              IN WDB2-GMT-ADPOST-PNRORT                   
130900            END-IF                                                        
131000         END-IF                                                           
131100         MOVE REQU-KDPOSTNR  TO WDB2-GMT-KDPOSTNR                         
131200         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-KDPOSTNR-ATTR                 
131300       ELSE                                                               
131400         MOVE ALL-PLUS TO RESP-KDPOSTNR-ATTR                              
131500       END-IF                                                             
131600                                                                          
131700       IF REQU-ADPOSTNR  NOT = ALL '+'                                    
131800         IF WDB2-GMT-KDPOSTNR = 'L'                                       
131900            MOVE REQU-ADPOSTNR TO WDB2-GMT-ADPOSTNR                       
132000                              IN WDB2-GMT-ADPOST-PNRORT                   
132100         ELSE                                                             
132200            MOVE REQU-ADPOSTNR TO WDB2-GMT-ADPOSTNR                       
132300                              IN WDB2-GMT-ADPOST-ORTPNR                   
132400         END-IF                                                           
132500         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-ADPOSTNR-ATTR                 
132600       ELSE                                                               
132700         MOVE ALL-PLUS TO RESP-ADPOSTNR-ATTR                              
132800       END-IF                                                             
132900                                                                          
133000       IF REQU-KDKUNDKAT  NOT = ALL '+'                                   
133100         MOVE REQU-KDKUNDKAT  TO WDB2-GMT-KDKUNDKAT                       
133200         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-KDKUNDKAT-ATTR                
133300       ELSE                                                               
133400         MOVE ALL-PLUS TO RESP-KDKUNDKAT-ATTR                             
133500       END-IF                                                             
133600                                                                          
133700       IF REQU-ADCITY    NOT = ALL '+'                                    
133800         IF WDB2-GMT-KDPOSTNR = 'L'                                       
133900            MOVE REQU-ADCITY  TO WDB2-GMT-ADCITY                          
134000                              IN WDB2-GMT-ADPOST-PNRORT                   
134100         ELSE                                                             
134200            MOVE REQU-ADCITY  TO WDB2-GMT-ADCITY                          
134300                              IN WDB2-GMT-ADPOST-ORTPNR                   
134400         END-IF                                                           
134500         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-ADCITY-ATTR                   
134600       ELSE                                                               
134700         MOVE ALL-PLUS TO RESP-ADCITY-ATTR                                
134800       END-IF                                                             
134900                                                                          
135000       IF REQU-ADCITY    NOT = ALL '+'                                    
135100         IF WDB2-GMT-KDPOSTNR = 'L'                                       
135200            MOVE REQU-ADCITY  TO WDB2-GMT-ADCITY                          
135300                              IN WDB2-GMT-ADPOST-PNRORT                   
135400         ELSE                                                             
135500            MOVE REQU-ADCITY  TO WDB2-GMT-ADCITY                          
135600                              IN WDB2-GMT-ADPOST-ORTPNR                   
135700         END-IF                                                           
135800         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-ADCITY-ATTR                   
135900       ELSE                                                               
136000         MOVE ALL-PLUS TO RESP-ADCITY-ATTR                                
136100       END-IF                                                             
136200                                                                          
136300       IF REQU-IDLONGITUDE NOT = ALL '+'                                  
136400         MOVE REQU-IDLONGITUDE TO WDB2-GMT-IDLONGITUDE                    
136500         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-IDLONGITUDE-ATTR              
136600       ELSE                                                               
136700         MOVE ALL-PLUS TO RESP-IDLONGITUDE-ATTR                           
136800       END-IF                                                             
136900                                                                          
137000       IF REQU-ADGMT-LAND NOT = ALL '+'                                   
137100         MOVE REQU-ADGMT-LAND TO WDB2-GMT-ADGMT-LAND                      
137200         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-ADGMT-LAND-ATTR               
137300       ELSE                                                               
137400         MOVE ALL-PLUS TO RESP-ADGMT-LAND-ATTR                            
137500       END-IF                                                             
137600                                                                          
137700       IF REQU-IDLATITUDE  NOT = ALL '+'                                  
137800         MOVE REQU-IDLATITUDE  TO WDB2-GMT-IDLATITUDE                     
137900         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-IDLATITUDE-ATTR               
138000       ELSE                                                               
138100         MOVE ALL-PLUS TO RESP-IDLATITUDE-ATTR                            
138200       END-IF                                                             
138300                                                                          
138400       IF REQU-IDTFN NOT = ALL '+'                                        
138500         MOVE REQU-IDTFN TO WDB2-GMT-IDTFN                                
138600         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-IDTFN-ATTR                    
138700       ELSE                                                               
138800         MOVE ALL-PLUS TO RESP-IDTFN-ATTR                                 
138900       END-IF                                                             
139000                                                                          
139100       IF REQU-BETEXT NOT = ALL '+'                                       
139200         MOVE REQU-BETEXT  TO WDB2-GMT-BETEXT                             
139300         MOVE MFS-ADD-LYS-UPP-FAELT TO RESP-BETEXT-ATTR                   
139400       ELSE                                                               
139500         MOVE ALL-PLUS TO RESP-BETEXT-ATTR                                
139600       END-IF                                                             
139700                                                                          
139800       IF REQU-FLRESTN NOT = ALL '+'                                      
139900         IF REQU-FLRESTN = YES                                            
140000           MOVE JA              TO WDB2-GMT-FLRESTN                       
140100         ELSE                                                             
140200           MOVE REQU-FLRESTN    TO WDB2-GMT-FLRESTN                       
140300         END-IF                                                           
140400       END-IF                                                             
140500                                                                          
140600       IF REQU-FLPRELRO NOT = ALL '+'                                     
140700         IF REQU-FLPRELRO = YES                                           
140800           MOVE JA              TO WDB2-GMT-FLPRELRO                      
140900         ELSE                                                             
141000           MOVE REQU-FLPRELRO   TO WDB2-GMT-FLPRELRO                      
141100         END-IF                                                           
141200       END-IF                                                             
141300                                                                          
141400       IF REQU-RESLATT-UPD NOT = ALL '+'                                  
141500         MOVE REQU-RESLATT-UPD TO WDB2-GMT-RESLATT                        
141600       END-IF                                                             
141700                                                                          
141800       IF REQU-IDKUNDNR-H-UPD NOT = ALL '+'                               
141900         MOVE REQU-IDKUNDNR-H-UPD TO WDB2-GMT-IDKUNDNR-HEAD               
142000       END-IF                                                             
142100                                                                          
142200       IF REQU-KDBEKALT-UPD NOT = ALL '+'                                 
142300         MOVE REQU-KDBEKALT-UPD TO WDB2-GMT-KDBEKALT                      
142400       END-IF                                                             
142500                                                                          
142600       IF REQU-FLOBKR-TACD-UPD NOT = ALL '+'                              
142700         IF REQU-FLOBKR-TACD-UPD = YES                                    
142800           MOVE JA              TO WDB2-GMT-FLOBKR-TACD                   
142900         ELSE                                                             
143000           MOVE REQU-FLOBKR-TACD-UPD TO WDB2-GMT-FLOBKR-TACD              
143100         END-IF                                                           
143200       END-IF                                                             
143300                                                                          
143400       IF REQU-FLDNDAP-UPD NOT = ALL '+'                                  
143500         IF REQU-FLDNDAP-UPD = YES                                        
143600           MOVE JA              TO WDB2-GMT-FLDNDAP                       
143700         ELSE                                                             
143800           MOVE REQU-FLDNDAP-UPD TO WDB2-GMT-FLDNDAP                      
143900         END-IF                                                           
144000       END-IF                                                             
144100                                                                          
144200       IF REQU-FLORDTIL-KL1 NOT = ALL '+'                                 
144300         IF REQU-FLORDTIL-KL1 = YES                                       
144400           MOVE JA              TO WDB2-GMT-FLORDTIL-KL1                  
144500         ELSE                                                             
144600           MOVE REQU-FLORDTIL-KL1 TO WDB2-GMT-FLORDTIL-KL1                
144700         END-IF                                                           
144800       END-IF                                                             
144900                                                                          
145000       IF REQU-FLORDTIL-KL2 NOT = ALL '+'                                 
145100         IF REQU-FLORDTIL-KL2 = YES                                       
145200           MOVE JA              TO WDB2-GMT-FLORDTIL-KL2                  
145300         ELSE                                                             
145400           MOVE REQU-FLORDTIL-KL2 TO WDB2-GMT-FLORDTIL-KL2                
145500         END-IF                                                           
145600       END-IF                                                             
145700                                                                          
145800       IF REQU-FLORDTIL-KL3 NOT = ALL '+'                                 
145900         IF REQU-FLORDTIL-KL3 = YES                                       
146000           MOVE JA              TO WDB2-GMT-FLORDTIL-KL3                  
146100         ELSE                                                             
146200           MOVE REQU-FLORDTIL-KL3 TO WDB2-GMT-FLORDTIL-KL3                
146300         END-IF                                                           
146400       END-IF                                                             
146500                                                                          
146600       IF REQU-FLORDTIL-KL4 NOT = ALL '+'                                 
146700         IF REQU-FLORDTIL-KL4 = YES                                       
146800           MOVE JA              TO WDB2-GMT-FLORDTIL-KL4                  
146900         ELSE                                                             
147000           MOVE REQU-FLORDTIL-KL4 TO WDB2-GMT-FLORDTIL-KL4                
147100         END-IF                                                           
147200       END-IF                                                             
147300                                                                          
147400       PERFORM IMS-REPL-WDB201                                            
147500                                                                          
147600       MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                            
147700       PERFORM MFS-FORM-ATTR                                              
147800       PERFORM MFS-RENSA-FAELT-IN                                         
147900* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
148000     END-IF                                                               
148100     .                                                                    
148200     EJECT                                                                
148300                                                                          
148400 HA-ISRT-CUSTOMER SECTION.                                                
148500                                                                          
148600     MOVE WS-IDDISTR              TO WDB2-GMT-IDDISTR                     
148700     MOVE WS-IDKUNDNR             TO WDB2-GMT-IDKUNDNR                    
148800                                     W-IDKUNDNR                           
148900                                                                          
149000     MOVE REQU-BEGMT-RAD1         TO WDB2-GMT-BEGMT-RAD1                  
149100                                                                          
149200     IF  REQU-IDPARTNER NOT = ALL '+'                                     
149300         MOVE REQU-IDPARTNER       TO W-IDPARTNER                         
149400         PERFORM IMS-GU-WDB201-IDPARTNER                                  
149500         IF SEGMENT-SAKNAS                                                
149600            MOVE REQU-IDPARTNER  TO WDB2-GMT-IDPARTNER                    
149700         ELSE                                                             
149800            MOVE SPACES          TO WDB2-GMT-IDPARTNER                    
149900         END-IF                                                           
150000     ELSE                                                                 
150100         MOVE SPACE              TO WDB2-GMT-IDPARTNER                    
150200     END-IF                                                               
150300                                                                          
150400     IF  REQU-BEGMT-RAD2 NOT = ALL '+'                                    
150500         MOVE REQU-BEGMT-RAD2     TO WDB2-GMT-BEGMT-RAD2                  
150600     ELSE                                                                 
150700         MOVE SPACE               TO WDB2-GMT-BEGMT-RAD2                  
150800     END-IF                                                               
150900                                                                          
151000     IF  REQU-IDDEALER-VIPS NOT = ALL '+'                                 
151100         MOVE REQU-IDDEALER-VIPS                                          
151200                                  TO WDB2-GMT-IDDEALER-VIPS               
151300     ELSE                                                                 
151400         MOVE SPACE               TO WDB2-GMT-IDDEALER-VIPS               
151500     END-IF                                                               
151600                                                                          
151700     IF  REQU-ADGMT-GATA NOT = ALL '+'                                    
151800         MOVE REQU-ADGMT-GATA     TO WDB2-GMT-ADGMT-GATA                  
151900     ELSE                                                                 
152000         MOVE SPACE               TO WDB2-GMT-ADGMT-GATA                  
152100     END-IF                                                               
152200                                                                          
152300     IF  REQU-IDLANDX2 NOT = ALL '+'                                      
152400         MOVE REQU-IDLANDX2       TO WDB2-GMT-IDLANDX2                    
152500     ELSE                                                                 
152600         MOVE SPACE               TO WDB2-GMT-IDLANDX2                    
152700     END-IF                                                               
152800                                                                          
152900                                                                          
153000     IF  REQU-KDPOSTNR  NOT = ALL '+'                                     
153100         MOVE REQU-KDPOSTNR       TO WDB2-GMT-KDPOSTNR                    
153200     ELSE                                                                 
153300         MOVE 'L'                 TO WDB2-GMT-KDPOSTNR                    
153400     END-IF                                                               
153500                                                                          
153600     IF  REQU-ADPOSTNR  NOT = ALL '+'                                     
153700         IF WDB2-GMT-KDPOSTNR = 'L'                                       
153800            MOVE REQU-ADPOSTNR    TO WDB2-GMT-ADPOSTNR                    
153900                                  IN WDB2-GMT-ADPOST-PNRORT               
154000         ELSE                                                             
154100            MOVE REQU-ADPOSTNR    TO WDB2-GMT-ADPOSTNR                    
154200                                  IN WDB2-GMT-ADPOST-ORTPNR               
154300         END-IF                                                           
154400     ELSE                                                                 
154500         IF WDB2-GMT-KDPOSTNR = 'L'                                       
154600            MOVE SPACE            TO WDB2-GMT-ADPOSTNR                    
154700                                  IN WDB2-GMT-ADPOST-PNRORT               
154800         ELSE                                                             
154900            MOVE SPACE            TO WDB2-GMT-ADPOSTNR                    
155000                                  IN WDB2-GMT-ADPOST-ORTPNR               
155100         END-IF                                                           
155200     END-IF                                                               
155300                                                                          
155400     MOVE REQU-KDKUNDKAT          TO WDB2-GMT-KDKUNDKAT                   
155500                                                                          
155600     IF  REQU-ADCITY    NOT = ALL '+'                                     
155700         IF WDB2-GMT-KDPOSTNR = 'L'                                       
155800            MOVE REQU-ADCITY      TO WDB2-GMT-ADCITY                      
155900                                  IN WDB2-GMT-ADPOST-PNRORT               
156000         ELSE                                                             
156100            MOVE REQU-ADCITY      TO WDB2-GMT-ADCITY                      
156200                                  IN WDB2-GMT-ADPOST-ORTPNR               
156300         END-IF                                                           
156400     ELSE                                                                 
156500         IF WDB2-GMT-KDPOSTNR = 'L'                                       
156600           MOVE SPACE             TO WDB2-GMT-ADCITY                      
156700                                  IN WDB2-GMT-ADPOST-PNRORT               
156800         ELSE                                                             
156900           MOVE SPACE             TO WDB2-GMT-ADCITY                      
157000                                  IN WDB2-GMT-ADPOST-ORTPNR               
157100         END-IF                                                           
157200     END-IF                                                               
157300                                                                          
157400     MOVE SPACE                   TO WDB2-GMT-IDLONGITUDE                 
157500     MOVE REQU-ADGMT-LAND         TO WDB2-GMT-ADGMT-LAND                  
157600     MOVE SPACE                   TO WDB2-GMT-IDLATITUDE                  
157700                                                                          
157800     IF  REQU-IDTFN NOT = ALL '+'                                         
157900         MOVE REQU-IDTFN          TO WDB2-GMT-IDTFN                       
158000     ELSE                                                                 
158100         MOVE SPACE               TO WDB2-GMT-IDTFN                       
158200     END-IF                                                               
158300                                                                          
158400     IF  REQU-BETEXT NOT = ALL '+'                                        
158500         MOVE REQU-BETEXT         TO WDB2-GMT-BETEXT                      
158600     ELSE                                                                 
158700         MOVE SPACE               TO WDB2-GMT-BETEXT                      
158800     END-IF                                                               
158900                                                                          
159000     MOVE SPACE                 TO WDB2-GMT-FLCOD                         
159100     MOVE SPACE                 TO WDB2-GMT-FLFAKURS                      
159200     MOVE SPACE                 TO WDB2-GMT-FLFAKVKT                      
159300     MOVE SPACE                 TO WDB2-GMT-FLOBKR-TACD                   
159400     MOVE SPACE                 TO WDB2-GMT-FLDNDAP                       
159500     MOVE NEJ                   TO WDB2-GMT-FLRETFG                       
159600     MOVE NEJ                   TO WDB2-GMT-FLSWCONS                      
159700     MOVE SPACE                 TO WDB2-GMT-FLOKFAK-G                     
159800     MOVE SPACE                 TO WDB2-GMT-FLOKFAK-K                     
159900     MOVE SPACE                 TO WDB2-GMT-FLOKFAK-N                     
160000     MOVE SPACE                 TO WDB2-GMT-FLOKFAK-R                     
160100     MOVE SPACE                 TO WDB2-GMT-FLPRELRO                      
160200     MOVE SPACE                 TO WDB2-GMT-FLRESTN                       
160300     MOVE SPACE                 TO WDB2-GMT-IDPARTNR                      
160400     MOVE WS-IDDISTR TO TEST-IDDISTR                                      
160500     IF DIST35-NONVCC-REFILL                                              
160600     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
160700     OR DIST35-NONVCC-VCC-TRANSFER                                        
160800     OR W-IDKUNDNR = ZERO                                                 
160900     OR DIST07-NA-CUSTOMERS                                               
161000        MOVE CHK-WC-IDFTG-PV    TO WDB2-GMT-IDFTG                         
161100     ELSE                                                                 
161200        MOVE ZERO               TO WDB2-GMT-IDFTG                         
161300     END-IF                                                               
161400                                                                          
161500     MOVE +1                    TO IDEX                                   
161600     PERFORM UNTIL IDEX > IX-DCCLEAR-MAX                                  
161700       MOVE SPACE               TO WDB2-GMT-IDDC-BULK (IDEX)              
161800                                   WDB2-GMT-IDDC-DAY (IDEX)               
161900                                   WDB2-GMT-IDDC-VOR (IDEX)               
162000                                                                          
162100       ADD +1                   TO IDEX                                   
162200     END-PERFORM                                                          
162300                                                                          
162400     MOVE SPACE                 TO WDB2-GMT-IDUSER-DCUPD                  
162500     MOVE ZERO                  TO WDB2-GMT-TIAAMMDD-DCUPD                
162600     MOVE SPACE                 TO WDB2-GMT-IDDEPOT                       
162700     MOVE ZERO                  TO WDB2-GMT-IDKUNDNR-HEAD                 
162800     MOVE SPACE                 TO WDB2-GMT-IDLEVNR                       
162900     MOVE SPACE                 TO WDB2-GMT-IDDC-RET                      
163000     MOVE ZERO                  TO WDB2-GMT-KVDAGAR-DOW                   
163100     MOVE +120                  TO WDB2-GMT-KVDAGAR-RTPMN                 
163200     MOVE +245                  TO WDB2-GMT-KVDAGAR-RTATG                 
163300     MOVE SPACE                 TO WDB2-GMT-IDROUTE                       
163400     MOVE SPACE                 TO WDB2-GMT-IDSKYLT                       
163500     MOVE SPACE                 TO WDB2-GMT-IDZON                         
163600     MOVE ZERO                  TO WDB2-GMT-KDBEKALT                      
163700     MOVE SPACE                 TO WDB2-GMT-KDGENFAK                      
163800     MOVE ZERO                  TO WDB2-GMT-KDHBLKRV                      
163900     MOVE ZERO                  TO WDB2-GMT-KDSPRAK                       
164000     MOVE ZERO                  TO WDB2-GMT-KDSTATNR                      
164100     MOVE ZERO                  TO WDB2-GMT-REAVDRAG                      
164200     MOVE ZERO                  TO WDB2-GMT-REEMBHNT                      
164300     MOVE ZERO                  TO WDB2-GMT-RESLATT                       
164400     MOVE ZERO                  TO WDB2-GMT-TIFAKT                        
164500     MOVE ZERO                  TO WDB2-GMT-TISTADAT                      
164600     MOVE ZERO                  TO WDB2-GMT-TISTADAT-COD                  
164700     MOVE ZERO                  TO WDB2-GMT-TISTATID-COD                  
164800     MOVE ZERO                  TO WDB2-GMT-TISTODAT                      
164900     MOVE ZERO                  TO WDB2-GMT-TISTODAT-COD                  
165000     MOVE ZERO                  TO WDB2-GMT-TISTOTID-COD                  
165100     MOVE SPACE                 TO WDB2-GMT-FLAUTORD                      
165200     MOVE SPACE                 TO WDB2-GMT-FLNC                          
165300     MOVE SPACE                 TO WDB2-GMT-FLSAMFAK                      
165400     MOVE SPACE                 TO WDB2-GMT-FLVR                          
165500     MOVE SPACE                 TO WDB2-GMT-FLURSRAP                      
165600     MOVE +1 TO IDEX                                                      
165700     PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                                 
165800       MOVE SPACE               TO WDB2-GMT-IDDC-TVSVOR (IDEX)            
165900       ADD +1 TO IDEX                                                     
166000     END-PERFORM                                                          
166100     MOVE SPACE                 TO WDB2-GMT-IDRFTAB                       
166200     MOVE ZERO                  TO WDB2-GMT-KDORDING                      
166300     MOVE ZERO                  TO WDB2-GMT-KVVECKOR-OB                   
166400     MOVE SPACE                 TO WDB2-GMT-FLORDTIL-KL1                  
166500     MOVE SPACE                 TO WDB2-GMT-FLORDTIL-KL2                  
166600     MOVE SPACE                 TO WDB2-GMT-FLORDTIL-KL3                  
166700     MOVE SPACE                 TO WDB2-GMT-FLORDTIL-KL4                  
166800                                                                          
166900     IF REQU-FLRESTN = YES                                                
167000       MOVE JA                  TO WDB2-GMT-FLRESTN                       
167100     ELSE                                                                 
167200       MOVE REQU-FLRESTN        TO WDB2-GMT-FLRESTN                       
167300     END-IF                                                               
167400                                                                          
167500     IF REQU-FLPRELRO = YES                                               
167600       MOVE JA                  TO WDB2-GMT-FLPRELRO                      
167700     ELSE                                                                 
167800       MOVE REQU-FLPRELRO       TO WDB2-GMT-FLPRELRO                      
167900     END-IF                                                               
168000                                                                          
168100     MOVE REQU-RESLATT-UPD TO WDB2-GMT-RESLATT                            
168200                                                                          
168300     IF REQU-IDKUNDNR-H-UPD NOT = ALL '+'                                 
168400       MOVE REQU-IDKUNDNR-H-UPD TO WDB2-GMT-IDKUNDNR-HEAD                 
168500     ELSE                                                                 
168600       MOVE +0                TO WDB2-GMT-IDKUNDNR-HEAD                   
168700     END-IF                                                               
168800                                                                          
168900     IF REQU-KDBEKALT-UPD NOT = ALL '+'                                   
169000       MOVE REQU-KDBEKALT-UPD TO WDB2-GMT-KDBEKALT                        
169100     END-IF                                                               
169200                                                                          
169300     IF REQU-FLOBKR-TACD-UPD = YES                                        
169400       MOVE JA                  TO WDB2-GMT-FLOBKR-TACD                   
169500     ELSE                                                                 
169600       MOVE REQU-FLOBKR-TACD-UPD TO WDB2-GMT-FLOBKR-TACD                  
169700     END-IF                                                               
169800                                                                          
169900     IF REQU-FLDNDAP-UPD = YES                                            
170000       MOVE JA                  TO WDB2-GMT-FLDNDAP                       
170100     ELSE                                                                 
170200       MOVE REQU-FLDNDAP-UPD    TO WDB2-GMT-FLDNDAP                       
170300     END-IF                                                               
170400                                                                          
170500     IF REQU-FLORDTIL-KL1 = YES                                           
170600       MOVE JA                  TO WDB2-GMT-FLORDTIL-KL1                  
170700     ELSE                                                                 
170800       MOVE REQU-FLORDTIL-KL1   TO WDB2-GMT-FLORDTIL-KL1                  
170900     END-IF                                                               
171000                                                                          
171100     IF REQU-FLORDTIL-KL2 = YES                                           
171200       MOVE JA                  TO WDB2-GMT-FLORDTIL-KL2                  
171300     ELSE                                                                 
171400       MOVE REQU-FLORDTIL-KL2   TO WDB2-GMT-FLORDTIL-KL2                  
171500     END-IF                                                               
171600                                                                          
171700     IF REQU-FLORDTIL-KL3 = YES                                           
171800       MOVE JA                  TO WDB2-GMT-FLORDTIL-KL3                  
171900     ELSE                                                                 
172000       MOVE REQU-FLORDTIL-KL3   TO WDB2-GMT-FLORDTIL-KL3                  
172100     END-IF                                                               
172200                                                                          
172300     IF REQU-FLORDTIL-KL4 = YES                                           
172400       MOVE JA                  TO WDB2-GMT-FLORDTIL-KL4                  
172500     ELSE                                                                 
172600       MOVE REQU-FLORDTIL-KL4   TO WDB2-GMT-FLORDTIL-KL4                  
172700     END-IF                                                               
172800                                                                          
172900*    IF REQU-FLORDTIL-KL1 = YES                                           
173000*      MOVE JA                  TO WDB2-GMT-FLORDTIL-KL1                  
173100*    ELSE                                                                 
173200*      MOVE REQU-FLORDTIL-KL1   TO WDB2-GMT-FLORDTIL-KL1                  
173300*    END-IF                                                               
173400                                                                          
173500*    IF REQU-FLORDTIL-KL2 = YES                                           
173600*      MOVE JA                  TO WDB2-GMT-FLORDTIL-KL2                  
173700*    ELSE                                                                 
173800*      MOVE REQU-FLORDTIL-KL2   TO WDB2-GMT-FLORDTIL-KL2                  
173900*    END-IF                                                               
174000                                                                          
174100*    IF REQU-FLORDTIL-KL3 = YES                                           
174200*      MOVE JA                  TO WDB2-GMT-FLORDTIL-KL3                  
174300*    ELSE                                                                 
174400*      MOVE REQU-FLORDTIL-KL3   TO WDB2-GMT-FLORDTIL-KL3                  
174500*    END-IF                                                               
174600                                                                          
174700*    IF REQU-FLORDTIL-KL4 = YES                                           
174800*      MOVE JA                  TO WDB2-GMT-FLORDTIL-KL4                  
174900*    ELSE                                                                 
175000*      MOVE REQU-FLORDTIL-KL4   TO WDB2-GMT-FLORDTIL-KL4                  
175100*    END-IF                                                               
175200                                                                          
175300     PERFORM S01-INIT-GMT-LDC-KUND                                        
175400     PERFORM IMS-ISRT-WDB201                                              
175500                                                                          
175600     MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                              
175700     PERFORM MFS-FORM-ATTR                                                
175800     PERFORM MFS-RENSA-FAELT-IN                                           
175900* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
176000     .                                                                    
176100     EJECT                                                                
176200                                                                          
176300 HB-COPY-CUSTOMER SECTION.                                                
176400                                                                          
176500     MOVE REQU-IDKUNDNR-COPY TO W-IDKUNDNR-COPY                           
176600     PERFORM IMS-GU-WDB201-COPY                                           
176700     IF SEGMENT-FINNS                                                     
176800       MOVE COPY-GMT-IDDISTR     TO WDB2-GMT-IDDISTR                      
176900       MOVE WS-IDKUNDNR          TO WDB2-GMT-IDKUNDNR                     
177000                                    W-IDKUNDNR                            
177100       MOVE SPACE                TO WDB2-GMT-BEGMT                        
177200                                    WDB2-GMT-ADGMT                        
177300       MOVE SPACE                TO WDB2-GMT-IDPARTNER                    
177400       MOVE SPACE                TO WDB2-GMT-IDDEALER-VIPS                
177500       MOVE COPY-GMT-IDLANDX2    TO WDB2-GMT-IDLANDX2                     
177600                                                                          
177700       IF KDKUNDKAT-MODIFY-Y                                              
177800         MOVE REQU-KDKUNDKAT     TO WDB2-GMT-KDKUNDKAT                    
177900       ELSE                                                               
178000         MOVE COPY-GMT-KDKUNDKAT TO WDB2-GMT-KDKUNDKAT                    
178100       END-IF                                                             
178200       MOVE SPACE                TO WDB2-GMT-IDLONGITUDE                  
178300       MOVE SPACE                TO WDB2-GMT-IDLATITUDE                   
178400       MOVE SPACE                TO WDB2-GMT-BETEXT                       
178500                                                                          
178600       MOVE NEJ                  TO WDB2-GMT-FLCOD                        
178700       MOVE COPY-GMT-KDPOSTNR    TO WDB2-GMT-KDPOSTNR                     
178800       MOVE COPY-GMT-FLFAKURS    TO WDB2-GMT-FLFAKURS                     
178900       MOVE COPY-GMT-FLFAKVKT    TO WDB2-GMT-FLFAKVKT                     
179000       MOVE COPY-GMT-FLOKFAK-G   TO WDB2-GMT-FLOKFAK-G                    
179100       MOVE COPY-GMT-FLOKFAK-K   TO WDB2-GMT-FLOKFAK-K                    
179200       MOVE COPY-GMT-FLOKFAK-N   TO WDB2-GMT-FLOKFAK-N                    
179300       MOVE COPY-GMT-FLOKFAK-R   TO WDB2-GMT-FLOKFAK-R                    
179400       MOVE COPY-GMT-FLPRELRO    TO WDB2-GMT-FLPRELRO                     
179500       MOVE COPY-GMT-FLRESTN     TO WDB2-GMT-FLRESTN                      
179600       MOVE COPY-GMT-FLRETFG     TO WDB2-GMT-FLRETFG                      
179700       MOVE COPY-GMT-FLSWCONS    TO WDB2-GMT-FLSWCONS                     
179800                                                                          
179900******************************************************************        
180000****   SKA DETTA KOPIERAS ???                                             
180100       MOVE COPY-GMT-IDPARTNR    TO WDB2-GMT-IDPARTNR                     
180200       MOVE COPY-GMT-IDFTG       TO WDB2-GMT-IDFTG                        
180300       MOVE COPY-GMT-IDDISTR     TO TEST-IDDISTR                          
180400       IF DIST35-NONVCC-REFILL                                            
180500       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
180600       OR DIST35-NONVCC-VCC-TRANSFER                                      
180700       OR DIST07-NA-CUSTOMERS                                             
180800       OR WS-IDKUNDNR = ZERO                                              
180900          MOVE CHK-WC-IDFTG-PV   TO WDB2-GMT-IDFTG                        
181000       END-IF                                                             
181100******************************************************************        
181200                                                                          
181300       MOVE +1                   TO IDEX                                  
181400       PERFORM UNTIL IDEX > IX-DCCLEAR-MAX                                
181500         MOVE COPY-GMT-IDDC-BULK (IDEX)                                   
181600                                 TO WDB2-GMT-IDDC-BULK (IDEX)             
181700         MOVE COPY-GMT-IDDC-DAY  (IDEX)                                   
181800                                 TO WDB2-GMT-IDDC-DAY  (IDEX)             
181900         MOVE COPY-GMT-IDDC-VOR  (IDEX)                                   
182000                                 TO WDB2-GMT-IDDC-VOR  (IDEX)             
182100         ADD +1                  TO IDEX                                  
182200       END-PERFORM                                                        
182300                                                                          
182400       MOVE REQU-IDUSER            TO WDB2-GMT-IDUSER-DCUPD               
182500       MOVE DAGENS-DATUM           TO WDB2-GMT-TIAAMMDD-DCUPD             
182600                                                                          
182700       MOVE COPY-GMT-IDDC-RET      TO WDB2-GMT-IDDC-RET                   
182800       MOVE COPY-GMT-KVDAGAR-DOW   TO WDB2-GMT-KVDAGAR-DOW                
182900       MOVE COPY-GMT-KVDAGAR-RTPMN TO WDB2-GMT-KVDAGAR-RTPMN              
183000       MOVE COPY-GMT-KVDAGAR-RTATG TO WDB2-GMT-KVDAGAR-RTATG              
183100                                                                          
183200       MOVE '  '                   TO WDB2-GMT-IDDEPOT                    
183300       MOVE COPY-GMT-IDLEVNR       TO WDB2-GMT-IDLEVNR                    
183400       MOVE COPY-GMT-IDKUNDNR-HEAD TO WDB2-GMT-IDKUNDNR-HEAD              
183500       MOVE COPY-GMT-KDBEKALT      TO WDB2-GMT-KDBEKALT                   
183600       MOVE COPY-GMT-FLOBKR-TACD   TO WDB2-GMT-FLOBKR-TACD                
183700       MOVE COPY-GMT-FLDNDAP       TO WDB2-GMT-FLDNDAP                    
183800       MOVE ' '                    TO WDB2-GMT-IDROUTE                    
183900       MOVE COPY-GMT-IDSKYLT       TO WDB2-GMT-IDSKYLT                    
184000       MOVE '00'                   TO WDB2-GMT-IDZON                      
184100       MOVE COPY-GMT-KDGENFAK      TO WDB2-GMT-KDGENFAK                   
184200       MOVE COPY-GMT-KDHBLKRV      TO WDB2-GMT-KDHBLKRV                   
184300       MOVE COPY-GMT-KDSPRAK       TO WDB2-GMT-KDSPRAK                    
184400       MOVE COPY-GMT-KDSTATNR      TO WDB2-GMT-KDSTATNR                   
184500       MOVE COPY-GMT-REAVDRAG      TO WDB2-GMT-REAVDRAG                   
184600       MOVE COPY-GMT-REEMBHNT      TO WDB2-GMT-REEMBHNT                   
184700       MOVE COPY-GMT-RESLATT       TO WDB2-GMT-RESLATT                    
184800                                                                          
184900******** NOLLSTÄLLS FÖR ATT MAN VERKLIGEN SKA KOLLA                       
185000******** ATT KUNDEN HAR VALIDA VÄRDEN INNAN DEN BÖRJAR GÄLLA              
185100                                                                          
185200       MOVE ZERO                   TO WDB2-GMT-TIFAKT                     
185300       MOVE ZERO                   TO WDB2-GMT-TISTADAT                   
185400       MOVE ZERO                   TO WDB2-GMT-TISTODAT                   
185500                                                                          
185600**********************************************************                
185700                                                                          
185800       MOVE ZERO                   TO WDB2-GMT-TISTADAT-COD               
185900                                      WDB2-GMT-TISTATID-COD               
186000                                      WDB2-GMT-TISTODAT-COD               
186100                                      WDB2-GMT-TISTOTID-COD               
186200                                                                          
186300       MOVE COPY-GMT-FLAUTORD      TO WDB2-GMT-FLAUTORD                   
186400       MOVE COPY-GMT-FLNC          TO WDB2-GMT-FLNC                       
186500       MOVE COPY-GMT-FLSAMFAK      TO WDB2-GMT-FLSAMFAK                   
186600       MOVE COPY-GMT-FLVR          TO WDB2-GMT-FLVR                       
186700       MOVE COPY-GMT-FLURSRAP      TO WDB2-GMT-FLURSRAP                   
186800       MOVE +1 TO IDEX                                                    
186900       PERFORM UNTIL IDEX > IDEX-TVSVOR-MAX                               
187000         MOVE COPY-GMT-IDDC-TVSVOR (IDEX)                                 
187100                       TO WDB2-GMT-IDDC-TVSVOR (IDEX)                     
187200         ADD +1 TO IDEX                                                   
187300       END-PERFORM                                                        
187400       MOVE COPY-GMT-FLPRERS       TO WDB2-GMT-FLPRERS                    
187500       MOVE COPY-GMT-IDRFTAB       TO WDB2-GMT-IDRFTAB                    
187600       MOVE COPY-GMT-KDORDING      TO WDB2-GMT-KDORDING                   
187700       MOVE COPY-GMT-KVVECKOR-OB   TO WDB2-GMT-KVVECKOR-OB                
187800       MOVE COPY-GMT-FLORDTIL-KL1  TO WDB2-GMT-FLORDTIL-KL1               
187900       MOVE COPY-GMT-FLORDTIL-KL2  TO WDB2-GMT-FLORDTIL-KL2               
188000       MOVE COPY-GMT-FLORDTIL-KL3  TO WDB2-GMT-FLORDTIL-KL3               
188100       MOVE COPY-GMT-FLORDTIL-KL4  TO WDB2-GMT-FLORDTIL-KL4               
188200                                                                          
188300       PERFORM S02-COPY-GMT-LDC-KUND                                      
188400       PERFORM IMS-ISRT-WDB201                                            
188500                                                                          
188600     END-IF                                                               
188700                                                                          
188800     MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                              
188900     PERFORM MFS-FORM-ATTR                                                
189000     MOVE MFS-ADD-SAETT-CURSOR TO RESP-BEGMT-RAD1-ATTR                    
189100     PERFORM MFS-RENSA-FAELT-IN                                           
189200* * * MFS-ROR-EJ-FAELT TILL FASTA VÄRDEN                                  
189300     .                                                                    
189400     EJECT                                                                
189500                                                                          
189600 S01-INIT-GMT-LDC-KUND SECTION.                                           
189700                                                                          
189800     MOVE SPACE                    TO WDB2-GMT-IDDC-RFS(1)                
189900                                      WDB2-GMT-IDDC-RFS(2)                
190000                                      WDB2-GMT-IDDC-RFS(3)                
190100                                      WDB2-GMT-IDDC-RFS(4)                
190200     MOVE ZERO                     TO WDB2-GMT-KVDAGAR-RFS(1)             
190300                                      WDB2-GMT-KVDAGAR-RFS(2)             
190400                                      WDB2-GMT-KVDAGAR-RFS(3)             
190500                                      WDB2-GMT-KVDAGAR-RFS(4)             
190600     MOVE NEJ                      TO WDB2-GMT-FLLDCKND                   
190700     MOVE NEJ                      TO WDB2-GMT-FLKVBRYT-ORDKL1            
190800     MOVE NEJ                      TO WDB2-GMT-FLKVBRYT-ORDKL2            
190900     MOVE NEJ                      TO WDB2-GMT-FLKVBRYT-ORDKL3            
191000     MOVE NEJ                      TO WDB2-GMT-FLKVBRYT-ORDKL4            
191100     MOVE '11'                     TO WDB2-GMT-IDDC-RET72(1)              
191200     MOVE SPACE                    TO WDB2-GMT-IDDC-RET72(2)              
191300     MOVE SPACE                    TO WDB2-GMT-IDDC-RET72(3)              
191400     MOVE ZERO                     TO WDB2-GMT-KVDAGAR-CDC                
191500     MOVE ZERO                     TO WDB2-GMT-KVDAGAR-SDC                
191600     MOVE ZERO                     TO WDB2-GMT-KVDAGAR-RFS-DEF            
191700     MOVE NEJ                      TO WDB2-GMT-FLRETFG                    
191800     MOVE NEJ                      TO WDB2-GMT-FLSWCONS                   
191900     MOVE NEJ                      TO WDB2-GMT-FLRETUR                    
192000     MOVE +1 TO IDEX                                                      
192100     PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                                
192200       MOVE SPACE               TO WDB2-GMT-IDDC-PREPLAN (IDEX)           
192300       ADD +1 TO IDEX                                                     
192400     END-PERFORM                                                          
192500     .                                                                    
192600     EJECT                                                                
192700                                                                          
192800 S02-COPY-GMT-LDC-KUND SECTION.                                           
192900                                                                          
193000     MOVE COPY-GMT-IDDC-RFS(1)     TO WDB2-GMT-IDDC-RFS(1)                
193100     MOVE COPY-GMT-KVDAGAR-RFS(1)                                         
193200                                   TO WDB2-GMT-KVDAGAR-RFS(1)             
193300     MOVE COPY-GMT-IDDC-RFS(2)     TO WDB2-GMT-IDDC-RFS(2)                
193400     MOVE COPY-GMT-KVDAGAR-RFS(2)                                         
193500                                   TO WDB2-GMT-KVDAGAR-RFS(2)             
193600     MOVE COPY-GMT-IDDC-RFS(3)     TO WDB2-GMT-IDDC-RFS(3)                
193700     MOVE COPY-GMT-KVDAGAR-RFS(3)                                         
193800                                   TO WDB2-GMT-KVDAGAR-RFS(3)             
193900     MOVE COPY-GMT-IDDC-RFS(4)     TO WDB2-GMT-IDDC-RFS(4)                
194000     MOVE COPY-GMT-KVDAGAR-RFS(4)                                         
194100                                   TO WDB2-GMT-KVDAGAR-RFS(4)             
194200     MOVE COPY-GMT-FLLDCKND        TO WDB2-GMT-FLLDCKND                   
194300     MOVE COPY-GMT-FLKVBRYT-ORDKL1 TO WDB2-GMT-FLKVBRYT-ORDKL1            
194400     MOVE COPY-GMT-FLKVBRYT-ORDKL2 TO WDB2-GMT-FLKVBRYT-ORDKL2            
194500     MOVE COPY-GMT-FLKVBRYT-ORDKL3 TO WDB2-GMT-FLKVBRYT-ORDKL3            
194600     MOVE COPY-GMT-FLKVBRYT-ORDKL4 TO WDB2-GMT-FLKVBRYT-ORDKL4            
194700     MOVE COPY-GMT-IDDC-RET72(1)   TO WDB2-GMT-IDDC-RET72(1)              
194800     MOVE COPY-GMT-IDDC-RET72(2)   TO WDB2-GMT-IDDC-RET72(2)              
194900     MOVE COPY-GMT-IDDC-RET72(3)   TO WDB2-GMT-IDDC-RET72(3)              
195000     MOVE COPY-GMT-KVDAGAR-CDC     TO WDB2-GMT-KVDAGAR-CDC                
195100     MOVE COPY-GMT-KVDAGAR-SDC     TO WDB2-GMT-KVDAGAR-SDC                
195200     MOVE COPY-GMT-KVDAGAR-RFS-DEF TO WDB2-GMT-KVDAGAR-RFS-DEF            
195300     MOVE COPY-GMT-FLRETUR         TO WDB2-GMT-FLRETUR                    
195400     MOVE +1                       TO IDEX                                
195500     PERFORM UNTIL IDEX > IDEX-PREPLAN-MAX                                
195600       MOVE COPY-GMT-IDDC-PREPLAN (IDEX)                                  
195700                               TO WDB2-GMT-IDDC-PREPLAN (IDEX)            
195800       ADD +1                      TO IDEX                                
195900     END-PERFORM                                                          
196000     .                                                                    
196100     EJECT                                                                
196200                                                                          
196300 MFS-RENSA-FAELT-UT SECTION.                                              
196400                                                                          
196500*    --- ALLA UTDATA-FÄLT                                                 
196600     MOVE ALL-SPACE TO RESP-IDKUNDNR-COPY                                 
196700                             RESP-BEGMT-RAD1                              
196800                             RESP-IDPARTNER                               
196900                             RESP-BEGMT-RAD2                              
197000                             RESP-IDDEALER-VIPS                           
197100                             RESP-ADGMT-GATA                              
197200                             RESP-IDLANDX2                                
197300                             RESP-ADPOSTNR                                
197400                             RESP-KDPOSTNR                                
197500                             RESP-KDKUNDKAT                               
197600                             RESP-ADCITY                                  
197700                             RESP-IDLONGITUDE                             
197800                             RESP-ADGMT-LAND                              
197900                             RESP-IDLATITUDE                              
198000                             RESP-IDTFN                                   
198100                             RESP-BETEXT                                  
198200                             RESP-FLRESTN                                 
198300                             RESP-IDZON                                   
198400                             RESP-FLPRELRO                                
198500                             RESP-IDDEPOT                                 
198600                             RESP-RESLATT-UT                              
198700                             RESP-IDROUTE                                 
198800                             RESP-IDKUNDNR-H-UT                           
198900                             RESP-KDBEKALT-UT                             
199000                             RESP-FLOBKR-TACD-UT                          
199100                             RESP-FLDNDAP-UT                              
199200                             RESP-FLORDTIL-KL1                            
199300                             RESP-FLORDTIL-KL2                            
199400                             RESP-FLORDTIL-KL3                            
199500                             RESP-FLORDTIL-KL4                            
199600     .                                                                    
199700     SKIP3                                                                
199800 MFS-RENSA-FAELT-IN SECTION.                                              
199900                                                                          
200000*    --- ALLA INDATA-FÄLT                                                 
200100     MOVE ALL-SPACE TO RESP-IDKUNDNR-COPY                                 
200200                             RESP-BEGMT-RAD1                              
200300                             RESP-IDPARTNER                               
200400                             RESP-BEGMT-RAD2                              
200500                             RESP-IDDEALER-VIPS                           
200600                             RESP-ADGMT-GATA                              
200700                             RESP-IDLANDX2                                
200800                             RESP-ADPOSTNR                                
200900                             RESP-KDPOSTNR                                
201000                             RESP-KDKUNDKAT                               
201100                             RESP-ADCITY                                  
201200                             RESP-IDLONGITUDE                             
201300                             RESP-ADGMT-LAND                              
201400                             RESP-IDLATITUDE                              
201500                             RESP-IDTFN                                   
201600                             RESP-BETEXT                                  
201700                             RESP-FLRESTN                                 
201800                             RESP-FLPRELRO                                
201900                             RESP-RESLATT-UPD                             
202000                             RESP-IDKUNDNR-H-UPD                          
202100                             RESP-KDBEKALT-UPD                            
202200                             RESP-FLOBKR-TACD-UPD                         
202300                             RESP-FLDNDAP-UPD                             
202400                             RESP-FLORDTIL-KL1                            
202500                             RESP-FLORDTIL-KL2                            
202600                             RESP-FLORDTIL-KL3                            
202700                             RESP-FLORDTIL-KL4                            
202800     .                                                                    
202900     EJECT                                                                
203000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
203100                                                                          
203200*    --- ALLA UTDATA-FÄLT                                                 
203300     MOVE ALL-PLUS TO RESP-IDKUNDNR-COPY                                  
203400                               RESP-BEGMT-RAD1                            
203500                               RESP-IDPARTNER                             
203600                               RESP-BEGMT-RAD2                            
203700                               RESP-IDDEALER-VIPS                         
203800                               RESP-ADGMT-GATA                            
203900                               RESP-IDLANDX2                              
204000                               RESP-ADPOSTNR                              
204100                               RESP-KDPOSTNR                              
204200                               RESP-KDKUNDKAT                             
204300                               RESP-ADCITY                                
204400                               RESP-IDLONGITUDE                           
204500                               RESP-ADGMT-LAND                            
204600                               RESP-IDLATITUDE                            
204700                               RESP-IDTFN                                 
204800                               RESP-BETEXT                                
204900                               RESP-FLRESTN                               
205000                               RESP-IDZON                                 
205100                               RESP-FLPRELRO                              
205200                               RESP-IDDEPOT                               
205300                               RESP-RESLATT-UT                            
205400                               RESP-IDROUTE                               
205500                               RESP-IDKUNDNR-H-UT                         
205600                               RESP-KDBEKALT-UT                           
205700                               RESP-FLOBKR-TACD-UT                        
205800                               RESP-FLDNDAP-UT                            
205900                               RESP-FLORDTIL-KL1                          
206000                               RESP-FLORDTIL-KL2                          
206100                               RESP-FLORDTIL-KL3                          
206200                               RESP-FLORDTIL-KL4                          
206300     .                                                                    
206400     SKIP3                                                                
206500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
206600                                                                          
206700*    --- ALLA INDATA-FÄLT                                                 
206800     MOVE ALL-PLUS TO RESP-IDKUNDNR-COPY                                  
206900*                              RESP-BEGMT-RAD1                            
207000                               RESP-BEGMT-RAD2                            
207100                               RESP-IDPARTNER                             
207200                               RESP-IDDEALER-VIPS                         
207300                               RESP-ADGMT-GATA                            
207400                               RESP-IDLANDX2                              
207500                               RESP-ADPOSTNR                              
207600                               RESP-KDPOSTNR                              
207700                               RESP-KDKUNDKAT                             
207800                               RESP-ADCITY                                
207900                               RESP-ADGMT-LAND                            
208000                               RESP-IDTFN                                 
208100                               RESP-BETEXT                                
208200                               RESP-FLRESTN                               
208300                               RESP-FLPRELRO                              
208400                               RESP-RESLATT-UPD                           
208500                               RESP-IDKUNDNR-H-UPD                        
208600                               RESP-KDBEKALT-UPD                          
208700                               RESP-FLOBKR-TACD-UPD                       
208800                               RESP-FLDNDAP-UPD                           
208900                               RESP-FLORDTIL-KL1                          
209000                               RESP-FLORDTIL-KL2                          
209100                               RESP-FLORDTIL-KL3                          
209200                               RESP-FLORDTIL-KL4                          
209300     .                                                                    
209400     EJECT                                                                
209500 MFS-FORM-ATTR SECTION.                                                   
209600                                                                          
209700*    --- ALLA INDATA-FÄLT                                                 
209800     MOVE MFS-FORMATETS-ATTR TO RESP-IDKUNDNR-COPY-ATTR                   
209900                                RESP-BEGMT-RAD1-ATTR                      
210000                                RESP-IDPARTNER-ATTR                       
210100                                RESP-BEGMT-RAD2-ATTR                      
210200                                RESP-IDDEALER-VIPS-ATTR                   
210300                                RESP-ADGMT-GATA-ATTR                      
210400                                RESP-IDLANDX2-ATTR                        
210500                                RESP-ADPOSTNR-ATTR                        
210600                                RESP-KDPOSTNR-ATTR                        
210700                                RESP-KDKUNDKAT-ATTR                       
210800                                RESP-ADCITY-ATTR                          
210900                                RESP-IDLONGITUDE-ATTR                     
211000                                RESP-ADGMT-LAND-ATTR                      
211100                                RESP-IDLATITUDE-ATTR                      
211200                                RESP-IDTFN-ATTR                           
211300                                RESP-BETEXT-ATTR                          
211400                                RESP-FLRESTN-ATTR                         
211500                                RESP-FLPRELRO-ATTR                        
211600                                RESP-RESLATT-UPD-ATTR                     
211700                                RESP-IDKUNDNR-H-UPD-ATTR                  
211800                                RESP-KDBEKALT-UPD-ATTR                    
211900                                RESP-FLOBKR-TACD-UPD-ATTR                 
212000                                RESP-FLDNDAP-UPD-ATTR                     
212100                                RESP-FLORDTIL-KL1-ATTR                    
212200                                RESP-FLORDTIL-KL2-ATTR                    
212300                                RESP-FLORDTIL-KL3-ATTR                    
212400                                RESP-FLORDTIL-KL4-ATTR                    
212500     .                                                                    
212600     EJECT                                                                
212700* --- IMS SEKTIONER ---                                                   
212800     SKIP3                                                                
212900 IMS-GU-WDB201-COPY SECTION.                                              
213000                                                                          
213100     STRING 'WDB201  (IDGMT    =' W-IDGMT-COPY-X ')'                      
213200          DELIMITED BY SIZE INTO SSA1                                     
213300     MOVE '  GE' TO GODK-STATUSKODER                                      
213400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB2-2 SSA1                    
213500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
213600     PERFORM IMS-STATUSKONTROLL                                           
213700     .                                                                    
213800     EJECT                                                                
213900 IMS-GHU-WDB201 SECTION.                                                  
214000                                                                          
214100     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
214200          DELIMITED BY SIZE INTO SSA1                                     
214300     MOVE '  GE' TO GODK-STATUSKODER                                      
214400     CALL CBLTDLI USING GHU WDB2-PCB DLI-IO-WDB2 SSA1                     
214500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
214600     PERFORM IMS-STATUSKONTROLL                                           
214700     .                                                                    
214800     SKIP3                                                                
214900 IMS-GU-WDB201-IDPARTNER SECTION.                                         
215000                                                                          
215100     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
215200                    '&IDGMT   <=' W-IDGMT-MAX-X                           
215300                    '&IDPARTNE =' W-IDPARTNER-X ')'                       
215400            DELIMITED BY SIZE INTO SSA1                                   
215500     MOVE '  GE' TO GODK-STATUSKODER                                      
215600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB2-2 SSA1                    
215700     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
215800     PERFORM IMS-STATUSKONTROLL                                           
215900     .                                                                    
216000     EJECT                                                                
216100 IMS-ISRT-WDB201 SECTION.                                                 
216200                                                                          
216300     MOVE 'WDB201   ' TO SSA1                                             
216400     MOVE '  II' TO GODK-STATUSKODER                                      
216500     CALL CBLTDLI USING ISRT WDB2-PCB DLI-IO-WDB2 SSA1                    
216600     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
216700     PERFORM IMS-STATUSKONTROLL                                           
216800     .                                                                    
216900     SKIP3                                                                
217000 IMS-REPL-WDB201 SECTION.                                                 
217100                                                                          
217200     MOVE '  ' TO GODK-STATUSKODER                                        
217300     CALL CBLTDLI USING REPL WDB2-PCB DLI-IO-WDB2                         
217400     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
217500     PERFORM IMS-STATUSKONTROLL                                           
217600     .                                                                    
217700                                                                          
217800                                                                          
217900 IMS-STATUSKONTROLL SECTION.                                              
218000                                                                          
218100     SET STATUS-IX TO 1                                                   
218200     SEARCH GODK-STATUS                                                   
218300       AT END                                                             
218400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
218500         DELIMITED BY SIZE INTO FELTEXT                                   
218600         CALL FELLOG                                                      
218700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
218800         CONTINUE                                                         
218900     END-SEARCH                                                           
219000     .                                                                    
219100     EJECT                                                                
219200*    -COPY WY2000P1                                                       

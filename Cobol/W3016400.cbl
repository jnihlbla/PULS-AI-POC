001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W3016400.                                                
001500 AUTHOR.         BO HAMMARIN.                                             
001600 DATE-WRITTEN.   MAJ-2000.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        PGM VISAR INFORMATION FÖR OVERSEA-RETURER                        
002010*        . DISTRIKT 7512 = USA                                            
002011*        . DISTRIKT 7625 = CANADA                                         
002020*        . DISTRIKT 7835 = AUSTRALIA                                      
002030*        . DISTRIKT 5220 = JAPAN                                          
002030*        . DISTRIKT 6121 = KOREA                                          
002030*        . DISTRIKT 5619 = MALAYSIA                                       
002030*        . DISTRIKT 6251 = THAILAND                                       
002030*        . DISTRIKT 6200 = TAIWAN                                         
002030*        . DISTRIKT 6270 = KINA                                           
002100*                                                                         
002220*        PROGRAMMET LÄSER      WDM6 VIA WDM6ESEQ                          
002230*        PROGRAMMET LÄSER      WDD3 VIA WDD3BSEQ                          
002240*        PROGRAMMET LÄSER      WDK6                                       
002241*        PROGRAMMET LÄSER      WDR1 (WDGX3157)                            
002242*        PROGRAMMET LÄSER      WDR4 (WDGX3171)                            
002243*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W3T164                                              
002600*        MID:         W3I16401                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W3O16401                                            
002901*                                                                         
002910*    CHANGE LOG:                                                          
002920*                                                                         
002930*    DIGAMBAR/20020714                                                    
002940*    WDM6E INDEX IS CHANGED TO REFER THE IDBYTREP-9KOMPL INSTEAD          
002950*    OF THE IDBYTREP THIS IS TO SHOW THE DETAILS IN DESCENDING            
002960*    ORDER OF THE IDBYTREP.                                               
002970                                                                          
003000                                                                          
003200 ENVIRONMENT DIVISION.                                                    
003300                                                                          
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003510                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                       PIC X(08)   VALUE 'W3016400'.            
003800                                                                          
003900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100                                                                          
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004310 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
004330 77  MAX-INDX                    PIC S9(4)   VALUE +13  COMP SYNC.        
004340 77  W-9KOMPL                    PIC 9(7)   VALUE 9999999.                
004400                                                                          
004600*    --- ARBETSFÄLT FÖR DIVERSE TILLSTÅND                                 
005404 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005405     88  NYCKLAR-OK                          VALUE 'J'.                   
005406     88  NYCKLAR-FEL                         VALUE 'N'.                   
005407                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '3164'.                
005700     88  GODK-MID                            VALUE '3164'                 
005800                                                   '3165'                 
005900                                                   '3166'.                
006200     88  HELP-MID                            VALUE '0551'.                
006210                                                                          
006220 77  W-READ-WITH-KEY             PIC X(1)    VALUE 'N'.                   
006230     88  READ-WITH-KEY                       VALUE 'J'.                   
006300     EJECT                                                                
006310                                                                          
006320 01  FILLER                      PIC  X(16)  VALUE 'DIVERSE   '.          
006330*                                                                         
006331 01  W-TEST-IDARTNR              PIC X(9) VALUE SPACE.                    
006333 01  WS-IDARTNR-9                PIC  9(9).                               
006335 01  WS-TIAAVV-NUM               PIC  9(4).                               
006336 01  WS-TIAAMMDD-NUM             PIC  9(6).                               
006337 01  WS-DAANKDAG                 PIC  9(8)   VALUE 20000000.              
006385     EJECT                                                                
006386                                                                          
006387*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
006388*                                                                         
006389 01  SPAR-AREA.                                                           
006390     03  SPAR-IDTRANS            PIC X(4)    VALUE '3164'.                
006391     03  SPAR-IDDISTR-ENTER      PIC 9(4)    VALUE ZERO.                  
006392     03  SPAR-IDBYTRAP-ENTER     PIC 9(7)    VALUE ZERO.                  
006394     03  SPAR-IDBYTRAD-ENTER     PIC 9(5)    VALUE ZERO.                  
006395     03  SPAR-IDARTNR-ENTER      PIC 9(8)    VALUE ZERO.                  
006396     03  SPAR-IDFAKT-ENTER       PIC 9(7)    VALUE ZERO.                  
006397     03  SPAR-IDKOLLI-ENTER      PIC 9(5)    VALUE ZERO.                  
006398     03  SPAR-IDDISTR-NEXT       PIC 9(4)    VALUE ZERO.                  
006399     03  SPAR-IDBYTRAP-NEXT      PIC 9(7)    VALUE ZERO.                  
006400     03  SPAR-IDBYTRAD-NEXT      PIC 9(5)    VALUE ZERO.                  
006410     03  SPAR-IDARTNR-NEXT       PIC 9(8)    VALUE ZERO.                  
006414     03  SPAR-IDFAKT-NEXT        PIC 9(7)    VALUE ZERO.                  
006415     03  SPAR-IDKOLLI-NEXT       PIC 9(5)    VALUE ZERO.                  
006420     EJECT                                                                
006421                                                                          
006422 01  FILLER                      PIC  X(16)  VALUE 'BYTES-TEST'.          
006423 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
006424*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
006425                                                                          
006426*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
006427                                                                          
006428*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
006429                                                                          
006430     EJECT                                                                
006431                                                                          
006432*    --- VALID IDDC CODES                                                 
006433*                                                                         
006434*01  -COPY WWDC99                                                         
006435     EJECT                                                                
006436                                                                          
006440*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006500 01  GENERELLA-SUBPROGRAM.                                                
006510     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007020     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
007100     EJECT                                                                
007110                                                                          
007120*    --- PARAMETRAR TILL ABEND                                            
007130                                                                          
007140 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007150 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007160 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007170     EJECT                                                                
007180                                                                          
007200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007300*01 -COPY WMEDAREA                                                        
007310     EJECT                                                                
007410                                                                          
007420 01  W009VADD-DATUM              PIC S9(5)   VALUE ZERO COMP-3.           
007430 01  W009VADD-ANTAL              PIC S9(3)   VALUE ZERO COMP-3.           
007440     EJECT                                                                
007450                                                                          
007500 01  MESSAGE-CODES.                                                       
007510     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007520     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
007800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007901     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
007902     EJECT                                                                
007903                                                                          
007910*01  -COPY WDATAREA                                                       
008000     EJECT                                                                
008010                                                                          
008100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008200*                                                                         
008300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500*01 -COPY WMSGINIT                                                        
008700     EJECT                                                                
008710                                                                          
008800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009200*01  MID -COPY W3I16401                                                   
009300     EJECT                                                                
009310                                                                          
009400 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
009710                                                                          
009800     03  MOD REDEFINES MSG-AREA.                                          
009900*      05  -COPY W3O16401                                                 
010000     EJECT                                                                
010010                                                                          
010100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010300*01  -COPY WMFSAREA                                                       
010400     EJECT                                                                
010410                                                                          
010500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010600*                                                                         
010700 01  FILLER                      PIC X(16)  VALUE 'IMS-WS'.               
010900 01  NYCKLAR-TILL-DLI.                                                    
011035     03  W-IDARTNR-D3-X.                                                  
011040         05  W-IDARTNR-D3        PIC S9(9)  VALUE ZERO COMP-3.            
011050     03  W-IDSKYLT-D3-X.                                                  
011060         05  W-IDSKYLT-D3        PIC X(3)   VALUE 'GB'.                   
011085     03  W-WDM6E1KY-MIN.                                                  
011086         05  W-IDARTNR-MIN   PIC S9(9)   COMP-3.                          
011090         05  W-IDDISTR-MIN   PIC S9(5)   COMP-3 VALUE ZERO.               
011100         05  W-IDBYTRAP-9KOMPL-MIN                                        
011101                             PIC S9(7)   COMP-3 VALUE ZERO.               
011102         05  W-IDBYTRAD-MIN  PIC S9(5)   COMP-3 VALUE ZERO.               
011103     03  W-WDM6E1KY-MAX.                                                  
011104         05  W-IDARTNR-MAX   PIC S9(9)   COMP-3.                          
011105         05  W-IDDISTR-MAX   PIC S9(5)   COMP-3 VALUE 99999.              
011106         05  W-IDBYTRAP-9KOMPL-MAX                                        
011107                             PIC S9(7)   COMP-3 VALUE 9999999.            
011108         05  W-IDBYTRAD-MAX  PIC S9(5)   COMP-3 VALUE 99999.              
011114     03  W-WDM601KY-X.                                                    
011115         05  W-IDDISTR-M6        PIC S9(5)  VALUE ZERO COMP-3.            
011116         05  W-IDBYTRAP-M6       PIC S9(7)  VALUE ZERO COMP-3.            
011119     03  W-IDBYTRAD-X.                                                    
011120         05  W-IDBYTRAD          PIC S9(5)  VALUE ZERO COMP-3.            
011121     03  W-IDARTNR-K6-X.                                                  
011122         05  W-IDARTNR-K6        PIC S9(9)   VALUE ZERO COMP-3.           
011123     03  W-KDSEGKEY-K6-X.                                                 
011124         05  W-KDSEGKEY-K6       PIC X(1)    VALUE '1'.                   
011125     03  W-WDGXKEY-3157-X.                                                
011126         05  FILLER              PIC X(4)    VALUE '3157'.                
011127         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
011128     03  W-KEY3158-X.                                                     
011129         05  W-IDDISTR-3158      PIC S9(4)   VALUE ZERO   COMP-3.         
011130         05  W-KDEXCHA-3158      PIC S9(3)   VALUE ZERO   COMP-3.         
011131     03  W-WDGXKEY-3171-X.                                                
011132         05  W-IDHTYP-3171       PIC X(4)    VALUE '3171'.                
011133         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
011134     03  W-IDFAKT-3172-X.                                                 
011135         05  W-IDFAKT-3172       PIC S9(7)   VALUE ZERO COMP-3.           
011136     03  W-IDKOLLI-3174-X.                                                
011137         05  W-IDKOLLI-3174      PIC S9(5)   VALUE ZERO COMP-3.           
011138     03  W-IDARTNR-3176-X.                                                
011139         05  W-IDARTNR-3176      PIC S9(9)   VALUE ZERO COMP-3.           
011140                                                                          
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                      VALUE '  '.                   
011600     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
011610     88  SEGMENT-SLUT                       VALUE 'GB'.                   
011700                                                                          
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000                                                                          
012100 01  SSA1                        PIC X(96).                               
012200 01  SSA2                        PIC X(64).                               
012210 01  SSA3                        PIC X(64).                               
012220 01  SSA4                        PIC X(64).                               
012300     EJECT                                                                
012310                                                                          
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012710                                                                          
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM6'.                        
013020 01  DLI-IO-WDM6.                                                         
013030     03  DLI-IO-WDM601.                                                   
013040*        05  -COPY WDM601                                                 
013050     03  DLI-IO-WDM611.                                                   
013060*        05  -COPY WDM611                                                 
013070     EJECT                                                                
013264 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDM6E1'.                      
013265 01  DLI-IO-WDM6E1.                                                       
013266*    03  -COPY WDM6E1                                                     
013267     EJECT                                                                
013268                                                                          
013269 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311  '.                    
013270 01  DLI-IO-WDD311.                                                       
013271*    03  -COPY WDD311                                                     
013280     EJECT                                                                
013320                                                                          
013330 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR101'.                      
013340 01  DLI-IO-WDR101.                                                       
013350*    03  -COPY WDGX01 -PRE WDR101-                                        
013360     EJECT                                                                
013362                                                                          
013363 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
013364 01  DLI-IO-WDK611.                                                       
013365*    03  -COPY WDK611                                                     
013366     EJECT                                                                
013367                                                                          
013370 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3158'.                    
013380 01  DLI-IO-WDGX3158.                                                     
013390*    03  -COPY WDGX3158                                                   
013391     EJECT                                                                
013392                                                                          
013393 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3171'.                    
013394 01  DLI-IO-WDGX3171.                                                     
013395*    03  -COPY WDGX01                                                     
013396     EJECT                                                                
013397                                                                          
013398 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3172'.                    
013399 01  DLI-IO-WDGX3172.                                                     
013400*    03  -COPY WDGX3172                                                   
013401     EJECT                                                                
013402                                                                          
013403 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3174'.                    
013404 01  DLI-IO-WDGX3174.                                                     
013405*    03  -COPY WDGX3174                                                   
013406     EJECT                                                                
013407                                                                          
013408 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3176'.                    
013409 01  DLI-IO-WDGX3176.                                                     
013410*    03  -COPY WDGX3176                                                   
013411     EJECT                                                                
013412                                                                          
013420 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013510                                                                          
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801                                                                          
013802*01  -COPY W0008  -PRE WDM6E-                                             
013803     05  FILLER                  PIC X.                                   
013804                                                                          
013828*01  -COPY W0008   -PRE WDM6-                                             
013829     05  FILLER                  PIC X.                                   
013830                                                                          
013850*01  -COPY W0008   -PRE WDD3B-                                            
013860     05  FILLER                  PIC X.                                   
013870                                                                          
013871*01  -COPY W0008   -PRE WDK6-                                             
013872     05  FILLER                  PIC X.                                   
013873                                                                          
013880*01  -COPY W0008   -PRE 3157-                                             
013890     05  FILLER                  PIC X.                                   
013891                                                                          
013892*01  -COPY W0008   -PRE 3171-                                             
013893     05  FILLER                  PIC X.                                   
013894     EJECT                                                                
014000                                                                          
014001 PROCEDURE DIVISION  USING MSG-PCB   USEA-PCB                             
014006                           WDM6E-PCB                                      
014008                           WDM6-PCB                                       
014009                           WDD3B-PCB                                      
014010                           WDK6-PCB                                       
014011                           3157-PCB                                       
014012                           3171-PCB.                                      
014013 MAIN SECTION.                                                            
014014     ENTRY 'DLITCBL' USING MSG-PCB   USEA-PCB                             
014015                           WDM6E-PCB                                      
014017                           WDM6-PCB                                       
014018                           WDD3B-PCB                                      
014019                           WDK6-PCB                                       
014020                           3157-PCB                                       
014030                           3171-PCB.                                      
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014310                                                                          
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-CHECK-KEYS                                               
014700       IF NYCKLAR-OK                                                      
014800         IF MFS-FIRST                                                     
014900           PERFORM C-FIRST-PAGE                                           
015000         ELSE                                                             
015100           IF MFS-NEXT                                                    
015110             PERFORM D-NEXT-PAGE                                          
015120           ELSE                                                           
015121             PERFORM E-SAME-PAGE                                          
015122           END-IF                                                         
015123         END-IF                                                           
015200         PERFORM F-READ-SHOW-INFO                                         
015400       END-IF                                                             
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O16401 + 4                      
015700       PERFORM IMS-INSERT-MSG                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016600                                                                          
016610 A-INIT SECTION.                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I16401                 
016900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
017000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I16401                 
017300       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
017400       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
017800     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
017900     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
018000                                                                          
018100     MOVE LOW-VALUE                       TO MSG-AREA                     
018200     MOVE 'W3O164N1'                      TO MFS-IDMOD                    
018300     MOVE '3164'                          TO MOD-IDTRANS                  
018400     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
018410                                             MOD-TEMFSINF                 
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE                         TO MFS-KDTRTYP                  
019000       MOVE '7'                           TO MFS-IDPFK                    
019100     END-IF                                                               
019110                                                                          
019200     MOVE 'GB'                            TO MED-IDSKYLT                  
019400     .                                                                    
019500     EJECT                                                                
019510                                                                          
019600 B-CHECK-KEYS SECTION.                                                    
019800     MOVE ALL '+'            TO MSGI-WMSGINIT                             
019900     MOVE '001'              TO MSGI-KDCALL                               
020000     MOVE MSG-LTERM-NAME     TO MSGI-IDLTERM-USER                         
020100     MOVE MSG-SIGNON-USERID  TO MSGI-IDUSER                               
020200     MOVE '3164'             TO MSGI-IDTRANS                              
020300     IF GODK-MID                                                          
020410       IF MID-IDARTNR-IN = ALL '+'                                        
020420         MOVE MID-IDARTNR-UT TO WS-IDARTNR-9                              
020422       ELSE                                                               
020430         MOVE MID-IDARTNR-IN TO WS-IDARTNR-9                              
020440       END-IF                                                             
020450       MOVE WS-IDARTNR-9     TO MSGI-IDARTNR                              
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020610     MOVE MSGI-SPAR-AREA     TO SPAR-AREA                                 
020700                                                                          
020800     MOVE JA TO NYCKLAR-SW                                                
020900                                                                          
021002*    -- KONTROLL AV IDARTNR                                               
021003     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
021004                                                                          
021005     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021006       MOVE '7'              TO MFS-IDPFK                                 
021007       MOVE SPACE            TO MFS-KDTRTYP                               
021008     END-IF                                                               
021009     INSPECT MSGI-IDARTNR REPLACING ALL     SPACE BY ZERO                 
021010     IF MSGI-IDARTNR NUMERIC                                              
021011       MOVE MSGI-IDARTNR     TO TEST-IDARTNR                              
021012       IF NOT BYT03-OBJEKT AND NOT BYT02-RENOV                            
021013         MOVE NEJ            TO NYCKLAR-SW                                
021014       ELSE                                                               
021015         IF BYT02-RENOV                                                   
021016           IF BYT16-BYTES                                                 
021017             COMPUTE TEST-IDARTNR = TEST-IDARTNR +                        
021018                                    6000                                  
021019             END-COMPUTE                                                  
021020           ELSE                                                           
021021             COMPUTE TEST-IDARTNR = TEST-IDARTNR +                        
021022                                    1000                                  
021023             END-COMPUTE                                                  
021024           END-IF                                                         
021025         END-IF                                                           
021032         MOVE TEST-IDARTNR   TO W-IDARTNR-K6                              
021033                                W-IDARTNR-D3                              
021034                                W-IDARTNR-3176                            
021035                                W-IDARTNR-MIN                             
021036                                W-IDARTNR-MAX                             
021038       END-IF                                                             
021039     ELSE                                                                 
021040       MOVE NEJ              TO NYCKLAR-SW                                
021050     END-IF                                                               
021101                                                                          
021102     IF GODK-MID OR NYCKLAR-OK                                            
021104       MOVE MSGI-IDARTNR(2:8) TO MOD-IDARTNR-UT                           
021106       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
021107     ELSE                                                                 
021108       MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-UT                           
021110     END-IF                                                               
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021400       MOVE ERR-WRONG-KEY     TO MED-IDMFSFEL                             
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
021700       PERFORM MFS-CLEAN-FIELDS-IN                                        
021800       PERFORM MFS-CLEAN-FIELDS-OUT                                       
021900     END-IF                                                               
022000     .                                                                    
022200     EJECT                                                                
022300                                                                          
022310 C-FIRST-PAGE SECTION.                                                    
022330     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
022340     CALL WMEDKONV USING MED-WMEDAREA                                     
022350     MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                  
022360                                                                          
022370     PERFORM MFS-CLEAN-FIELDS-IN                                          
022380     .                                                                    
022390     EJECT                                                                
022391                                                                          
022392 D-NEXT-PAGE SECTION.                                                     
022394     IF SPAR-IDTRANS = '3164'                                             
022406       COMPUTE W-IDBYTRAP-9KOMPL-MIN =                                    
022407                          W-9KOMPL - SPAR-IDBYTRAP-NEXT                   
022408       MOVE SPAR-IDDISTR-NEXT  TO W-IDDISTR-MIN                           
022410       MOVE SPAR-IDBYTRAD-NEXT TO W-IDBYTRAD-MIN                          
022412                                  W-IDBYTRAD                              
022413       MOVE SPAR-IDFAKT-NEXT   TO W-IDFAKT-3172                           
022414       MOVE SPAR-IDKOLLI-NEXT  TO W-IDKOLLI-3174                          
022415       MOVE SPAR-IDARTNR-NEXT  TO W-IDARTNR-MIN                           
022416                                  W-IDARTNR-MAX                           
022417                                  W-IDARTNR-3176                          
022418     ELSE                                                                 
022419       PERFORM MFS-CLEAN-FIELDS-IN                                        
022420     END-IF                                                               
022421     .                                                                    
022422     EJECT                                                                
022423                                                                          
022430 E-SAME-PAGE SECTION.                                                     
022450     IF SPAR-IDTRANS = '3164' OR '0551'                                   
022460       COMPUTE W-IDBYTRAP-9KOMPL-MIN =                                    
022461                          W-9KOMPL - SPAR-IDBYTRAP-ENTER                  
022470       MOVE SPAR-IDDISTR-ENTER  TO W-IDDISTR-MIN                          
022490       MOVE SPAR-IDBYTRAD-ENTER TO W-IDBYTRAD-MIN                         
022500       MOVE SPAR-IDFAKT-ENTER   TO W-IDFAKT-3172                          
022510       MOVE SPAR-IDKOLLI-ENTER  TO W-IDKOLLI-3174                         
022520       MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR-MIN                           
022530                                  W-IDARTNR-MAX                           
022540                                  W-IDARTNR-3176                          
022592     ELSE                                                                 
022593       PERFORM MFS-CLEAN-FIELDS-IN                                        
022594     END-IF                                                               
022595     .                                                                    
022596     EJECT                                                                
023301                                                                          
023302 F-READ-SHOW-INFO SECTION.                                                
023319     PERFORM IMS-GU-WDD311-BSEQ                                           
023320     IF SEGMENT-FINNS                                                     
023330       MOVE TEXT-BEART               TO MOD-BEART-CORE                    
023340     ELSE                                                                 
023350       MOVE 'UNKNOWN'                TO MOD-BEART-CORE                    
023360     END-IF                                                               
023370                                                                          
023380     MOVE +1 TO INDX                                                      
025859     IF MFS-ENTER                                                         
025860       IF (MFS-ENTER  AND SPAR-IDFAKT-ENTER = ZERO )                      
025861         PERFORM IMS-GU-WDM6E1                                            
025867         PERFORM FA-BUILD-LINES                                           
025874         IF INDX < MAX-INDX                                               
025875           PERFORM IMS-GU-WDGX3171                                        
025876           IF SEGMENT-FINNS                                               
025877              PERFORM FB-BUILD-LINES                                      
025878           END-IF                                                         
025887         END-IF                                                           
025888       ELSE                                                               
025889         MOVE  JA  TO W-READ-WITH-KEY                                     
025891         PERFORM IMS-GU-WDGX3171                                          
025892         IF SEGMENT-FINNS                                                 
025893            PERFORM FB-BUILD-LINES                                        
025894         END-IF                                                           
025895       END-IF                                                             
025896     END-IF                                                               
025897                                                                          
025898     IF MFS-NEXT                                                          
025900       IF (MFS-NEXT   AND SPAR-IDFAKT-NEXT  = ZERO )                      
025901         PERFORM IMS-GN-WDM6E1                                            
025902*        PERFORM IMS-GU-WDM6E1                                            
025903         PERFORM FA-BUILD-LINES                                           
025913         IF INDX < MAX-INDX                                               
025914           PERFORM IMS-GU-WDGX3171                                        
025915           IF SEGMENT-FINNS                                               
025916              PERFORM FB-BUILD-LINES                                      
025917           END-IF                                                         
025926         END-IF                                                           
025927       ELSE                                                               
025928         MOVE  JA  TO W-READ-WITH-KEY                                     
025930         PERFORM IMS-GU-WDGX3171                                          
025931         IF SEGMENT-FINNS                                                 
025932            PERFORM FB-BUILD-LINES                                        
025933         END-IF                                                           
025934       END-IF                                                             
025935     END-IF                                                               
025936                                                                          
025937     IF MFS-FIRST                                                         
025939       MOVE ZERO                     TO SPAR-IDDISTR-NEXT                 
025940                                        SPAR-IDBYTRAP-NEXT                
025941                                        SPAR-IDBYTRAD-NEXT                
025942                                        SPAR-IDARTNR-NEXT                 
025943                                        SPAR-IDFAKT-NEXT                  
025944                                        SPAR-IDKOLLI-NEXT                 
025945                                        SPAR-IDDISTR-ENTER                
025946                                        SPAR-IDBYTRAP-ENTER               
025947                                        SPAR-IDBYTRAD-ENTER               
025948                                        SPAR-IDARTNR-ENTER                
025949                                        SPAR-IDFAKT-ENTER                 
025950                                        SPAR-IDKOLLI-ENTER                
025951       PERFORM IMS-GU-WDM6E1                                              
025953       PERFORM FA-BUILD-LINES                                             
025960       IF INDX <  MAX-INDX                                                
025966         PERFORM IMS-GU-WDGX3171                                          
025967         IF SEGMENT-FINNS                                                 
025968            PERFORM FB-BUILD-LINES                                        
025969         END-IF                                                           
025970       END-IF                                                             
025971     END-IF                                                               
025972                                                                          
025973     IF INDX < MAX-INDX                                                   
025974       ADD +1 TO INDX                                                     
025975       PERFORM UNTIL INDX > MAX-INDX                                      
025976         MOVE MFS-RENSA-FAELT        TO MOD-IDDISTR  (INDX)               
025977                                        MOD-DAREGDAT (INDX)               
025978                                        MOD-KVANTAL  (INDX)               
025979                                        MOD-DAANKDAG (INDX)               
025980                                        MOD-FLANK    (INDX)               
025981         ADD +1 TO INDX                                                   
025982       END-PERFORM                                                        
025983     END-IF                                                               
025984                                                                          
025985     MOVE '002'                      TO MSGI-KDCALL                       
025986     MOVE '3164'                     TO SPAR-IDTRANS                      
025987     MOVE SPAR-AREA                  TO MSGI-SPAR-AREA                    
025988     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
025989     .                                                                    
025990     EJECT                                                                
025991 FA-BUILD-LINES SECTION.                                                  
025992     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
025993                   SEGMENT-SLUT   OR                                      
025994                   INDX > MAX-INDX                                        
025995                                                                          
025996       IF SEQE-IDDISTR = 7512 OR 7625 OR 6121 OR 5619 OR 6251 OR          
                               6200 OR 6270                                     
025997                                                                          
025998         MOVE SEQE-IDDISTR             TO W-IDDISTR-M6                    
025999         COMPUTE W-IDBYTRAP-M6 = W-9KOMPL - SEQE-IDBYTRAP-9KOMPL          
026000*        PERFORM IMS-GU-WDM601                                            
026010         MOVE SEQE-IDBYTRAD            TO W-IDBYTRAD                      
026020                                                                          
026040         PERFORM IMS-GU-WDM611                                            
026050                                                                          
026060         IF RAPP-IDDISTR = 7512 OR                                        
026070                           7625 OR                                        
026070                           6121 OR                                        
026070                           5619 OR                                        
026070                           6251 OR                                        
026070                           6200 OR                                        
026070                           6270                                           
026090           IF RAPP-KDBYTSTA-RAPP < 4                                      
026091             IF INDX = 1                                                  
026092               MOVE RAPP-IDDISTR     TO SPAR-IDDISTR-ENTER                
026093               MOVE RAPP-IDBYTRAP    TO SPAR-IDBYTRAP-ENTER               
026094               MOVE OBJ-IDBYTRAD     TO SPAR-IDBYTRAD-ENTER               
026095               MOVE OBJ-IDARTNR-OBJ  TO SPAR-IDARTNR-ENTER                
026096               MOVE ZERO             TO SPAR-IDFAKT-ENTER                 
026097                                        SPAR-IDKOLLI-ENTER                
026098             END-IF                                                       
026099             MOVE RAPP-IDDISTR       TO MOD-IDDISTR  (INDX)               
026100             MOVE RAPP-DAREGDAT      TO MOD-DAREGDAT (INDX)               
026101             MOVE OBJ-KVRETUR-URSP   TO MOD-KVANTAL  (INDX)               
026102             IF RAPP-KDBYTSTA-RAPP = 3                                    
026103               MOVE RAPP-DAANKDAG    TO MOD-DAANKDAG (INDX)               
026104               MOVE '*'              TO MOD-FLANK    (INDX)               
026105             ELSE                                                         
026106               MOVE ZERO             TO MOD-DAANKDAG (INDX)               
026107               MOVE SPACE            TO MOD-FLANK    (INDX)               
026108               IF RAPP-DAREGDAT NOT = 99999999                            
026109                 PERFORM IMS-GU-WDK611                                    
026110                                                                          
026111                 MOVE RAPP-IDDISTR   TO W-IDDISTR-3158                    
026112                 MOVE CLAG-KDEXCHA   TO W-KDEXCHA-3158                    
026113                 PERFORM IMS-GU-WDGX3158                                  
026114                                                                          
026115                 IF SEGMENT-FINNS                                         
026116                   PERFORM S01-CALC-ETA                                   
026117                   MOVE WS-DAANKDAG  TO MOD-DAANKDAG (INDX)               
026118                 END-IF                                                   
026119               END-IF                                                     
026120             END-IF                                                       
026121             ADD +1 TO INDX                                               
026122           END-IF                                                         
026123         END-IF                                                           
026124                                                                          
026125       END-IF                                                             
026126       PERFORM IMS-GN-WDM6E1                                              
026128     END-PERFORM                                                          
026129                                                                          
026130     IF SEGMENT-FINNS                                                     
026131       MOVE SEQE-IDDISTR             TO SPAR-IDDISTR-NEXT                 
026132       COMPUTE W-IDBYTRAP-M6 = W-9KOMPL - SEQE-IDBYTRAP-9KOMPL            
026133       MOVE W-IDBYTRAP-M6            TO SPAR-IDBYTRAP-NEXT                
026134       MOVE SEQE-IDBYTRAD            TO SPAR-IDBYTRAD-NEXT                
026135       MOVE SEQE-IDARTNR             TO SPAR-IDARTNR-NEXT                 
026136       MOVE ZERO                     TO SPAR-IDFAKT-NEXT                  
026137                                        SPAR-IDKOLLI-NEXT                 
026144     ELSE                                                                 
026145       MOVE SPAR-IDDISTR-ENTER       TO SPAR-IDDISTR-NEXT                 
026146       MOVE SPAR-IDBYTRAP-ENTER      TO SPAR-IDBYTRAP-NEXT                
026147       MOVE SPAR-IDBYTRAD-ENTER      TO SPAR-IDBYTRAD-NEXT                
026148       MOVE SPAR-IDARTNR-ENTER       TO SPAR-IDARTNR-NEXT                 
026149       MOVE ZERO                     TO SPAR-IDFAKT-NEXT                  
026150                                        SPAR-IDKOLLI-NEXT                 
026151                                                                          
026152     END-IF                                                               
026153     .                                                                    
026154     EJECT                                                                
026155                                                                          
026156                                                                          
026157 FB-BUILD-LINES SECTION.                                                  
026158     IF READ-WITH-KEY                                                     
026159        PERFORM IMS-GNP-WDGX3172-KVAL                                     
026160     ELSE                                                                 
026161        PERFORM IMS-GNP-WDGX3172                                          
026162     END-IF                                                               
026163     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
026164                   INDX > MAX-INDX                                        
026165       IF SEGMENT-FINNS                                                   
026166         MOVE 3172-IDFAKT               TO W-IDFAKT-3172                  
026167         MOVE 3172-IDDC-SEND            TO WS-IDDC                        
026168         IF NDC-AU OR NDC-JP                                              
026169           IF READ-WITH-KEY                                               
026170              PERFORM IMS-GNP-WDGX3174-KVAL                               
026171           ELSE                                                           
026172              PERFORM IMS-GNP-WDGX3174                                    
026173           END-IF                                                         
026174           PERFORM UNTIL SEGMENT-SAKNAS OR                                
026175                         INDX > MAX-INDX                                  
026176             IF SEGMENT-FINNS                                             
026177                MOVE 3174-IDKOLLI             TO W-IDKOLLI-3174           
026178               IF 3174-KDTRSTAT < 4                                       
026179                 PERFORM IMS-GNP-WDGX3176                                 
026180                 PERFORM UNTIL SEGMENT-SAKNAS OR                          
026181                               INDX > MAX-INDX                            
026182                   IF SEGMENT-FINNS                                       
026183                     IF INDX = 1                                          
026184                       MOVE ZERO            TO SPAR-IDDISTR-ENTER         
026185                                               SPAR-IDBYTRAP-ENTER        
026186                                               SPAR-IDBYTRAD-ENTER        
026187                       MOVE 3172-IDFAKT     TO SPAR-IDFAKT-ENTER          
026188                       MOVE 3174-IDKOLLI    TO SPAR-IDKOLLI-ENTER         
026189                       MOVE 3176-IDARTNR-OBJ TO SPAR-IDARTNR-ENTER        
026190                     END-IF                                               
026191                     IF  INDX   <=  MAX-INDX                              
026192                       IF NDC-JP                                          
026193                          MOVE 5220          TO MOD-IDDISTR (INDX)        
026194                                               W-IDDISTR-3158             
026195                       ELSE                                               
026196                          MOVE 7835          TO MOD-IDDISTR (INDX)        
026197                                               W-IDDISTR-3158             
026198                       END-IF                                             
026199                       MOVE 3172-DASNDDAT   TO MOD-DAREGDAT (INDX)        
026200                       MOVE 3176-KVANTMOT   TO MOD-KVANTAL  (INDX)        
026201                       IF 3172-KDTRSTAT = 3                               
026202                         MOVE 3172-DAANKDAG TO MOD-DAANKDAG (INDX)        
026203                         MOVE '*'           TO MOD-FLANK    (INDX)        
026204                       ELSE                                               
026205                         PERFORM IMS-GU-WDK611                            
026206                                                                          
026207                         MOVE CLAG-KDEXCHA    TO W-KDEXCHA-3158           
026208                         PERFORM IMS-GU-WDGX3158                          
026209                                                                          
026210                         IF SEGMENT-FINNS                                 
026211                           PERFORM S01-CALC-ETA                           
026212                           MOVE WS-DAANKDAG TO MOD-DAANKDAG (INDX)        
026213                         ELSE                                             
026214                           MOVE ZERO        TO MOD-DAANKDAG (INDX)        
026215                         END-IF                                           
026216                         MOVE SPACE         TO MOD-FLANK    (INDX)        
026217                       END-IF                                             
026218                     END-IF                                               
026219                     ADD +1 TO INDX                                       
026220                     MOVE NEJ   TO  W-READ-WITH-KEY                       
026221                   END-IF                                                 
026222                   PERFORM IMS-GNP-WDGX3176                               
026223                 END-PERFORM                                              
026224               END-IF                                                     
026225             END-IF                                                       
026226             PERFORM IMS-GNP-WDGX3174                                     
026227           END-PERFORM                                                    
026228         END-IF                                                           
026229       END-IF                                                             
026230       PERFORM IMS-GNP-WDGX3172                                           
026231     END-PERFORM                                                          
026232                                                                          
026233     IF SEGMENT-FINNS                                                     
026234       MOVE ZERO                     TO SPAR-IDDISTR-NEXT                 
026235                                        SPAR-IDBYTRAP-NEXT                
026236                                        SPAR-IDBYTRAD-NEXT                
026237       MOVE 3172-IDFAKT              TO SPAR-IDFAKT-NEXT                  
026238       MOVE 3174-IDKOLLI             TO SPAR-IDKOLLI-NEXT                 
026239       MOVE 3176-IDARTNR-OBJ         TO SPAR-IDARTNR-NEXT                 
026240     ELSE                                                                 
026241       MOVE INF-LAST-PAGE TO MED-IDMFSINF                                 
026242       CALL WMEDKONV      USING MED-WMEDAREA                              
026243       MOVE MED-TEMFSINF  TO MOD-TEMFSINF                                 
026244                                                                          
026245       MOVE SPAR-IDDISTR-ENTER       TO SPAR-IDDISTR-NEXT                 
026246       MOVE SPAR-IDBYTRAP-ENTER      TO SPAR-IDBYTRAP-NEXT                
026247       MOVE SPAR-IDBYTRAD-ENTER      TO SPAR-IDBYTRAD-NEXT                
026248       MOVE SPAR-IDFAKT-ENTER        TO SPAR-IDFAKT-NEXT                  
026249       MOVE SPAR-IDKOLLI-ENTER       TO SPAR-IDKOLLI-NEXT                 
026250       MOVE SPAR-IDARTNR-ENTER       TO SPAR-IDARTNR-NEXT                 
026251     END-IF                                                               
026252     .                                                                    
026253     EJECT                                                                
026260                                                                          
026305 S01-CALC-ETA SECTION.                                                    
026306     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
026307     MOVE RAPP-DAREGDAT(3:6) TO DAT-I-TIDATUM                             
026308                                                                          
026309     CALL WDATKONV USING DAT-KDDATFORM                                    
026310                         DAT-I-TIDATUM                                    
026311                         DAT-O-TIDATUM                                    
026312                         DAT-KDSVAR                                       
026313                                                                          
026314     IF DAT-KDSVAR = ' '                                                  
026315       MOVE DAT-TIAAVV-GRP   TO WS-TIAAVV-NUM                             
026316     ELSE                                                                 
026317       DISPLAY '**** TIAAVV' DAT-I-TIDATUM                                
026318       MOVE +1000            TO RKOD-ABEND-MED-DUMP                       
026319       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
026320     END-IF                                                               
026321                                                                          
026322     MOVE WS-TIAAVV-NUM      TO W009VADD-DATUM                            
026323     MOVE 3158-KVVECKOR-BYRE TO W009VADD-ANTAL                            
026324                                                                          
026325     CALL W009VADD USING W009VADD-DATUM W009VADD-ANTAL                    
026326                                                                          
026327     MOVE 'AAVV'             TO DAT-KDDATFORM                             
026328     MOVE W009VADD-DATUM     TO DAT-I-TIDATUM                             
026329                                                                          
026330     CALL WDATKONV USING DAT-KDDATFORM                                    
026331                         DAT-I-TIDATUM                                    
026332                         DAT-O-TIDATUM                                    
026333                         DAT-KDSVAR                                       
026334                                                                          
026335     IF DAT-KDSVAR = ' '                                                  
026336       MOVE DAT-TIAAMMDD     TO WS-TIAAMMDD-NUM                           
026337     ELSE                                                                 
026338       DISPLAY '**** TIAAMMDD' DAT-I-TIDATUM                              
026339       MOVE +1000            TO RKOD-ABEND-MED-DUMP                       
026340       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
026341     END-IF                                                               
026342                                                                          
026343     MOVE 20000000           TO WS-DAANKDAG                               
026344     ADD  WS-TIAAMMDD-NUM    TO WS-DAANKDAG                               
026345     .                                                                    
026346     EJECT                                                                
026347                                                                          
026348 MFS-CLEAN-FIELDS-OUT SECTION.                                            
026349*    --- ALLA UTDATA-FÄLT                                                 
026350     MOVE MFS-RENSA-FAELT   TO MOD-BEART-CORE                             
026351                                                                          
026352     MOVE +1 TO INDX                                                      
026353     PERFORM UNTIL INDX > MAX-INDX                                        
026354       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR  (INDX)                        
026355                               MOD-DAREGDAT (INDX)                        
026356                               MOD-KVANTAL  (INDX)                        
026357                               MOD-DAANKDAG (INDX)                        
026358                               MOD-FLANK    (INDX)                        
026359       ADD +1 TO INDX                                                     
026360     END-PERFORM                                                          
026361     .                                                                    
026362                                                                          
026363 MFS-CLEAN-FIELDS-IN SECTION.                                             
026364*    --- ALLA INDATA-FÄLT                                                 
026370     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
026400     .                                                                    
027900     EJECT                                                                
027910                                                                          
029400* --- IMS SEKTIONER ---                                                   
029600 IMS-GET-MSG SECTION.                                                     
029800     MOVE '  QC'          TO GODK-STATUSKODER                             
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300                                                                          
030400 IMS-INSERT-MSG SECTION.                                                  
030600     IF MSGI-IDLAND-SPR = 'GB'                                            
030700       MOVE 'N'           TO MFS-KDHUVOMR                                 
030800     END-IF                                                               
030900     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
031000     MOVE SPACE           TO GODK-STATUSKODER                             
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502                                                                          
031690 IMS-GU-WDD311-BSEQ SECTION.                                              
031693     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-D3-X ')'                      
031694     DELIMITED BY SIZE INTO SSA1                                          
031695     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-D3-X ')'                      
031696     DELIMITED BY SIZE INTO SSA2                                          
031697     MOVE '  GE'            TO GODK-STATUSKODER                           
031698     CALL CBLTDLI USING GU WDD3B-PCB DLI-IO-WDD311 SSA1 SSA2              
031699     MOVE WDD3B-STATUS-CODE TO STATUS-WS                                  
031700     PERFORM IMS-STATUSKONTROLL                                           
031701     .                                                                    
031710     EJECT                                                                
031760                                                                          
031761 IMS-GU-WDM6E1 SECTION.                                                   
031762                                                                          
031763     STRING 'WDM6E1  (WDM6E1KY>=' W-WDM6E1KY-MIN ')'                      
031764                    '&WDM6E1KY<=' W-WDM6E1KY-MAX ')'                      
031765          DELIMITED BY SIZE INTO SSA1                                     
031766     MOVE '  GE' TO GODK-STATUSKODER                                      
031767     CALL CBLTDLI USING GU WDM6E-PCB DLI-IO-WDM6E1 SSA1                   
031768     MOVE WDM6E-STATUS-CODE TO STATUS-WS                                  
031769     PERFORM IMS-STATUSKONTROLL                                           
031770     .                                                                    
031771     EJECT                                                                
031772 IMS-GN-WDM6E1 SECTION.                                                   
031773                                                                          
031774     STRING 'WDM6E1  (WDM6E1KY>=' W-WDM6E1KY-MIN                          
031775                    '&WDM6E1KY<=' W-WDM6E1KY-MAX ')'                      
031776          DELIMITED BY SIZE INTO SSA1                                     
031777     MOVE '  GEGB' TO GODK-STATUSKODER                                    
031778     CALL CBLTDLI USING GN WDM6E-PCB DLI-IO-WDM6E1 SSA1                   
031779     MOVE WDM6E-STATUS-CODE TO STATUS-WS                                  
031780     PERFORM IMS-STATUSKONTROLL                                           
031781     .                                                                    
031782     EJECT                                                                
031832 IMS-GU-WDM601 SECTION.                                                   
031833                                                                          
031834     STRING 'WDM601  (WDM601KY =' W-WDM601KY-X ')'                        
031835          DELIMITED BY SIZE INTO SSA1                                     
031838     MOVE '  GE' TO GODK-STATUSKODER                                      
031839     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-WDM601 SSA1                    
031840     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
031841     PERFORM IMS-STATUSKONTROLL                                           
031842     .                                                                    
031843     SKIP2                                                                
031844 IMS-GU-WDM611 SECTION.                                                   
031845                                                                          
031846     STRING 'WDM601  *D(WDM601KY =' W-WDM601KY-X ')'                      
031847          DELIMITED BY SIZE INTO SSA1                                     
031848     STRING 'WDM611  (IDBYTRAD =' W-IDBYTRAD-X ')'                        
031849          DELIMITED BY SIZE INTO SSA2                                     
031850     MOVE '  GE' TO GODK-STATUSKODER                                      
031851     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-WDM6 SSA1 SSA2                 
031852     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
031853     PERFORM IMS-STATUSKONTROLL                                           
031854     .                                                                    
031855     SKIP2                                                                
031865 IMS-GU-WDK611 SECTION.                                                   
031866     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
031867          DELIMITED BY SIZE INTO SSA1                                     
031868     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-K6-X ')'                     
031869          DELIMITED BY SIZE INTO SSA2                                     
031870     MOVE '  '             TO GODK-STATUSKODER                            
031871     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
031872     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
031873     PERFORM IMS-STATUSKONTROLL                                           
031874     .                                                                    
031875     EJECT                                                                
031876                                                                          
031877 IMS-GU-WDGX3158 SECTION.                                                 
031878     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-3157-X ')'                    
031879          DELIMITED BY SIZE INTO SSA1                                     
031880     STRING 'WDGX3158(KEY3158  =' W-KEY3158-X ')'                         
031881          DELIMITED BY SIZE INTO SSA2                                     
031882     MOVE '  GE'           TO GODK-STATUSKODER                            
031883     CALL CBLTDLI USING GU 3157-PCB DLI-IO-WDGX3158 SSA1 SSA2             
031884     MOVE 3157-STATUS-CODE TO STATUS-WS                                   
031885     PERFORM IMS-STATUSKONTROLL                                           
031886     .                                                                    
031887     EJECT                                                                
031888                                                                          
031889 IMS-GU-WDGX3171 SECTION.                                                 
031890     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3171-X ')'                    
031891          DELIMITED BY SIZE INTO SSA1                                     
031892     MOVE '  '             TO GODK-STATUSKODER                            
031893     CALL CBLTDLI USING GU  3171-PCB DLI-IO-WDGX3171 SSA1                 
031894     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
031895     PERFORM IMS-STATUSKONTROLL                                           
031896     .                                                                    
031897     EJECT                                                                
031898                                                                          
031899 IMS-GNP-WDGX3172 SECTION.                                                
031900     MOVE 'WDGX3172  '     TO SSA1                                        
031901     MOVE '  GE'           TO GODK-STATUSKODER                            
031902     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3172 SSA1                 
031903     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
031904     PERFORM IMS-STATUSKONTROLL                                           
031905     .                                                                    
031906     EJECT                                                                
031907                                                                          
031908 IMS-GNP-WDGX3172-KVAL SECTION.                                           
031909     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-3172-X ')'                     
031910          DELIMITED BY SIZE INTO SSA1                                     
031911     MOVE '  GE'           TO GODK-STATUSKODER                            
031912     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3172 SSA1                 
031913     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
031914     PERFORM IMS-STATUSKONTROLL                                           
031915     .                                                                    
031916     EJECT                                                                
031917                                                                          
031918 IMS-GNP-WDGX3174 SECTION.                                                
031919     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-3172-X ')'                     
031920          DELIMITED BY SIZE INTO SSA1                                     
031921     MOVE 'WDGX3174 '       TO SSA2                                       
031922     MOVE '  GE'           TO GODK-STATUSKODER                            
031923     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3174 SSA1 SSA2            
031924     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
031925     PERFORM IMS-STATUSKONTROLL                                           
031926     .                                                                    
031927     EJECT                                                                
031928                                                                          
031929 IMS-GNP-WDGX3174-KVAL SECTION.                                           
031930     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-3172-X ')'                     
031931          DELIMITED BY SIZE INTO SSA1                                     
031932     STRING 'WDGX3174(IDKOLLI  =' W-IDKOLLI-3174-X ')'                    
031933          DELIMITED BY SIZE INTO SSA2                                     
031934     MOVE '  GE'           TO GODK-STATUSKODER                            
031935     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3174 SSA1 SSA2            
031936     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
031937     PERFORM IMS-STATUSKONTROLL                                           
031938     .                                                                    
031939     EJECT                                                                
031940                                                                          
031941 IMS-GNP-WDGX3176 SECTION.                                                
031942     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-3172-X ')'                     
031943          DELIMITED BY SIZE INTO SSA1                                     
031944     STRING 'WDGX3174(IDKOLLI  =' W-IDKOLLI-3174-X ')'                    
031945          DELIMITED BY SIZE INTO SSA2                                     
031946     STRING 'WDGX3176(IDARTNRO =' W-IDARTNR-3176-X ')'                    
031947          DELIMITED BY SIZE INTO SSA3                                     
031948     MOVE '  GE'           TO GODK-STATUSKODER                            
031949     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3176 SSA1 SSA2            
031950                                                     SSA3                 
031951     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
031952     PERFORM IMS-STATUSKONTROLL                                           
031953     .                                                                    
031954     EJECT                                                                
031973                                                                          
031974 IMS-STATUSKONTROLL SECTION.                                              
031980     SET STATUS-IX TO 1                                                   
032000     SEARCH GODK-STATUS                                                   
032100       AT END                                                             
032200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032300         DELIMITED BY SIZE INTO FELTEXT                                   
032400         CALL FELLOG                                                      
032500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    
032900     EJECT                                                                

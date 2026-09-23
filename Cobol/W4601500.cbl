001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4601500.                                                
001300 AUTHOR.         GERRY CARMICHAEL..                                       
001400 DATE-WRITTEN.   98/09/03.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700                                                                          
001800*    FUNKTION:                                                            
001900*    LÄGGER UPP TRANSAR FÖR NDC ORDERBEKRÄFTELSER FRÅN VIPS               
002000*    PÅ DISPATCHERN FÖR VIDARE BEHANDLING I W40253.                       
002100*                                                                         
002201*    DATABASER: UPPDATERAR   KOMMUNIKATIONS DB                            
002202*                            WLKOMA-(WDP8)                                
002203*               UPPDATERAR   HÄNDELSE REGISTER (CHKPOINT)                 
002204*                            WL4579-(WDR4)                                
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003101     SKIP2                                                                
003102*          --- INFIL                                                      
003110     SELECT W46012                     ASSIGN TO W46015D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003701     SKIP3                                                                
003702 FD  W46012                                                               
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY W460RHN      -L.                                               
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W4601500'.            
004200 01  CHKP-VAR.                                                            
004300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004800     03 CHKP-MAX                 PIC S9(3)   VALUE +3   COMP-3.           
004810 77  POST-ANT                    PIC S9(3)   VALUE +0   COMP-3.           
004820 77  RAD-IX                      PIC S9(4)   VALUE +0  COMP SYNC.         
004830 77  MAX-RAD-IX                  PIC S9(4)   VALUE +14 COMP SYNC.         
004840 77  SPAR-IDDISTR                PIC 9(4)    VALUE ZERO.                  
004850 77  SPAR-IDKUNDNR               PIC 9(6)    VALUE ZERO.                  
004860 77  SPAR-IDORDNR                PIC 9(7)    VALUE ZERO.                  
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005001 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
005010                                                                          
005020 77  W46012-EOF-SW               PIC X       VALUE 'N'.                   
005030     88  END-OF-W46012                       VALUE 'J'.                   
005100     SKIP2                                                                
005110                                                                          
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005910*                                                                         
006000 01  W-DATUM                     PIC 9(6)    VALUE ZERO.                  
006001 01  W-TIKLOCK                   PIC 9(8)    VALUE ZERO.                  
006500     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007020     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
007030     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  IN-AREA-START               PIC X(24)   VALUE                        
007403                                             'IN-AREA-START'.             
007404     SKIP2                                                                
007405                                                                          
007410*01  AREA -COPY W460RHN     -PRE IN-                                      
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007910 01  FILLER          PIC X(16)   VALUE 'NYCKLAR TILL DLI'.                
008100     SKIP3                                                                
008110*01  -COPY WDGX01                                                         
008120     EJECT                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008800     88  IMS-EJ-OK                           VALUE 'XD'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100                                                                          
010201 01  FILLER         PIC X(16) VALUE '4580-IO-AREA'.                       
010202 01  4580-IO-AREA.                                                        
010210*    03  -COPY WDGX4580                                                   
010300                                                                          
010700     EJECT                                                                
010710 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
010720*01  -COPY WMSGKOM                                                        
010730     EJECT                                                                
010740                                                                          
010750 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
010760     SKIP3                                                                
010770*01  -COPY WMSGAREA                                                       
010780     EJECT                                                                
010790*                                                                         
010791*    --- AREOR FÖR W006KOM SUBMODUL                                       
010792*                                                                         
010793 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
010794 01  KOM-IO-AREA.                                                         
010795   03  KOM-AREA                     PIC X(1000) VALUE SPACE.              
010796*03  FILLER  -COPY W4I25301   -RED KOM-AREA.                              
010800                                                                          
010810 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0009   -PRE MSG-                                              
011101                                                                          
011102 01  DISP-PCB                PIC X.                                       
011103 01  KOMA-PCB                PIC X.                                       
011104                                                                          
011108*01  -COPY W0008  -PRE 4579-                                              
011110     05  FILLER              PIC X.                                       
011400     EJECT                                                                
011501 PROCEDURE DIVISION  USING MSG-PCB                                        
011502                           DISP-PCB                                       
011503                           KOMA-PCB                                       
011504                           4579-PCB.                                      
011505 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB                                        
011520                           DISP-PCB                                       
011530                           KOMA-PCB                                       
011540                           4579-PCB.                                      
011600                                                                          
011801     PERFORM A-INITIERA                                                   
011811                                                                          
011820     PERFORM IMS-LAS-ATERSTART                                            
011821                                                                          
011823     IF SEGMENT-SAKNAS                                                    
011824        MOVE SPACE        TO 4580-WDGX4580-CTX                            
011825        MOVE '1'          TO 4580-KDSEGKEY                                
011826        MOVE ZERO         TO 4580-KVPOST                                  
011827        MOVE W-DATUM      TO 4580-TIUPPDAT                                
011828        MOVE W-TIKLOCK    TO 4580-TIUPPTID                                
011829                                                                          
011830        PERFORM IMS-ISRT-ATERSTART                                        
011831        PERFORM IMS-LAS-ATERSTART                                         
011832     END-IF                                                               
011833                                                                          
011834     IF 4580-KVPOST > +0                                                  
011840        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
011850     ELSE                                                                 
011860        PERFORM S01-LAS-W46012                                            
011870     END-IF                                                               
011871                                                                          
011880     IF NOT END-OF-W46012                                                 
011890        MOVE IN-RHN-IDDISTR TO SPAR-IDDISTR                               
011891        MOVE IN-RHN-IDKUNDNR TO SPAR-IDKUNDNR                             
011892        MOVE IN-RHN-IDORDNR TO SPAR-IDORDNR                               
011893        PERFORM S03-SKAPA-MSG-KOM-AREA                                    
011894     END-IF                                                               
011895                                                                          
011896     PERFORM UNTIL END-OF-W46012                                          
011897                                                                          
011898        PERFORM C-BEARBETA                                                
011899        PERFORM S01-LAS-W46012                                            
011906                                                                          
011907     END-PERFORM                                                          
011908                                                                          
011909     PERFORM Z-FINIT                                                      
011910     MOVE ZERO TO RETURN-CODE                                             
011920     GOBACK                                                               
011930     .                                                                    
011940     EJECT                                                                
011950                                                                          
011960 A-INITIERA SECTION.                                                      
011971                                                                          
011972     PERFORM IMS-RESTART                                                  
011973                                                                          
011980     OPEN INPUT W46012                                                    
011990                                                                          
011991     MOVE +0                   TO POST-ANT                                
011992                                  CHKP-ANT                                
011993                                  RAD-IX                                  
011994                                  SPAR-IDDISTR                            
011995                                  SPAR-IDKUNDNR                           
011996                                  SPAR-IDORDNR                            
011997                                                                          
011998     MOVE SPACE                TO MSG-AREA                                
011999                                                                          
012000     ACCEPT W-DATUM            FROM DATE                                  
012001     ACCEPT W-TIKLOCK          FROM TIME                                  
012002                                                                          
012003     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
012004     .                                                                    
012005     EJECT                                                                
012006 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
012007                                                                          
012008     PERFORM S01-LAS-W46012                                               
012009                                                                          
012010     PERFORM UNTIL END-OF-W46012  OR                                      
012011                     POST-ANT = 4580-KVPOST                               
012012        PERFORM S01-LAS-W46012                                            
012014        ADD +1           TO POST-ANT                                      
012017     END-PERFORM                                                          
012018                                                                          
012019     IF END-OF-W46012                                                     
012020        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
012021                      TO FELTEXT                                          
012022        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
012023     END-IF                                                               
012024     .                                                                    
012025     EJECT                                                                
012026                                                                          
012027 C-BEARBETA SECTION.                                                      
012044                                                                          
012047     IF SPAR-IDDISTR  = IN-RHN-IDDISTR  AND                               
012048        SPAR-IDKUNDNR = IN-RHN-IDKUNDNR AND                               
012049        SPAR-IDORDNR  = IN-RHN-IDORDNR  AND                               
012050        RAD-IX NOT > MAX-RAD-IX                                           
012051       CONTINUE                                                           
012052     ELSE                                                                 
012053       PERFORM S05-AVSLUTA-TRANS                                          
012054       PERFORM X-TAG-CHECKPOINT                                           
012055       MOVE IN-RHN-IDDISTR  TO SPAR-IDDISTR                               
012056       MOVE IN-RHN-IDKUNDNR TO SPAR-IDKUNDNR                              
012057       MOVE IN-RHN-IDORDNR  TO SPAR-IDORDNR                               
012058       PERFORM S03-SKAPA-MSG-KOM-AREA                                     
012059     END-IF                                                               
012060                                                                          
012061     IF IN-RHN-IDPTYP = 'RHN'                                             
012062       PERFORM CA-SKAPA-OBKR-TRANS                                        
012063     ELSE                                                                 
012064       MOVE 'FELAKTIG POSTTTYP PÅ INFILEN W46012'                         
012065                      TO FELTEXT                                          
012066       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
012067     END-IF                                                               
012069                                                                          
012070     .                                                                    
012071     EJECT                                                                
012080                                                                          
012081                                                                          
012082 CA-SKAPA-OBKR-TRANS SECTION.                                             
012083                                                                          
012084     ADD +1 TO RAD-IX                                                     
012085                                                                          
012086     IF RAD-IX = +1                                                       
012087                                                                          
012088       MOVE SPACE                TO KOM-AREA                              
012089                                                                          
012090       COMPUTE MSG-KVLL = LENGTH OF MID-W4I25301-CTX + 17                 
012091                                                                          
012092       MOVE LOW-VALUE            TO MSG-KDZ1                              
012093       MOVE LOW-VALUE            TO MSG-KDZ2                              
012094       MOVE 'W4T253X '           TO MSG-KDTRANS-1                         
012095       MOVE '4253'               TO MSG-IDTRANS-1                         
012096       MOVE '1'                  TO MSG-KDMFSFOR-1                        
012097                                                                          
012098       MOVE 'VIPS'               TO MID-IDSYSTEM                          
012100       MOVE IN-RHN-IDDISTR       TO MID-IDDISTR                           
012101       MOVE IN-RHN-IDKUNDNR      TO MID-IDKUNDNR                          
012102       MOVE IN-RHN-IDORDNR       TO MID-IDORDNR                           
012103       MOVE IN-RHN-KDORDKL       TO MID-KDORDKL                           
012104       MOVE NEJ                  TO MID-FLSLUT                            
012105     END-IF                                                               
012110                                                                          
012120     MOVE IN-RHN-IDARTNR         TO MID-IDARTNR   (RAD-IX)                
012130     MOVE IN-RHN-IDLOPNR         TO MID-IDLOPNR   (RAD-IX)                
012140     MOVE IN-RHN-IDSEKVNR        TO MID-IDSEKVNR  (RAD-IX)                
012141     MOVE SPACE                  TO MID-IDDC      (RAD-IX)                
012142     MOVE IN-RHN-KDORDBEK        TO MID-KDORDBEK  (RAD-IX)                
012143     MOVE IN-RHN-KVBEART         TO MID-KVBEART   (RAD-IX)                
012144     MOVE IN-RHN-BEART           TO MID-BEART-USA (RAD-IX)                
012145                                                                          
012146     .                                                                    
012150     EJECT                                                                
012180                                                                          
012200 S01-LAS-W46012 SECTION.                                                  
012201     SKIP2                                                                
012202     READ W46012 INTO IN-AREA                                             
012203       AT END                                                             
012204          MOVE JA TO W46012-EOF-SW                                        
012205     END-READ                                                             
012206                                                                          
012207     IF NOT END-OF-W46012                                                 
012208        MOVE 'W46012'       TO POSTSUM-FDNAMN                             
012209        MOVE 'W46012D1'     TO POSTSUM-DDNAMN2                            
012210        MOVE IN-RHN-IDPTYP  TO POSTSUM-TRANSTYP                           
012211        CALL POSTSUM USING POSTSUM-PARM                                   
012212     END-IF                                                               
012213     .                                                                    
012214     EJECT                                                                
012215                                                                          
012224 S03-SKAPA-MSG-KOM-AREA SECTION.                                          
012225     SKIP2                                                                
012226     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
012227     MOVE +54                    TO MSG-KOM-KVLL                          
012228     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
012229     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
012230     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
012234     MOVE 'W4I25301'             TO MSG-KOM-IDCPYTXT                      
012238     MOVE 'W460RHN '             TO MSG-KOM-IDSNDNOD                      
012240     MOVE 'W4601500'             TO MSG-KOM-IDSNDJOB                      
012241     MOVE W-DATUM                TO MSG-KOM-TIREGDAT                      
012242     ADD +1                      TO W-TIKLOCK                             
012243     MOVE W-TIKLOCK              TO MSG-KOM-TIKLOCK                       
012244     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
012245     .                                                                    
012246     EJECT                                                                
012247                                                                          
012248 S05-AVSLUTA-TRANS SECTION.                                               
012249     SKIP2                                                                
012250                                                                          
012260     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
012270     CALL W006KOM USING MSG-PCB                                           
012271                        DISP-PCB                                          
012272                        KOMA-PCB                                          
012273                        MSG-KOM-WMSGKOM                                   
012274                        MSG-IO-AREA                                       
012275     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
012276*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
012277*       DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                 
012278        MOVE ' FELAKTIG DATUM,TID PÅ INPUTFIL W46012 '                    
012279                      TO FELTEXT                                          
012280        DISPLAY ' FELAKTIG DATUM,TID INPUTFIL W46012 '                    
012281        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
012282     END-IF                                                               
012285                                                                          
012286     MOVE ZERO     TO RAD-IX                                              
012287     MOVE SPACE    TO KOM-AREA                                            
012288     .                                                                    
012289     EJECT                                                                
012290                                                                          
012291 X-TAG-CHECKPOINT SECTION.                                                
012292     SKIP2                                                                
012293*    UPPDATERA ÅTERSTARTREGISTRET                                         
012294     PERFORM IMS-LAS-ATERSTART                                            
012295     ADD +1          TO 4580-KVPOST                                       
012296     ACCEPT 4580-TIUPPDAT FROM DATE                                       
012297     ACCEPT 4580-TIUPPTID FROM TIME                                       
012298                                                                          
012299     PERFORM IMS-REPL-ATERSTART                                           
012300                                                                          
012301*    TAG CHECKPOINT                                                       
012302     PERFORM IMS-CHECKPOINT                                               
012303     .                                                                    
012304     EJECT                                                                
012305                                                                          
012306 Z-FINIT    SECTION.                                                      
012307                                                                          
012308     IF SPAR-IDDISTR  NOT = ZERO AND                                      
012309        SPAR-IDKUNDNR NOT = ZERO AND                                      
012310        SPAR-IDORDNR  NOT = ZERO                                          
012311       PERFORM S05-AVSLUTA-TRANS                                          
012312     END-IF                                                               
012313                                                                          
012314     CLOSE  W46012                                                        
012315                                                                          
012316*    NOLLA ÅTERSTARTINFORMATIONEN                                         
012317     PERFORM IMS-LAS-ATERSTART                                            
012318     MOVE +0         TO 4580-KVPOST                                       
012319     ACCEPT 4580-TIUPPDAT FROM DATE                                       
012320     ACCEPT 4580-TIUPPTID FROM TIME                                       
012321                                                                          
012322     PERFORM IMS-REPL-ATERSTART                                           
012323                                                                          
012324     MOVE 'S'      TO POSTSUM-OPKOD                                       
012325     CALL POSTSUM USING POSTSUM-PARM                                      
012326     .                                                                    
012327     EJECT                                                                
012328                                                                          
012329* IMS SECTIONER                                                           
012330     SKIP3                                                                
012331                                                                          
012332 IMS-RESTART SECTION.                                                     
012333     SKIP2                                                                
012334     MOVE SPACE TO MSG-IO-AREA                                            
012335     MOVE '  ' TO GODK-STATUSKODER                                        
012336     CALL CBLTDLI USING XRST MSG-PCB                                      
012337                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
012338                        CHKP-AREA-LENGTH CHKP-AREA                        
012339     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
012340     PERFORM IMS-STATUSKONTROLL                                           
012341     .                                                                    
012342                                                                          
012343 IMS-CHECKPOINT SECTION.                                                  
012344     MOVE SPACE        TO MSG-IO-AREA                                     
012345     MOVE '  XD'       TO GODK-STATUSKODER                                
012346     CALL CBLTDLI USING CHKP MSG-PCB                                      
012347                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
012348                        CHKP-AREA-LENGTH CHKP-AREA                        
012349     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
012350     PERFORM IMS-STATUSKONTROLL                                           
012351     IF IMS-EJ-OK                                                         
012352       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
012353       MOVE ' IMS-KONTROLLREGION EJ TILLGÄNGLIG '                         
012354                            TO FELTEXT                                    
012355       CALL FELLOG                                                        
012356     END-IF                                                               
012357     .                                                                    
012358                                                                          
012359 IMS-LAS-ATERSTART SECTION.                                               
012360     SKIP2                                                                
012361     MOVE '4579'         TO IDHTYP                                        
012362     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
012363     MOVE 'W4601500'     TO NYCKEL-VALFRI(1:8)                            
012364     STRING 'WL457901(WDGXKEY  =' WDGX01 ')'                              
012365                    DELIMITED BY SIZE INTO SSA1                           
012366     MOVE 'WL457911 '    TO SSA2                                          
012367     MOVE '  GE'           TO GODK-STATUSKODER                            
012368     CALL CBLTDLI USING GHU 4579-PCB 4580-IO-AREA SSA1 SSA2               
012369     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
012370     PERFORM IMS-STATUSKONTROLL                                           
012371     .                                                                    
012372                                                                          
012373 IMS-ISRT-ATERSTART SECTION.                                              
012374     SKIP2                                                                
012375     MOVE '4579'         TO IDHTYP                                        
012376     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
012377     MOVE 'W4601500'     TO NYCKEL-VALFRI(1:8)                            
012378     STRING 'WL457901(WDGXKEY  =' WDGX01 ')'                              
012379                    DELIMITED BY SIZE INTO SSA1                           
012380     MOVE 'WL457911 '    TO SSA2                                          
012381     MOVE '  '           TO GODK-STATUSKODER                              
012382     CALL CBLTDLI USING ISRT 4579-PCB 4580-IO-AREA SSA1 SSA2              
012383     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
012384     PERFORM IMS-STATUSKONTROLL                                           
012385     .                                                                    
012386                                                                          
012387 IMS-REPL-ATERSTART SECTION.                                              
012388     SKIP2                                                                
012389     MOVE '  '             TO GODK-STATUSKODER                            
012390     CALL CBLTDLI USING REPL 4579-PCB 4580-IO-AREA                        
012391     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
012392     PERFORM IMS-STATUSKONTROLL                                           
012393     .                                                                    
012394                                                                          
012395     EJECT                                                                
012396 IMS-STATUSKONTROLL SECTION.                                              
012397     SET STATUS-IX TO 1                                                   
012398     SEARCH GODK-STATUS                                                   
012399       AT END                                                             
012400         MOVE ' STATUSKOD FRÅN IMS EJ TILLÅTEN '                          
012401                            TO FELTEXT                                    
012402         CALL FELLOG                                                      
012403       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
012404         CONTINUE                                                         
012410     END-SEARCH                                                           
012500     .                                                                    

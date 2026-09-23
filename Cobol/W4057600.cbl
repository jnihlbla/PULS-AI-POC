001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W4057600.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/04/27.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        FRÅGEBILD RESTORDER                                              
002100*                                                                         
002210*        PROGRAMMET LÄSER      WLORDP (WDA5A)                             
002220*                              WDB6                                       
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W4T576                                              
002600*        MID:         W4I57601                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W4O57601                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W4057600'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004401*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004402 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004410 77  MAX-INDX                    PIC S9(4)  VALUE +15   COMP SYNC.        
004420                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '4576'.                
005600     88  GODK-MID                            VALUE '4571' '4572'          
005700                                                   '4573' '4574'          
005800                                                   '4575' '4576'          
005900                                                   '4577' '4578'          
006000                                                   '4579'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200     EJECT                                                                
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007601     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007610     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  ART-MISSING             PIC X(3)    VALUE '017'.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008501     EJECT                                                                
008502*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008503*                                                                         
008504 01  SPAR-AREA.                                                           
008505     03  SPAR-IDTRANS              PIC X(4)    VALUE '4576'.              
008506     03  SPAR-WDA5-ENTER.                                                 
008509         05  SPAR-KDRAPRIO-ENTER   PIC S9(3) VALUE ZERO COMP-3.           
008510         05  SPAR-DARODAT-ENTER    PIC  9(8) VALUE ZERO.                  
008511         05  SPAR-IDDISTR-ENTER    PIC S9(5) VALUE ZERO COMP-3.           
008512         05  SPAR-IDKUNDNR-ENTER   PIC S9(7) VALUE ZERO COMP-3.           
008513         05  SPAR-IDKUNDRF-ENTER   PIC X(10) VALUE SPACE.                 
008514         05  SPAR-IDLOPNR-ENTER    PIC S9(3) VALUE ZERO COMP-3.           
008520     03  SPAR-WDA5-NEXT.                                                  
008550         05  SPAR-KDRAPRIO-NEXT    PIC S9(3) VALUE ZERO COMP-3.           
008560         05  SPAR-DARODAT-NEXT     PIC  9(8) VALUE ZERO.                  
008570         05  SPAR-IDDISTR-NEXT     PIC S9(5) VALUE ZERO COMP-3.           
008580         05  SPAR-IDKUNDNR-NEXT    PIC S9(7) VALUE ZERO COMP-3.           
008590         05  SPAR-IDKUNDRF-NEXT    PIC X(10) VALUE SPACE.                 
008591         05  SPAR-IDLOPNR-NEXT     PIC S9(3) VALUE ZERO COMP-3.           
008600     EJECT                                                                
008610*    --- GENERELLA ARBETSAREROR                                           
008620 01  WS-IDDISTR                    PIC X(5)    VALUE ZERO.                
008630 01  WS-DARODAT                    PIC X(8)    VALUE ZERO.                
008640 01  WS-TIRES                      PIC X(7)    VALUE ZERO.                
008670     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W4I57601                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W4O57601                                                 
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011001*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
011002     03  W-WDA5ASEQ-MIN-X.                                                
011003         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
011004         05  W-IDDC-MIN          PIC X(2)  VALUE SPACE.                   
011006         05  FILLER              PIC X(2)  VALUE SPACE.                   
011007                                                                          
011008     03  W-WDA5ASEQ-MAX-X.                                                
011009         05  W-IDARTNR-MAX       PIC S9(9) VALUE ZERO COMP-3.             
011010         05  W-IDDC-MAX          PIC X(2)  VALUE SPACE.                   
011012         05  FILLER              PIC X(2)  VALUE SPACE.                   
011013                                                                          
011014     03  W-WDA5ASEQ-X.                                                    
011015         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
011016         05  W-IDDC              PIC X(2)  VALUE SPACE.                   
011017         05  W-KDRAPRIO          PIC S9(3) VALUE ZERO COMP-3.             
011091     SKIP2                                                                
011092     03  W-DARODAT-X.                                                     
011094         05  W-DARODAT           PIC  9(8) VALUE ZERO.                    
011095     SKIP2                                                                
011096     03  W-IDDISTR-X.                                                     
011097         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
011098     SKIP2                                                                
011099     03  W-IDKUNDNR-X.                                                    
011100         05  W-IDKUNDNR          PIC S9(7) VALUE ZERO COMP-3.             
011101     SKIP2                                                                
011102     03  W-IDKUNDRF-X.                                                    
011103         05  W-IDKUNDRF          PIC X(10) VALUE SPACE.                   
011104     SKIP2                                                                
011105     03  W-IDLOPNR-X.                                                     
011106         05  W-IDLOPNR           PIC S9(3) VALUE ZERO COMP-3.             
011107     SKIP2                                                                
011110     03  W-KDSTARAD-X.                                                    
011120         05  W-KDSTARAD          PIC X     VALUE SPACE.                   
011130     SKIP2                                                                
011140     03  W-KDSTARAD-EJ-TPO.                                               
011150         05  W-KDSTARAD-XTRA     PIC X     VALUE '1'.                     
011151                                                                          
011152     03  W-IDDC-B6-X.                                                     
011153         05 W-IDDC-B6                  PIC X(2).                          
011154                                                                          
011160     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(112).                              
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013350 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLORDP01'.                    
013360 01  DLI-IO-WLORDP01.                                                     
013370*    03  -COPY WDA501                                                     
013371                                                                          
013372 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013373 01   DLI-IO-AREA-B601.                                                   
013374*     03  -COPY WDB601                                                    
013375                                                                          
013380     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801                                                                          
014003*01  -COPY W0008  -PRE ORDP-                                              
014004     05  FILLER                  PIC X.                                   
014005                                                                          
014006*01  -COPY W0008  -PRE WDB6-                                              
014007     05  FILLER                  PIC X.                                   
014008     EJECT                                                                
014009 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
014010                           ORDP-PCB WDB6-PCB.                             
014011 MAIN SECTION.                                                            
014012     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
014020                           ORDP-PCB WDB6-PCB.                             
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014901           IF MFS-FIRST                                                   
014902             PERFORM C-FOERSTA-SIDA                                       
014903           ELSE                                                           
014904             IF MFS-NEXT                                                  
014905               PERFORM D-NAESTA-SIDA                                      
014906             ELSE                                                         
014907               PERFORM E-SAMMA-SIDA                                       
014908             END-IF                                                       
014910           END-IF                                                         
015200             PERFORM F-LAES-VISA-INFO                                     
015300       END-IF                                                             
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O57601 + 4                      
015700       PERFORM IMS-INSERT-MSG                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I57601                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I57601                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W4O576N1' TO MFS-IDMOD                                         
018300     MOVE '4576' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019110                                                                          
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019704                                                                          
019710     MOVE JA TO NYCKLAR-SW                                                
019712     MOVE LOW-VALUE           TO W-WDA5ASEQ-MIN-X                         
019713     MOVE HIGH-VALUE          TO W-WDA5ASEQ-MAX-X                         
019720                                                                          
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '4576'            TO MSGI-IDTRANS                               
020201                                                                          
020202*---FLYTTAR IN EN NOLLA I STATUSFÄLTET NÄR MAN HAR BLANKAT                
020203*---UT DET. GÖRS FÖR ATT MAN INTE SKALL FÅ NYCKLAR-FEL                    
020210     IF MID-KDSTARAD-IN = SPACE                                           
020220       MOVE ZERO TO MID-KDSTARAD-IN                                       
020230     END-IF                                                               
020240                                                                          
020300     IF EGEN-MID                                                          
020410         MOVE MID-IDARTNR-IN      TO MSGI-IDARTNR                         
020420         MOVE MID-IDDC-IN         TO MSGI-IDDC-KEY                        
020430         MOVE MID-KDSTARAD-IN     TO MSGI-KDSTARAD                        
020440     ELSE                                                                 
020450       IF MID-IDARTNR-IN NUMERIC                                          
020460         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
020470       END-IF                                                             
020471       MOVE MID-IDDC-IN      TO W-IDDC-B6                                 
020472       PERFORM IMS-GU-WDB601                                              
020480       IF DCS-KDDC = SPACE OR DCS-DDC                                     
020481         CONTINUE                                                         
020482       ELSE                                                               
020490         MOVE MID-IDDC-IN TO MSGI-IDDC-KEY                                
020491       END-IF                                                             
020492       IF MID-KDSTARAD-IN NUMERIC                                         
020493         MOVE MID-KDSTARAD-IN TO MSGI-KDSTARAD                            
020494       END-IF                                                             
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020620     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
020700     MOVE MSGI-IDLAND-SPR     TO MED-IDSKYLT                              
021008                                                                          
021009*    -- KONTROLL AV NYCKLAR                                               
021013                                                                          
021014     MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-IN                             
021015                               MOD-IDDC-IN                                
021016                               MOD-KDSTARAD-IN                            
021017                                                                          
021018     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021019       MOVE '7'         TO MFS-IDPFK                                      
021020       MOVE SPACE       TO MFS-KDTRTYP                                    
021021     END-IF                                                               
021022                                                                          
021023     IF MID-IDDC-IN NOT = ALL '+'                                         
021024       MOVE '7'         TO MFS-IDPFK                                      
021025       MOVE SPACE       TO MFS-KDTRTYP                                    
021030     END-IF                                                               
021031                                                                          
021034     IF MID-KDSTARAD-IN NOT = ALL '+'                                     
021035       MOVE '7'         TO MFS-IDPFK                                      
021036       MOVE SPACE       TO MFS-KDTRTYP                                    
021037     END-IF                                                               
021038                                                                          
021039     IF MSGI-IDARTNR NUMERIC AND MSGI-IDARTNR > ZERO                      
021040       MOVE MSGI-IDARTNR    TO W-IDARTNR                                  
021041                               W-IDARTNR-MIN                              
021042                               W-IDARTNR-MAX                              
021043     ELSE                                                                 
021044       MOVE NEJ             TO NYCKLAR-SW                                 
021045     END-IF                                                               
021046                                                                          
021047     IF DCS-IDDC NOT = MSGI-IDDC-KEY                                      
021048        MOVE MSGI-IDDC-KEY    TO W-IDDC-B6                                
021049        PERFORM IMS-GU-WDB601                                             
021050     END-IF                                                               
021051     IF DCS-KDDC = SPACE OR DCS-DDC                                       
021052       MOVE NEJ             TO NYCKLAR-SW                                 
021053     ELSE                                                                 
021054       MOVE MSGI-IDDC-KEY   TO W-IDDC                                     
021060                               W-IDDC-MIN                                 
021061                               W-IDDC-MAX                                 
021090     END-IF                                                               
021100                                                                          
021108     IF MSGI-KDSTARAD NUMERIC                                             
021109       IF MSGI-KDSTARAD > 1 AND MSGI-KDSTARAD < 5                         
021110         OR MSGI-KDSTARAD = 0                                             
021111         MOVE MSGI-KDSTARAD   TO W-KDSTARAD                               
021112       ELSE                                                               
021113         MOVE NEJ           TO NYCKLAR-SW                                 
021114       END-IF                                                             
021115     ELSE                                                                 
021116       MOVE NEJ           TO NYCKLAR-SW                                   
021117     END-IF                                                               
021118                                                                          
021119     IF GODK-MID OR NYCKLAR-OK                                            
021120       MOVE MSGI-IDARTNR     TO MOD-IDARTNR-UT                            
021121       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
021122                                                                          
021123       MOVE MSGI-IDDC-KEY   TO MOD-IDDC-UT                                
021124       INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                
021125                                                                          
021126       MOVE MSGI-KDSTARAD   TO MOD-KDSTARAD-UT                            
021127       INSPECT MOD-KDSTARAD-UT REPLACING LEADING ZERO BY SPACE            
021128                                                                          
021129     ELSE                                                                 
021130       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
021131                               MOD-IDDC-UT                                
021132                               MOD-KDSTARAD-UT                            
021140     END-IF                                                               
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021310*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
021320*---GODKÄND BILD                                                          
021330       IF GODK-MID                                                        
021400         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
021500         CALL WMEDKONV USING MED-WMEDAREA                                 
021600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
021610         MOVE +1 TO INDX                                                  
021620         PERFORM UNTIL INDX > MAX-INDX                                    
021630           PERFORM MFS-RENSA-FAELT-UT                                     
021640           ADD +1 TO INDX                                                 
021650         END-PERFORM                                                      
021660       END-IF                                                             
021900     END-IF                                                               
022000     .                                                                    
022101     EJECT                                                                
022102 C-FOERSTA-SIDA SECTION.                                                  
022103                                                                          
022104     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022105     CALL WMEDKONV USING MED-WMEDAREA                                     
022106     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
022107                                                                          
022109     .                                                                    
022110     EJECT                                                                
022111 D-NAESTA-SIDA SECTION.                                                   
022113     IF SPAR-IDTRANS = '4576'                                             
022115       MOVE SPAR-KDRAPRIO-NEXT  TO W-KDRAPRIO                             
022116       MOVE SPAR-DARODAT-NEXT   TO W-DARODAT                              
022117       MOVE SPAR-IDDISTR-NEXT   TO W-IDDISTR                              
022118       MOVE SPAR-IDKUNDNR-NEXT  TO W-IDKUNDNR                             
022119       MOVE SPAR-IDKUNDRF-NEXT  TO W-IDKUNDRF                             
022120       MOVE SPAR-IDLOPNR-NEXT   TO W-IDLOPNR                              
022122     END-IF                                                               
022123     .                                                                    
022124     EJECT                                                                
022125 E-SAMMA-SIDA SECTION.                                                    
022126                                                                          
022127     IF SPAR-IDTRANS = '4576' OR '0551'                                   
022129       MOVE SPAR-KDRAPRIO-ENTER  TO W-KDRAPRIO                            
022130       MOVE SPAR-DARODAT-ENTER   TO W-DARODAT                             
022131       MOVE SPAR-IDDISTR-ENTER   TO W-IDDISTR                             
022132       MOVE SPAR-IDKUNDNR-ENTER  TO W-IDKUNDNR                            
022133       MOVE SPAR-IDKUNDRF-ENTER  TO W-IDKUNDRF                            
022134       MOVE SPAR-IDLOPNR-ENTER   TO W-IDLOPNR                             
022135     END-IF                                                               
022136     .                                                                    
022137     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022540     IF MFS-NEXT OR MFS-ENTER                                             
022550       PERFORM IMS-GU-RAD                                                 
022560     ELSE                                                                 
022570       IF MSGI-KDSTARAD > 1                                               
022580         PERFORM IMS-GET-KDSTARAD                                         
022590       ELSE                                                               
022600         PERFORM IMS-GET-RAD                                              
022610       END-IF                                                             
022700     END-IF                                                               
022710                                                                          
022800     IF SEGMENT-SAKNAS                                                    
022900        MOVE W-WDA5ASEQ-MIN-X TO SPAR-WDA5-ENTER                          
023000        MOVE ART-MISSING  TO MED-IDMFSFEL                                 
023100        CALL WMEDKONV USING MED-WMEDAREA                                  
023200        MOVE MED-MFSFEL   TO MOD-TEMFSFEL                                 
023300        MOVE +1 TO INDX                                                   
023400        PERFORM MFS-RENSA-FAELT-UT                                        
023410     ELSE                                                                 
023420      MOVE +1 TO INDX                                                     
023430      PERFORM UNTIL INDX > MAX-INDX                                       
023434        IF SEGMENT-FINNS                                                  
023453          IF INDX = 1                                                     
023456            MOVE RAD-KDRAPRIO TO SPAR-KDRAPRIO-ENTER                      
023457                                 SPAR-KDRAPRIO-NEXT                       
023458            MOVE RAD-DARODAT  TO SPAR-DARODAT-ENTER                       
023459                                 SPAR-DARODAT-NEXT                        
023460            MOVE RAD-IDDISTR  TO SPAR-IDDISTR-ENTER                       
023461                                 SPAR-IDDISTR-NEXT                        
023462            MOVE RAD-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                      
023463                                 SPAR-IDKUNDNR-NEXT                       
023464            MOVE RAD-IDKUNDRF TO SPAR-IDKUNDRF-ENTER                      
023465                                 SPAR-IDKUNDRF-NEXT                       
023466            MOVE RAD-IDLOPNR  TO SPAR-IDLOPNR-ENTER                       
023467                                 SPAR-IDLOPNR-NEXT                        
023468          END-IF                                                          
023469          PERFORM FB-FLYTTA-DATA-TILL-MOD                                 
023470        ELSE                                                              
023471          PERFORM MFS-RENSA-UT-RAD                                        
023472        END-IF                                                            
023473                                                                          
023474        IF MSGI-KDSTARAD > 1                                              
023475          PERFORM IMS-GET-KDSTARAD                                        
023476        ELSE                                                              
023477          PERFORM IMS-GET-RAD                                             
023478        END-IF                                                            
023479      END-PERFORM                                                         
023480                                                                          
023481       IF SEGMENT-FINNS                                                   
023482         MOVE RAD-KDRAPRIO TO SPAR-KDRAPRIO-NEXT                          
023483         MOVE RAD-DARODAT  TO SPAR-DARODAT-NEXT                           
023484         MOVE RAD-IDDISTR  TO SPAR-IDDISTR-NEXT                           
023485         MOVE RAD-IDKUNDNR TO SPAR-IDKUNDNR-NEXT                          
023486         MOVE RAD-IDKUNDRF TO SPAR-IDKUNDRF-NEXT                          
023487         MOVE RAD-IDLOPNR  TO SPAR-IDLOPNR-NEXT                           
023488         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
023489         CALL WMEDKONV USING MED-WMEDAREA                                 
023490         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
023494       END-IF                                                             
023495                                                                          
023496       MOVE '002'      TO MSGI-KDCALL                                     
023497       MOVE '4576'   TO SPAR-IDTRANS                                      
023498       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
023499       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023500     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
023801 FB-FLYTTA-DATA-TILL-MOD SECTION.                                         
023802                                                                          
023810     MOVE RAD-IDDISTR      TO WS-IDDISTR                                  
023820     MOVE WS-IDDISTR(2:4)  TO MOD-IDDISTR (INDX)                          
023821     INSPECT MOD-IDDISTR (INDX) REPLACING LEADING ZERO BY SPACE           
023822                                                                          
023823     MOVE RAD-DARODAT      TO WS-DARODAT                                  
023824     MOVE WS-DARODAT(3:6)  TO MOD-TIRODAT (INDX)                          
023826                                                                          
023827     MOVE RAD-TIRES        TO WS-TIRES                                    
023828     MOVE WS-TIRES(2:6)    TO MOD-TIRES (INDX)                            
023829     IF RAD-TIRES = ZERO                                                  
023830       INSPECT MOD-TIRES (INDX) REPLACING LEADING ZERO BY SPACE           
023831     ELSE                                                                 
023832       CONTINUE                                                           
023834     END-IF                                                               
023835                                                                          
023840     MOVE RAD-KVART        TO MOD-KVART (INDX)                            
023841     INSPECT MOD-KVART (INDX) REPLACING LEADING ZERO BY SPACE             
023842                                                                          
023843     MOVE RAD-KDRAPRIO     TO MOD-KDRAPRIO (INDX)                         
023844     INSPECT MOD-KDRAPRIO (INDX) REPLACING LEADING ZERO BY SPACE          
023845                                                                          
023846     MOVE RAD-IDORDNR5     TO MOD-IDORDNR5 (INDX)                         
023847     INSPECT MOD-IDORDNR5 (INDX) REPLACING LEADING ZERO BY SPACE          
023848                                                                          
023849     MOVE RAD-IDKUNDRF-LEV TO MOD-IDKUNDRF-LEV (INDX)                     
023850     INSPECT MOD-IDKUNDRF-LEV (INDX)                                      
023851                             REPLACING LEADING ZERO BY SPACE              
023852                                                                          
023853     MOVE RAD-IDKUNDNR     TO MOD-IDKUNDNR (INDX)                         
023854     MOVE RAD-KDSTARAD     TO MOD-KDSTARAD (INDX)                         
023870     MOVE RAD-KDORDKL      TO MOD-KDORDKL (INDX)                          
023891                                                                          
023892     ADD +1 TO INDX                                                       
023900     .                                                                    
024000     EJECT                                                                
024800 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025000*    --- ALLA UTDATA-FÄLT                                                 
025110*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025120     PERFORM UNTIL INDX > MAX-INDX                                        
025130       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR (INDX)                         
025140                               MOD-IDKUNDNR (INDX)                        
025150                               MOD-TIRODAT (INDX)                         
025160                               MOD-KVART (INDX)                           
025170                               MOD-KDSTARAD (INDX)                        
025180                               MOD-IDORDNR5 (INDX)                        
025190                               MOD-KDORDKL (INDX)                         
025200                               MOD-KDRAPRIO (INDX)                        
025300                               MOD-TIRES (INDX)                           
025301                               MOD-IDKUNDRF-LEV (INDX)                    
025310       ADD +1 TO INDX                                                     
025320     END-PERFORM                                                          
025400     .                                                                    
025501     SKIP3                                                                
025502 MFS-RENSA-UT-RAD SECTION.                                                
025503                                                                          
025504     MOVE MFS-RENSA-FAELT   TO MOD-IDDISTR (INDX)                         
025505                               MOD-IDKUNDNR (INDX)                        
025506                               MOD-TIRODAT (INDX)                         
025507                               MOD-KVART (INDX)                           
025508                               MOD-KDSTARAD (INDX)                        
025509                               MOD-IDORDNR5 (INDX)                        
025510                               MOD-KDORDKL (INDX)                         
025511                               MOD-KDRAPRIO (INDX)                        
025512                               MOD-TIRES (INDX)                           
025513                               MOD-IDKUNDRF-LEV (INDX)                    
025514     ADD +1 TO INDX                                                       
025515     .                                                                    
025516     SKIP3                                                                
029400* --- IMS SEKTIONER ---                                                   
029500     SKIP3                                                                
029600 IMS-GET-MSG SECTION.                                                     
029700                                                                          
029800     MOVE '  QC' TO GODK-STATUSKODER                                      
029900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030100     PERFORM IMS-STATUSKONTROLL                                           
030200     .                                                                    
030300     SKIP3                                                                
030400 IMS-INSERT-MSG SECTION.                                                  
030500                                                                          
030600*    IF MSGI-IDLAND-SPR = 'SE'                                            
030700*      MOVE '0' TO MFS-KDHUVOMR                                           
030800*    END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GET-RAD SECTION.                                                     
031503                                                                          
031505     STRING 'WLORDP01(WDA5ASEQ>=' W-WDA5ASEQ-MIN-X                        
031506                    '&WDA5ASEQ<=' W-WDA5ASEQ-MAX-X                        
031507                    '&KDSTARAD >' W-KDSTARAD-EJ-TPO ')'                   
031508          DELIMITED BY SIZE INTO SSA1                                     
031509     MOVE '  GE' TO GODK-STATUSKODER                                      
031510     CALL CBLTDLI USING GN ORDP-PCB DLI-IO-WLORDP01 SSA1                  
031511     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
031512     PERFORM IMS-STATUSKONTROLL                                           
031520     .                                                                    
031600     EJECT                                                                
031610 IMS-GET-KDSTARAD SECTION.                                                
031620                                                                          
031630     STRING 'WLORDP01(WDA5ASEQ>=' W-WDA5ASEQ-MIN-X                        
031640                    '&WDA5ASEQ<=' W-WDA5ASEQ-MAX-X                        
031641                    '&KDSTARAD =' W-KDSTARAD-X ')'                        
031650          DELIMITED BY SIZE INTO SSA1                                     
031660     MOVE '  GE' TO GODK-STATUSKODER                                      
031670     CALL CBLTDLI USING GN ORDP-PCB DLI-IO-WLORDP01 SSA1                  
031680     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
031690     PERFORM IMS-STATUSKONTROLL                                           
031691     .                                                                    
031692     EJECT                                                                
031693 IMS-GU-RAD SECTION.                                                      
031694                                                                          
031695     STRING 'WLORDP01(WDA5ASEQ =' W-WDA5ASEQ-X                            
031697                    '&DARODAT  =' W-DARODAT-X                             
031698                    '&IDDISTR  =' W-IDDISTR-X                             
031699                    '&IDKUNDNR =' W-IDKUNDNR-X                            
031700                    '&IDKUNDRF =' W-IDKUNDRF-X                            
031701                    '&IDLOPNR  =' W-IDLOPNR-X ')'                         
031702          DELIMITED BY SIZE INTO SSA1                                     
031703     MOVE '  GE' TO GODK-STATUSKODER                                      
031704     CALL CBLTDLI USING GU ORDP-PCB DLI-IO-WLORDP01 SSA1                  
031705     MOVE ORDP-STATUS-CODE TO STATUS-WS                                   
031706     PERFORM IMS-STATUSKONTROLL                                           
031707     .                                                                    
031708     EJECT                                                                
031709 IMS-GU-WDB601    SECTION.                                                
031710     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
031711          DELIMITED BY SIZE INTO SSA1                                     
031712     MOVE '  GE' TO GODK-STATUSKODER                                      
031713     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
031714     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
031715     PERFORM IMS-STATUSKONTROLL                                           
031716     IF SEGMENT-SAKNAS                                                    
031717         MOVE SPACE TO DCS-KDDC                                           
031718     END-IF                                                               
031719     .                                                                    
031720                                                                          
031730 IMS-STATUSKONTROLL SECTION.                                              
031800                                                                          
031900     SET STATUS-IX TO 1                                                   
032000     SEARCH GODK-STATUS                                                   
032100       AT END                                                             
032200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032300         DELIMITED BY SIZE INTO FELTEXT                                   
032400         CALL FELLOG                                                      
032500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032600         CONTINUE                                                         
032700     END-SEARCH                                                           
032800     .                                                                    

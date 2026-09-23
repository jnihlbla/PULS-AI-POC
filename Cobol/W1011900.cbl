001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W1011900.                                                
001500 AUTHOR.         GAVIN SMITH.                                             
001600 DATE-WRITTEN.   98/09/09.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        LOOK AT AND UPDATE WEEK PPRODUCT GROUP CHANGE REGISTER           
002100*     CHECKED BY WY2000                                                   
002201*        PROGRAMMET LÄSER      WL1117 (WDGX)                              
002210*        PROGRAMMET LÄSER      WLARTC (WDK6V)                             
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W1T119,W1T119U                                      
002600*        MID:         W1I11901                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W1O11901                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W1011900'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 77  PG-CHECK                    PIC XX      VALUE SPACE.                 
004401     88  GOOD-PG                             VALUE '11' '14' '15'         
004402                                                   '16' '17' '18'         
004403                                                   '19' '21' '24'         
004404                                                   '25' '26' '27'         
004405                                                   '29' '31' '32'         
004406                                                   '35' '36' '37'         
004407                                                   '38'.                  
004408                                                                          
004409*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004410 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004411 77  SPAR-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
004420 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700                                                                          
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005310 77  INPUT-SW                  PIC X       VALUE 'J'.                     
005320     88  INPUT-OK                          VALUE 'J'.                     
005330     88  INPUT-ERROR                       VALUE 'N'.                     
005340                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '1119'.                
005600     88  GODK-MID                            VALUE                        
005700                                                                          
005800                                                                          
005900                                                   '1117'                 
006000                                                   '1119'.                
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
007800     03  NOT-VALID-PG            PIC X(3)    VALUE '109'.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008501     EJECT                                                                
008502*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008503*                                                                         
008504 01  SPAR-AREA .                                                          
008505     03  SPAR-IDTRANS           PIC X(4)    VALUE '1119'.                 
008506     03  SPAR-IDARTNR-ENTER       PIC S9(9) VALUE ZERO  COMP-3.           
008510     03  SPAR-IDARTNR-NEXT        PIC S9(9) VALUE ZERO  COMP-3.           
008520     03  SPAR-IDARTV              PIC 9(9) OCCURS 10.                     
008530     03  SPAR-BEARTV              PIC X(25) OCCURS 10.                    
008540     03  SPAR-PRODA               PIC 9(2) OCCURS 10.                     
008550     03  SPAR-PRODN               PIC 9(2) OCCURS 10.                     
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W1I11901                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W1O11901                                                 
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
011001*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
011002     03  W-IDARTNR-MIN-X.                                                 
011003         05  W-IDARTNR-MIN     PIC S9(9)      VALUE ZERO  COMP-3.         
011004                                                                          
011005     03  W-WDGXKEY-X.                                                     
011006         05  FILLER              PIC X(4)     VALUE '1117'.               
011007         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
011008     03  W-IDARTNR-X.                                                     
011009         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011610     88  EOF-1117                            VALUE 'GA' 'GE' .            
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL111701'.                    
013002 01  DLI-IO-WL111701.                                                     
013003*    03  -COPY WDG201   -PRE 1117-                                        
013004     EJECT                                                                
013005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WL111711'.                    
013006 01  DLI-IO-WL111711.                                                     
013007*    03  -COPY WDGX1118 -PRE 1117-                                        
013008 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
013009 01  DLI-IO-WLARTC01.                                                     
013010*    03  -COPY WDK601  -PRE ARTC-                                         
013011     EJECT                                                                
013012 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
013013 01  DLI-IO-WLARTC11.                                                     
013020*    03  -COPY WDK611  -PRE ARTC-                                         
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801                                                                          
013802*01  -COPY W0008  -PRE 1117-                                              
013803     05  FILLER                  PIC X.                                   
013804                                                                          
013805*01  -COPY W0008  -PRE ARTC-                                              
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 1117-PCB ARTC-PCB.            
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 1117-PCB ARTC-PCB.            
014100                                                                          
014200     PERFORM IMS-GET-MSG                                                  
014210     IF SEGMENT-FINNS                                                     
014220       PERFORM A-INIT                                                     
014230       PERFORM B-KOLLA-NYCKLAR                                            
014240       IF NYCKLAR-OK                                                      
014250         IF MFS-UPDATE                                                    
014260           PERFORM G-KOLLA-INPUT                                          
014270           IF INPUT-OK                                                    
014280           PERFORM H-UPPDATERA                                            
014290           END-IF                                                         
014291         ELSE                                                             
014292           IF MFS-FIRST                                                   
014293             PERFORM C-FOERSTA-SIDA                                       
014294           ELSE                                                           
014295             IF MFS-NEXT                                                  
014296               PERFORM D-NAESTA-SIDA                                      
014297             ELSE                                                         
014298               PERFORM E-SAMMA-SIDA                                       
014299             END-IF                                                       
014300           END-IF                                                         
014301         END-IF                                                           
014302         PERFORM F-LAES-VISA-INFO                                         
014303       END-IF                                                             
014304       COMPUTE MSG-KVLL = LENGTH OF MOD-W1O11901 + 4                      
014305       PERFORM IMS-INSERT-MSG                                             
014306     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1O11901                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1O11901                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W1O119N1' TO MFS-IDMOD                                         
018300     MOVE '1119' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019200     MOVE 'GB'                 TO MED-IDSKYLT                             
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '1119'            TO MSGI-IDTRANS                               
020300     IF GODK-MID                                                          
020400       IF MID-IDARTNR-IN NOT = ALL '+'                                    
020401         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
020500       END-IF                                                             
020510     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020700     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
020800     MOVE JA TO NYCKLAR-SW                                                
020900                                                                          
021001                                                                          
021016*    -- KONTROLL AV IDARTNR                                               
021017     MOVE SPACE         TO MOD-IDARTNR-IN                                 
021018                                                                          
021019     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021020       MOVE '7'         TO MFS-IDPFK                                      
021021       MOVE SPACE       TO MFS-KDTRTYP                                    
021022       MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                                
021023     END-IF                                                               
021024     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
021025     IF MSGI-IDARTNR NUMERIC                                              
021026       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
021027                            W-IDARTNR-MIN                                 
021028                            SPAR-IDARTNR-ENTER                            
021029     ELSE                                                                 
021030       MOVE NEJ TO NYCKLAR-SW                                             
021040     END-IF                                                               
021101                                                                          
021102     IF NYCKLAR-OK                                                        
021103       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
021104       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
021105     ELSE                                                                 
021106       MOVE SPACE           TO MOD-IDARTNR-UT                             
021110     END-IF                                                               
021200***********************************************                           
021300     IF NYCKLAR-FEL                                                       
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021700       PERFORM MFS-RENSA-FAELT-IN                                         
021800       PERFORM MFS-RENSA-FAELT-UT                                         
021900     END-IF                                                               
022000     .                                                                    
022101     EJECT                                                                
022102 C-FOERSTA-SIDA SECTION.                                                  
022103                                                                          
022104     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022105     CALL WMEDKONV USING MED-WMEDAREA                                     
022106     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
022107                                                                          
022108     PERFORM MFS-RENSA-FAELT-IN                                           
022109     .                                                                    
022110     EJECT                                                                
022111 D-NAESTA-SIDA SECTION.                                                   
022112                                                                          
022113     IF SPAR-IDTRANS = '1119'                                             
022114       MOVE SPAR-IDARTNR-NEXT TO W-IDARTNR-MIN                            
022115     ELSE                                                                 
022116       PERFORM MFS-RENSA-FAELT-IN                                         
022117     END-IF                                                               
022118     .                                                                    
022119     EJECT                                                                
022120 E-SAMMA-SIDA SECTION.                                                    
022121                                                                          
022122     IF SPAR-IDTRANS = '1119' OR '0551'                                   
022123       MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR-MIN                           
022124       IF MID-ROW-IN = ALL '+'                                            
022125         PERFORM MFS-RENSA-FAELT-IN                                       
022126       ELSE                                                               
022127         MOVE INF-FIRST-PAGE TO MED-IDMFSINF                              
022128         CALL WMEDKONV USING MED-WMEDAREA                                 
022129         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
022130         PERFORM EA-MID-INDATA-TILL-MOD                                   
022131       END-IF                                                             
022132     ELSE                                                                 
022133       PERFORM MFS-RENSA-FAELT-IN                                         
022134     END-IF                                                               
022135     .                                                                    
022136     EJECT                                                                
022137 EA-MID-INDATA-TILL-MOD SECTION.                                          
022138                                                                          
022139* * * * * FÖR VARJE MID-FÄLT                                              
022140* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
022141* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
022142* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
022143     MOVE +1 TO INDX                                                      
022144     PERFORM UNTIL INDX > MAX-INDX                                        
022145       IF MID-UPDAT (INDX) NOT = ALL '+'                                  
022146        MOVE MID-UPDAT (INDX) TO  MOD-UPDAT (INDX)                        
022148        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-UPDAT-ATTR (INDX)              
022149       ELSE                                                               
022150        MOVE MFS-RENSA-FAELT TO MOD-UPDAT (INDX)                          
022151       END-IF                                                             
022152       IF MID-PRODB (INDX) NOT = ALL '+'                                  
022153        MOVE MID-PRODB (INDX) TO  MOD-PRODB (INDX)                        
022154        MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-PRODB-ATTR (INDX)              
022155       ELSE                                                               
022156        MOVE MFS-RENSA-FAELT TO MOD-PRODB (INDX)                          
022157       END-IF                                                             
022158       ADD +1 TO INDX                                                     
022159     END-PERFORM                                                          
022160     .                                                                    
022200     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022510     MOVE W-IDARTNR-MIN TO W-IDARTNR                                      
022511     MOVE W-IDARTNR-MIN TO SPAR-IDARTNR-ENTER                             
022512     MOVE +1 TO INDX                                                      
022513     IF W-IDARTNR > 0                                                     
022520       PERFORM  IMS-GU-ARTC-01                                            
022600       IF SEGMENT-SAKNAS                                                  
022610         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
022620         CALL WMEDKONV USING MED-WMEDAREA                                 
022630         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022640         PERFORM MFS-RENSA-FAELT-IN                                       
022650         PERFORM MFS-RENSA-FAELT-UT                                       
022700       ELSE                                                               
022702         MOVE    ARTC-ART-KDPRODSL  TO MOD-PRODA  (INDX)                  
022704                                      SPAR-PRODA  (INDX)                  
022705         MOVE    ARTC-ART-IDARTNR   TO MOD-IDARTV (INDX)                  
022706                                      SPAR-IDARTV (INDX)                  
022708     INSPECT MOD-IDARTV (INDX) REPLACING LEADING ZERO BY SPACE            
022709         MOVE    '       '          TO MOD-BEARTV (INDX)                  
022800         PERFORM  IMS-GU-1117-11                                          
023403         IF SEGMENT-SAKNAS                                                
023404           MOVE ZERO TO MOD-PRODN (INDX)                                  
023405                        SPAR-PRODN (INDX)                                 
023406**         MOVE MFS-BLANKA-UT-FAELT TO MOD-PRODN-ATTR (INDX)              
023408         ELSE                                                             
023409           MOVE 1117-1118-KDPRODSL TO MOD-PRODN (INDX)                    
023410                                     SPAR-PRODN (INDX)                    
023411         END-IF                                                           
023412         ADD +1 TO INDX                                                   
023413       END-IF                                                             
023414         PERFORM  IMS-GN1-1117-11                                         
023415         IF SEGMENT-FINNS                                                 
023416           MOVE 1117-1118-IDARTNR  TO MOD-IDARTV (INDX)                   
023417                                     SPAR-IDARTV (INDX)                   
023418     INSPECT MOD-IDARTV (INDX) REPLACING LEADING ZERO BY SPACE            
023419           MOVE 1117-1118-KDPRODSL TO MOD-PRODN  (INDX)                   
023420                                     SPAR-PRODN  (INDX)                   
023421           MOVE 1117-1118-IDARTNR  TO W-IDARTNR                           
023422           PERFORM  IMS-GU-ARTC-01                                        
023423           MOVE    ARTC-ART-KDPRODSL  TO MOD-PRODA  (INDX)                
023424                                        SPAR-PRODA  (INDX)                
023425           MOVE    '       '          TO MOD-BEARTV (INDX)                
023426                                        SPAR-BEARTV (INDX)                
023427         END-IF                                                           
023428         ADD +1 TO INDX                                                   
023429         PERFORM  IMS-GN-1117-11                                          
023430       PERFORM UNTIL INDX > MAX-INDX OR  EOF-1117                         
023431         IF SEGMENT-FINNS                                                 
023432           MOVE 1117-1118-IDARTNR  TO MOD-IDARTV (INDX)                   
023433                                     SPAR-IDARTV (INDX)                   
023434     INSPECT MOD-IDARTV (INDX) REPLACING LEADING ZERO BY SPACE            
023435           MOVE 1117-1118-KDPRODSL TO MOD-PRODN  (INDX)                   
023436                                     SPAR-PRODN  (INDX)                   
023437           MOVE 1117-1118-IDARTNR  TO W-IDARTNR                           
023438           PERFORM  IMS-GU-ARTC-01                                        
023439           MOVE    ARTC-ART-KDPRODSL TO MOD-PRODA  (INDX)                 
023440                                       SPAR-PRODA  (INDX)                 
023441           MOVE    '       '          TO MOD-BEARTV (INDX)                
023442                                        SPAR-BEARTV (INDX)                
023443         END-IF                                                           
023444         ADD +1 TO INDX                                                   
023445         PERFORM  IMS-GN-1117-11                                          
023446       END-PERFORM                                                        
023447       IF SEGMENT-FINNS                                                   
023448         MOVE ARTC-ART-IDARTNR TO SPAR-IDARTNR-NEXT                       
023449         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
023450         CALL WMEDKONV USING MED-WMEDAREA                                 
023451         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
023452       ELSE                                                               
023453*        MOVE ARTC-ART-IDARTNR TO SPAR-IDARTNR-NEXT                       
023454         CONTINUE                                                         
023455       END-IF                                                             
023456                                                                          
023457       MOVE '002'      TO MSGI-KDCALL                                     
023458       MOVE '1119'   TO SPAR-IDTRANS                                      
023459       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
023460       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023461     END-IF                                                               
023470                                                                          
023600     .                                                                    
023700     EJECT                                                                
024710 G-KOLLA-INPUT SECTION.                                                   
024711     MOVE JA TO INPUT-SW                                                  
024712     IF MID-ROW-IN = ALL '+'                                              
024713       MOVE INF-FIRST-PAGE       TO MED-IDMFSFEL                          
024714       MOVE 'GB'                 TO MED-IDSKYLT                           
024715       CALL WMEDKONV USING MED-WMEDAREA                                   
024716       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024717       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024718       MOVE NEJ TO INPUT-SW                                               
024719     ELSE                                                                 
024720       MOVE 1 TO INDX                                                     
024721       PERFORM UNTIL INDX > MAX-INDX OR INPUT-ERROR                       
024722        IF MID-UPDAT (INDX)  = 'D'                                        
024723          MOVE MFS-ALFA-FAELT-RAETT TO MOD-UPDAT-ATTR (INDX)              
024724        ELSE                                                              
024725          MOVE MID-PRODB (INDX) TO PG-CHECK                               
024726          IF MID-UPDAT (INDX)  = 'C' AND GOOD-PG                          
024727            MOVE MFS-NUM-FAELT-RAETT TO MOD-PRODB-ATTR (INDX)             
024731          ELSE                                                            
024732            IF (MID-UPDAT (INDX) = (ALL '+' OR SPACE OR  ' +' ))          
024733            AND (MID-PRODB (INDX) = ( ALL '+' OR SPACE OR ' +'))          
024737              MOVE MFS-NUM-FAELT-RAETT TO MOD-PRODB-ATTR (INDX)           
024738              MOVE MFS-ALFA-FAELT-RAETT TO MOD-UPDAT-ATTR (INDX)          
024739            ELSE                                                          
024740             PERFORM MFS-ROER-EJ-FAELT-UT                                 
024741             PERFORM MFS-ROER-EJ-FAELT-IN                                 
024742             PERFORM MFS-LAES-IN-IGEN                                     
024743             MOVE MFS-NUM-FAELT-FEL TO MOD-PRODB-ATTR (INDX)              
024744             MOVE MFS-ALFA-FAELT-FEL TO MOD-UPDAT-ATTR (INDX)             
024745             MOVE NOT-VALID-PG         TO MED-IDMFSFEL                    
024746             MOVE 'GB'                 TO MED-IDSKYLT                     
024747             CALL WMEDKONV USING MED-WMEDAREA                             
024748             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
024749             MOVE NEJ TO INPUT-SW                                         
024750            END-IF                                                        
024751          END-IF                                                          
024752        END-IF                                                            
024753        ADD +1 TO INDX                                                    
024754       END-PERFORM                                                        
024755     END-IF                                                               
024756     .                                                                    
024757     EJECT                                                                
024758 H-UPPDATERA SECTION.                                                     
024759                                                                          
024760     MOVE +1 TO INDX                                                      
024761     PERFORM UNTIL INDX > MAX-INDX                                        
024762                                                                          
024763      IF MID-UPDAT (INDX) =  'D'                                          
024764          MOVE SPAR-IDARTV (INDX)  TO W-IDARTNR                           
024765          PERFORM IMS-GHU-1117-11                                         
024766          PERFORM IMS-DLET-1117-11                                        
024768      END-IF                                                              
024769                                                                          
024770      IF MID-UPDAT (INDX) =  'C'                                          
024771        IF SPAR-PRODN (INDX) > 10                                         
024772          MOVE SPAR-IDARTV (INDX)  TO W-IDARTNR                           
024773          PERFORM IMS-GHU-1117-11                                         
024774          MOVE  MID-PRODB (INDX) TO  1117-1118-KDPRODSL                   
024775          PERFORM IMS-REPL-1117-11                                        
024776        ELSE                                                              
024777          PERFORM IMS-GU-1117-01                                          
024778          MOVE SPAR-IDARTV (INDX) TO 1117-1118-IDARTNR                    
024779          MOVE  MID-PRODB  (INDX) TO 1117-1118-KDPRODSL                   
024780          PERFORM IMS-ISRT-1117-11                                        
024781        END-IF                                                            
024782      END-IF                                                              
024783      ADD +1 TO INDX                                                      
024784     END-PERFORM                                                          
024785     PERFORM MFS-RENSA-FAELT-IN                                           
024786     .                                                                    
024790     EJECT                                                                
024800 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025000*    --- ALLA UTDATA-FÄLT                                                 
025110*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025200     MOVE MFS-RENSA-FAELT TO                                              
025300                             MOD-IDARTNR-UT                               
025301                                                                          
025302                                                                          
025303                                                                          
025304     MOVE INDX TO SPAR-INDX                                               
025305     MOVE +1 TO INDX                                                      
025306     PERFORM UNTIL INDX > MAX-INDX                                        
025307       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
025308       ADD +1 TO INDX                                                     
025309     END-PERFORM                                                          
025310     MOVE SPAR-INDX TO INDX                                               
025320     .                                                                    
025501     SKIP3                                                                
025502 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
025503                                                                          
025504*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
025505     MOVE MFS-RENSA-FAELT TO                                              
025506                             MOD-IDARTV (INDX)                            
025507                             MOD-BEARTV (INDX)                            
025508                             MOD-PRODA (INDX)                             
025509                             MOD-PRODN (INDX)                             
025510                                                                          
025520     .                                                                    
025600     SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025900*    --- ALLA INDATA-FÄLT                                                 
026000     MOVE MFS-RENSA-FAELT TO MOD-UPDAT (01) MOD-PRODB (01)                
026100                             MOD-UPDAT (02) MOD-PRODB (02)                
026110                             MOD-UPDAT (03) MOD-PRODB (03)                
026120                             MOD-UPDAT (04) MOD-PRODB (04)                
026130                             MOD-UPDAT (05) MOD-PRODB (05)                
026140                             MOD-UPDAT (06) MOD-PRODB (06)                
026150                             MOD-UPDAT (07) MOD-PRODB (07)                
026160                             MOD-UPDAT (08) MOD-PRODB (08)                
026170                             MOD-UPDAT (09) MOD-PRODB (09)                
026180                             MOD-UPDAT (10) MOD-PRODB (10)                
026200                                MOD-IDARTNR-IN                            
026210     .                                                                    
026300     EJECT                                                                
026400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026500                                                                          
026600*    --- ALLA UTDATA-FÄLT                                                 
026710*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
026800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
026900                               MOD-TEMFSINF                               
027000     MOVE INDX TO SPAR-INDX                                               
027001     MOVE +1 TO INDX                                                      
027002     PERFORM UNTIL INDX > MAX-INDX                                        
027003       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
027004       ADD +1 TO INDX                                                     
027005     END-PERFORM                                                          
027006     MOVE SPAR-INDX TO INDX                                               
027007     .                                                                    
027008     SKIP2                                                                
027009 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
027010                                                                          
027011*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027012     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTV (INDX)                          
027020                               MOD-BEARTV (INDX)                          
027030                               MOD-PRODA  (INDX)                          
027040                               MOD-PRODN  (INDX)                          
027100     .                                                                    
027200     SKIP3                                                                
027300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027400                                                                          
027500*    --- ALLA INDATA-FÄLT                                                 
027600*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-IN                             
027730     MOVE INDX TO SPAR-INDX                                               
027740     MOVE +1 TO INDX                                                      
027750     PERFORM UNTIL INDX > MAX-INDX                                        
027760       PERFORM MFS-ROER-EJ-RAD-FAELT-IN                                   
027770       ADD +1 TO INDX                                                     
027780     END-PERFORM                                                          
027790     MOVE SPAR-INDX TO INDX                                               
027791     .                                                                    
027792     SKIP2                                                                
027910 MFS-ROER-EJ-RAD-FAELT-IN  SECTION.                                       
027920                                                                          
027930*    --- INDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027940     MOVE MFS-ROER-EJ-FAELT TO MOD-UPDAT  (INDX)                          
027970                               MOD-PRODB  (INDX)                          
027980     .                                                                    
027990     SKIP3                                                                
028000 MFS-FORM-ATTR SECTION.                                                   
028100                                                                          
028200*    --- ALLA INDATA-FÄLT                                                 
028300     MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-IN-ATTR                       
028400                          MOD-UPDAT-ATTR (01) MOD-PRODB-ATTR (01)         
028410                          MOD-UPDAT-ATTR (02) MOD-PRODB-ATTR (02)         
028420                          MOD-UPDAT-ATTR (03) MOD-PRODB-ATTR (03)         
028430                          MOD-UPDAT-ATTR (04) MOD-PRODB-ATTR (04)         
028440                          MOD-UPDAT-ATTR (05) MOD-PRODB-ATTR (05)         
028450                          MOD-UPDAT-ATTR (06) MOD-PRODB-ATTR (06)         
028460                          MOD-UPDAT-ATTR (07) MOD-PRODB-ATTR (07)         
028470                          MOD-UPDAT-ATTR (08) MOD-PRODB-ATTR (08)         
028480                          MOD-UPDAT-ATTR (09) MOD-PRODB-ATTR (09)         
028490                          MOD-UPDAT-ATTR (10) MOD-PRODB-ATTR (10)         
028500     .                                                                    
028600     SKIP2                                                                
028700 MFS-LAES-IN-IGEN SECTION.                                                
028800                                                                          
028900*    --- ALLA INDATA-FÄLT                                                 
029000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-IN-ATTR                    
029010                          MOD-UPDAT-ATTR (01) MOD-PRODB-ATTR (01)         
029020                          MOD-UPDAT-ATTR (02) MOD-PRODB-ATTR (02)         
029030                          MOD-UPDAT-ATTR (03) MOD-PRODB-ATTR (03)         
029040                          MOD-UPDAT-ATTR (04) MOD-PRODB-ATTR (04)         
029050                          MOD-UPDAT-ATTR (05) MOD-PRODB-ATTR (05)         
029060                          MOD-UPDAT-ATTR (06) MOD-PRODB-ATTR (06)         
029070                          MOD-UPDAT-ATTR (07) MOD-PRODB-ATTR (07)         
029080                          MOD-UPDAT-ATTR (08) MOD-PRODB-ATTR (08)         
029090                          MOD-UPDAT-ATTR (09) MOD-PRODB-ATTR (09)         
029091                          MOD-UPDAT-ATTR (10) MOD-PRODB-ATTR (10)         
029100                                                                          
029200     .                                                                    
029300     EJECT                                                                
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
030600     IF MSGI-IDLAND-SPR = 'SE'                                            
030700       MOVE '0' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GU-1117-01 SECTION.                                                  
031503                                                                          
031504     STRING 'WL111701(WDGXKEY  =' W-WDGXKEY-X ')'                         
031505          DELIMITED BY SIZE INTO SSA1                                     
031508     MOVE '    ' TO GODK-STATUSKODER                                      
031509     CALL CBLTDLI USING GU 1117-PCB DLI-IO-WL111701 SSA1                  
031510     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
031511     PERFORM IMS-STATUSKONTROLL                                           
031512     .                                                                    
031513     EJECT                                                                
031514 IMS-GU-1117-11 SECTION.                                                  
031515                                                                          
031516     STRING 'WL111701(WDGXKEY  =' W-WDGXKEY-X ')'                         
031517          DELIMITED BY SIZE INTO SSA1                                     
031518     STRING 'WL111711(IDARTNR  =' W-IDARTNR-X ')'                         
031519          DELIMITED BY SIZE INTO SSA2                                     
031520     MOVE '  GE' TO GODK-STATUSKODER                                      
031521     CALL CBLTDLI USING GU 1117-PCB DLI-IO-WL111711 SSA1 SSA2             
031522     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
031523     PERFORM IMS-STATUSKONTROLL                                           
031524     .                                                                    
031525     EJECT                                                                
031526 IMS-ISRT-1117-11 SECTION.                                                
031527                                                                          
031528     STRING 'WL111701(WDGXKEY  =' W-WDGXKEY-X ')'                         
031529          DELIMITED BY SIZE INTO SSA1                                     
031530     MOVE 'WL111711' TO SSA2                                              
031531     MOVE '  II' TO GODK-STATUSKODER                                      
031532     CALL CBLTDLI USING ISRT 1117-PCB DLI-IO-WL111711 SSA1 SSA2           
031533     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
031534     PERFORM IMS-STATUSKONTROLL                                           
031535     .                                                                    
031536     EJECT                                                                
031537 IMS-GHU-1117-11 SECTION.                                                 
031538                                                                          
031539     STRING 'WL111701(WDGXKEY  =' W-WDGXKEY-X ')'                         
031540          DELIMITED BY SIZE INTO SSA1                                     
031541     STRING 'WL111711(IDARTNR  =' W-IDARTNR-X ')'                         
031542          DELIMITED BY SIZE INTO SSA2                                     
031543     MOVE '  GE' TO GODK-STATUSKODER                                      
031544     CALL CBLTDLI USING GHU 1117-PCB DLI-IO-WL111711 SSA1 SSA2            
031545     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
031546     PERFORM IMS-STATUSKONTROLL                                           
031547     .                                                                    
031548     EJECT                                                                
031549 IMS-REPL-1117-11 SECTION.                                                
031550                                                                          
031551*    MOVE   'WL111701 ' TO  SSA1                                          
031553*    MOVE   'WL111711 ' TO SSA2                                           
031555     MOVE '  GE' TO GODK-STATUSKODER                                      
031556     CALL CBLTDLI USING REPL 1117-PCB DLI-IO-WL111711                     
031557     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
031558     PERFORM IMS-STATUSKONTROLL                                           
031559     .                                                                    
031560     EJECT                                                                
031561 IMS-GN1-1117-11 SECTION.                                                 
031562                                                                          
031563     STRING 'WL111701(WDGXKEY  =' W-WDGXKEY-X ')'                         
031564          DELIMITED BY SIZE INTO SSA1                                     
031565     STRING 'WL111711(IDARTNR  >' W-IDARTNR-X ')'                         
031566          DELIMITED BY SIZE INTO SSA2                                     
031567     MOVE 'GAGE' TO GODK-STATUSKODER                                      
031568     CALL CBLTDLI USING GU 1117-PCB DLI-IO-WL111711 SSA1 SSA2             
031569     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
031570     PERFORM IMS-STATUSKONTROLL                                           
031571     .                                                                    
031572     EJECT                                                                
031573 IMS-GN-1117-11 SECTION.                                                  
031574                                                                          
031575     STRING 'WL111701(WDGXKEY  =' W-WDGXKEY-X ')'                         
031576          DELIMITED BY SIZE INTO SSA1                                     
031577     MOVE 'WL111711' TO SSA2                                              
031578     MOVE 'GAGE' TO GODK-STATUSKODER                                      
031579     CALL CBLTDLI USING GN 1117-PCB DLI-IO-WL111711 SSA1 SSA2             
031580     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
031581     PERFORM IMS-STATUSKONTROLL                                           
031582     .                                                                    
031583     EJECT                                                                
031584 IMS-DLET-1117-11 SECTION.                                                
031585     MOVE '  DJ' TO GODK-STATUSKODER                                      
031586     CALL CBLTDLI USING DLET 1117-PCB DLI-IO-WL111711                     
031587     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
031588     PERFORM IMS-STATUSKONTROLL                                           
031589     .                                                                    
031590     EJECT                                                                
031591                                                                          
031592 IMS-GET-1117-PG SECTION.                                                 
031593                                                                          
031594     STRING 'WL111711(IDARTNR =>' W-IDARTNR-X ')'                         
031595          DELIMITED BY SIZE INTO SSA1                                     
031596     MOVE '  GE' TO GODK-STATUSKODER                                      
031597     CALL CBLTDLI USING GNP 1117-PCB DLI-IO-WL111711 SSA1                 
031598     MOVE 1117-STATUS-CODE TO STATUS-WS                                   
031599     PERFORM IMS-STATUSKONTROLL                                           
031600     .                                                                    
031601     EJECT                                                                
031602 IMS-GU-ARTC-01 SECTION.                                                  
031603                                                                          
031604     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031605          DELIMITED BY SIZE INTO SSA1                                     
031606     MOVE '  GE' TO GODK-STATUSKODER                                      
031607     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
031608     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031609     PERFORM IMS-STATUSKONTROLL                                           
031610     .                                                                    
031611     EJECT                                                                
031700 IMS-STATUSKONTROLL SECTION.                                              
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

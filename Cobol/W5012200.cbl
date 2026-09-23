001370 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5012200.                                                
001500 AUTHOR.         BO HAMMARIN.                                             
001600 DATE-WRITTEN.   97/03/14.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
001910*        FRÅGE-MPP SOM VISAR                                              
001920*        - EXISTERANDE PRISER FÖR UNDERLEVERANTÖRER TILL                  
001921*          ANGIVEN ARTIKEL                                                
001940*        - ENDAST ENGELSKT FORMAT                                         
002100*                                                                         
002200*        INMATNINGSREGLER                                                 
002201*        1 IDARTNR, OM IFYLLT                                             
002202*                   -FRÅN SKÄRM                                           
002203*                   OM EJ IFYLLT                                          
002204*                   -FRÅN USERDB                                          
002205*                                                                         
002210*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W5T122                                              
002600*        MID:         W5I12201                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W5O12201                                            
002901*                                                                         
002910*        ÄNDRAD FÖR ETRACKER NO 1567775, INSTALLERAD 2004-11-23           
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)  VALUE 'W5012200'.             
003700                                                                          
003900 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
004100 77  JA                          PIC X      VALUE 'J'.                    
004200 77  NEJ                         PIC X      VALUE 'N'.                    
004300                                                                          
004401*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004402 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004410 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X      VALUE 'J'.                    
005100     88  NYCKLAR-OK                         VALUE 'J'.                    
005200     88  NYCKLAR-FEL                        VALUE 'N'.                    
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)   VALUE SPACE.                  
005500     88  EGEN-MID                           VALUE '5122'.                 
005600     88  GODK-MID                           VALUE '5122'                  
005700                                                  '5119'.                 
006100     88  HELP-MID                           VALUE '0551'.                 
006101                                                                          
006110 77  WS-DAPRLIST                 PIC 9(8).                                
006112                                                                          
006120 01  VARIABLER.                                                           
006121     03  WS-IDLEVNR-HUV          PIC X(5).                                
006122     03  W-IDLEVNR               PIC X(5)    VALUE SPACE.                 
006123     03  W-DAPRLIST-9KOMPL       PIC 9(8)    VALUE ZERO.                  
006200     EJECT                                                                
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)   VALUE 'WMEDKONV'.             
006600     03  W005INIT                PIC X(8)   VALUE 'W005INIT'.             
006700     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
006800     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007601     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007610     03  INF-PF8                 PIC X(3)    VALUE '105'.                 
007710     03  INF-PRICE-MISSING       PIC X(3)    VALUE '301'.                 
007720     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
007800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007801     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008501     EJECT                                                                
008502*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008503* SPAR-AREA FÖR KOMMUNIKATION MED MSG-BASEN                               
008504 01  SPAR-AREA.                                                           
008505     03  SPAR-IDTRANS               PIC X(4)  VALUE '5122'.               
008506     03  SPAR-IDARTNR-ENTER         PIC S9(9) COMP-3 VALUE ZERO.          
008507     03  SPAR-IDARTNR-NEXT          PIC S9(9) COMP-3 VALUE ZERO.          
008508     03  SPAR-KDSEGKEY-ENTER        PIC X(1).                             
008509     03  SPAR-KDSEGKEY-NEXT         PIC X(1).                             
008511     03  SPAR-DAPRLIST-9KOMPL-ENTER PIC 9(8)  VALUE ZERO.                 
008512     03  SPAR-DAPRLIST-9KOMPL-NEXT  PIC 9(8)  VALUE ZERO.                 
008513     03  FILLER                     PIC X(2)  VALUE SPACE.                
008516     03  SPAR-IDLEVNR-ENTER         PIC X(5)  VALUE SPACE.                
008520     03  SPAR-IDLEVNR-NEXT          PIC X(5)  VALUE SPACE.                
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                   PIC X(16)   VALUE 'MID-AREA'.               
009000     SKIP3                                                                
009100*01  MID -COPY W5I12201                                                   
009200     EJECT                                                                
009300 01  FILLER                   PIC X(16)  VALUE 'MSG/MOD-AREA'.            
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W5O12201                                                 
009900     EJECT                                                                
010000 01  FILLER                    PIC X(16)   VALUE 'MFS-AREA'.              
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010300     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                    PIC X(16)   VALUE 'IMS-WS'.                
010800                                                                          
010900 01  NYCKLAR-TILL-DLI.                                                    
011001*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
011002     03  FILLER            PIC X(12) VALUE 'DLI-NYCKEL ='.                
011003     03  W-IDARTNR-MIN-X.                                                 
011004         05  W-IDARTNR-MIN         PIC S9(9)   COMP-3 VALUE ZERO.         
011005     03  W-KDSEGKEY-MIN-X.                                                
011006         05  W-KDSEGKEY-MIN        PIC X(1)           VALUE '1'.          
011013                                                                          
011014     03  W-IDARTNR-X.                                                     
011015         05  W-IDARTNR             PIC S9(9)   VALUE ZERO COMP-3.         
011016     03  W-KDSEGKEY-X.                                                    
011017         05  W-KDSEGKEY            PIC X(1)    VALUE '1'.                 
011018     03  W-WDK621KY-X.                                                    
011020         05  W-DAPRLIST-9K-21      PIC 9(8)    VALUE ZERO.                
011030         05  W-IDLEVNR-21          PIC X(5)    VALUE LOW-VALUE.           
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                 PIC XX.                                    
011400     88  SEGMENT-FINNS                     VALUE '  '.                    
011500     88  SEGMENT-FINNS-REDAN               VALUE 'II'.                    
011600     88  SEGMENT-SAKNAS                    VALUE 'GE'.                    
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
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
013002 01  DLI-IO-WLARTC01.                                                     
013003*    03  -COPY WDK601                                                     
013004     EJECT                                                                
013005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
013006 01  DLI-IO-WLARTC11.                                                     
013007*    03  -COPY WDK611                                                     
013008     EJECT                                                                
013009 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC21'.                    
013010 01  DLI-IO-WLARTC21.                                                     
013020*    03  -COPY WDK621                                                     
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009  -PRE MSG-                                               
013600*01  -COPY W0008  -PRE USEA-                                              
013700     05  FILLER                  PIC X.                                   
013801     EJECT                                                                
013802*01  -COPY W0008  -PRE ARTC-                                              
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ARTC-PCB.                     
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTC-PCB.                     
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
015200         PERFORM F-LAES-VISA-INFO                                         
015300       END-IF                                                             
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O12201 + 4                      
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
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I12201                 
016900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
017000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W5I12201                 
017300       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
017400       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
017800     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
017900     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
018000                                                                          
018100     MOVE LOW-VALUE       TO MSG-AREA                                     
018200     MOVE 'W5O122N1'      TO MFS-IDMOD                                    
018300     MOVE '5122'          TO MOD-IDTRANS                                  
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       MOVE MSGI-SPAR-AREA TO SPAR-AREA                                   
018800     ELSE                                                                 
018900       MOVE SPACE          TO MFS-KDTRTYP                                 
019000       MOVE '7'            TO MFS-IDPFK                                   
019100     END-IF                                                               
019200                                                                          
019300     MOVE 'GB'             TO MED-IDSKYLT                                 
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019800     MOVE ALL '+'                TO MSGI-WMSGINIT                         
019900     MOVE '001'                  TO MSGI-KDCALL                           
020000     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
020100     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
020200     MOVE '5122'                 TO MSGI-IDTRANS                          
020210                                                                          
020420     IF GODK-MID                                                          
020430       MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                            
020440     ELSE                                                                 
020450       MOVE MSGI-IDARTNR       TO MID-IDARTNR-IN                          
020460     END-IF                                                               
020470                                                                          
020510                                                                          
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020601                                                                          
020610     IF GODK-MID                                                          
020620       MOVE MSGI-SPAR-AREA       TO SPAR-AREA                             
020630     END-IF                                                               
020700                                                                          
020800     MOVE JA                     TO NYCKLAR-SW                            
021001                                                                          
021002*    -- KONTROLL AV IDARTNR                                               
021003     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
021004                                                                          
021005     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021006       MOVE '7'           TO MFS-IDPFK                                    
021007       MOVE SPACE         TO MFS-KDTRTYP                                  
021008     END-IF                                                               
021009     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
021010     IF MSGI-IDARTNR NUMERIC                                              
021011       MOVE MSGI-IDARTNR  TO W-IDARTNR                                    
021012     ELSE                                                                 
021013       MOVE NEJ           TO NYCKLAR-SW                                   
021020     END-IF                                                               
021101                                                                          
021102     IF NYCKLAR-OK                                                        
021103       MOVE MSGI-IDARTNR    TO MOD-IDARTNR-UT                             
021104     ELSE                                                                 
021105       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
021110     END-IF                                                               
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                 
021700*      PERFORM MFS-RENSA-FAELT-IN                                         
021800       PERFORM MFS-RENSA-FAELT-UT                                         
021900     END-IF                                                               
022000     .                                                                    
022101     EJECT                                                                
022102 C-FOERSTA-SIDA SECTION.                                                  
022103                                                                          
022104     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022105     CALL WMEDKONV USING MED-WMEDAREA                                     
022106     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
022107                                                                          
022108     PERFORM MFS-RENSA-FAELT-IN                                           
022109     .                                                                    
022110     EJECT                                                                
022111 D-NAESTA-SIDA SECTION.                                                   
022112                                                                          
022113     IF SPAR-IDTRANS = '5122'                                             
022114       MOVE SPAR-IDARTNR-NEXT         TO W-IDARTNR-MIN                    
022115                                         W-IDARTNR                        
022116       MOVE SPAR-KDSEGKEY-NEXT        TO W-KDSEGKEY-MIN                   
022117       MOVE SPAR-DAPRLIST-9KOMPL-NEXT TO W-DAPRLIST-9KOMPL                
022119       MOVE SPAR-IDLEVNR-NEXT         TO W-IDLEVNR                        
022120     ELSE                                                                 
022121       PERFORM MFS-RENSA-FAELT-IN                                         
022122     END-IF                                                               
022123     .                                                                    
022124     EJECT                                                                
022125 E-SAMMA-SIDA SECTION.                                                    
022126                                                                          
022127     IF SPAR-IDTRANS = '5122' OR '0551'                                   
022128       MOVE SPAR-IDARTNR-ENTER         TO W-IDARTNR-MIN                   
022129                                          W-IDARTNR                       
022130       MOVE SPAR-KDSEGKEY-ENTER        TO W-KDSEGKEY-MIN                  
022131       MOVE SPAR-DAPRLIST-9KOMPL-ENTER TO W-DAPRLIST-9KOMPL               
022133       MOVE SPAR-IDLEVNR-ENTER         TO W-IDLEVNR                       
022135       IF MID-IDARTNR-IN = ALL '+'                                        
022136         PERFORM MFS-RENSA-FAELT-IN                                       
022137       ELSE                                                               
022141         PERFORM EA-MID-INDATA-TILL-MOD                                   
022142       END-IF                                                             
022143     ELSE                                                                 
022144       PERFORM MFS-RENSA-FAELT-IN                                         
022145     END-IF                                                               
022146     .                                                                    
022147     EJECT                                                                
022148 EA-MID-INDATA-TILL-MOD SECTION.                                          
022149                                                                          
022150     IF MID-IDARTNR-IN NOT = ALL '+'                                      
022151        MOVE MID-IDARTNR-IN       TO MOD-IDARTNR-IN                       
022152     ELSE                                                                 
022153        MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-IN                       
022154     END-IF                                                               
022155     IF MID-IDARTNR-UT NOT = ALL '+'                                      
022156        MOVE MID-IDARTNR-UT       TO MOD-IDARTNR-UT                       
022157     ELSE                                                                 
022158        MOVE MFS-RENSA-FAELT      TO MOD-IDARTNR-UT                       
022159     END-IF                                                               
022160     .                                                                    
022200     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022600     PERFORM IMS-GU-ARTC-ART                                              
022700                                                                          
022800     IF SEGMENT-SAKNAS                                                    
022810       MOVE ERR-PART-MISSING TO MED-IDMFSFEL                              
023000       CALL WMEDKONV USING MED-WMEDAREA                                   
023100       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
023200       PERFORM MFS-RENSA-FAELT-UT                                         
023300     ELSE                                                                 
023310       MOVE W-KDSEGKEY-MIN        TO W-KDSEGKEY                           
023400       PERFORM IMS-GNP-ARTC-CLAG                                          
023401       IF SEGMENT-SAKNAS                                                  
023402         MOVE INF-PRICE-MISSING    TO MED-IDMFSINF                        
023403         CALL WMEDKONV USING MED-WMEDAREA                                 
023404         MOVE MED-MFSINF           TO MOD-TEMFSINF                        
023405         PERFORM MFS-RENSA-FAELT-UT                                       
023406                                                                          
023407       ELSE                                                               
023408         MOVE CLAG-KDSEGKEY         TO W-KDSEGKEY                         
023409         MOVE W-DAPRLIST-9KOMPL     TO W-DAPRLIST-9K-21                   
023412                                                                          
023413         MOVE +1 TO INDX                                                  
023414         PERFORM IMS-GNP-ARTC-PRL                                         
023415         IF SEGMENT-FINNS                                                 
023416           MOVE ART-IDARTNR         TO SPAR-IDARTNR-ENTER                 
023417           MOVE CLAG-KDSEGKEY       TO SPAR-KDSEGKEY-ENTER                
023418           MOVE PRL-DAPRLIST-9KOMPL TO SPAR-DAPRLIST-9KOMPL-ENTER         
023420           MOVE PRL-IDLEVNR         TO SPAR-IDLEVNR-ENTER                 
023421         ELSE                                                             
023422           MOVE INF-PRICE-MISSING   TO MED-IDMFSINF                       
023423           CALL WMEDKONV USING MED-WMEDAREA                               
023424           MOVE MED-MFSINF          TO MOD-TEMFSINF                       
023425           PERFORM MFS-RENSA-FAELT-UT                                     
023426           MOVE W-IDARTNR-MIN       TO SPAR-IDARTNR-ENTER                 
023427           MOVE W-KDSEGKEY-MIN      TO SPAR-KDSEGKEY-ENTER                
023429           MOVE W-DAPRLIST-9KOMPL   TO SPAR-DAPRLIST-9KOMPL-ENTER         
023431           MOVE W-IDLEVNR           TO SPAR-IDLEVNR-ENTER                 
023432         END-IF                                                           
023433                                                                          
023435         PERFORM UNTIL INDX > MAX-INDX                                    
023436           IF SEGMENT-FINNS                                               
023441               IF PRL-FLHUVLEV = 'J'                                      
023442                 MOVE 'YES'         TO MOD-FLHUVLEV(INDX)                 
023443               ELSE                                                       
023444                 MOVE 'NO'          TO MOD-FLHUVLEV(INDX)                 
023445               END-IF                                                     
023446               IF PRL-SUINLEV-PR > 0 AND PRL-KDSTATUS-PR = 1              
023447                 MOVE 'INLEV'       TO MOD-KDSTATUS(INDX)                 
023449               ELSE                                                       
023450                 IF PRL-KDSTATUS-PR = 1                                   
023451                   MOVE 'GODK'      TO MOD-KDSTATUS(INDX)                 
023453                 ELSE                                                     
023454                   MOVE 'PREL'      TO MOD-KDSTATUS(INDX)                 
023456                 END-IF                                                   
023457               END-IF                                                     
023458                                                                          
023459               MOVE PRL-KDPRURSP    TO MOD-KDPRURSP (INDX)                
023460               COMPUTE WS-DAPRLIST = 99999999 -                           
023461                                     PRL-DAPRLIST-9KOMPL                  
023462               MOVE WS-DAPRLIST     TO MOD-DAPRLIST (INDX)                
023463               MOVE PRL-IDLEVNR     TO MOD-IDLEVNR (INDX)                 
023464               MOVE PRL-PRARTBEL-PR TO MOD-PRARTBEL-PR (INDX)             
023465               MOVE PRL-KDVALISO    TO MOD-KDVALISO (INDX)                
023466               MOVE PRL-KDFPKPRI    TO MOD-KDFPKPRI (INDX)                
023467                                                                          
023468               ADD 1                TO INDX                               
023469             PERFORM IMS-GNP-ARTC-PRL                                     
023470           ELSE                                                           
023471             MOVE MFS-RENSA-FAELT TO MOD-KDPRURSP (INDX)                  
023472                                     MOD-DAPRLIST (INDX)                  
023473                                     MOD-IDLEVNR (INDX)                   
023474                                     MOD-PRARTBEL-PR (INDX)               
023475                                     MOD-KDVALISO (INDX)                  
023476                                     MOD-FLHUVLEV (INDX)                  
023477                                     MOD-KDSTATUS (INDX)                  
023478                                     MOD-KDFPKPRI (INDX)                  
023479             ADD 1                TO INDX                                 
023480           END-IF                                                         
023481         END-PERFORM                                                      
023482                                                                          
023483         IF SEGMENT-SAKNAS AND                                            
023484            MOD-DAPRLIST (1) NOT > ZERO                                   
023485           MOVE INF-PRICE-MISSING    TO MED-IDMFSINF                      
023486           CALL WMEDKONV USING MED-WMEDAREA                               
023487           MOVE MED-MFSINF           TO MOD-TEMFSINF                      
023488           PERFORM MFS-RENSA-FAELT-UT                                     
023489         ELSE                                                             
023490           IF SEGMENT-FINNS                                               
023491             MOVE ART-IDARTNR         TO SPAR-IDARTNR-NEXT                
023492             MOVE CLAG-KDSEGKEY       TO SPAR-KDSEGKEY-NEXT               
023493             MOVE PRL-DAPRLIST-9KOMPL TO SPAR-DAPRLIST-9KOMPL-NEXT        
023494             MOVE PRL-IDLEVNR         TO SPAR-IDLEVNR-NEXT                
023495             MOVE INF-PF8             TO MED-IDMFSINF                     
023496             CALL WMEDKONV USING MED-WMEDAREA                             
023497             MOVE MED-TEMFSINF        TO MOD-TEMFSINF                     
023502           ELSE                                                           
023503             MOVE ZERO                TO SPAR-IDARTNR-NEXT                
023504             MOVE '1'                 TO SPAR-KDSEGKEY-NEXT               
023505             MOVE ZERO                TO SPAR-DAPRLIST-9KOMPL-NEXT        
023506             MOVE SPACE               TO SPAR-IDLEVNR-NEXT                
023507           END-IF                                                         
023508         END-IF                                                           
023509                                                                          
023510         MOVE '002'      TO MSGI-KDCALL                                   
023511         MOVE '5122'     TO MSGI-IDTRANS                                  
023512                            SPAR-IDTRANS                                  
023513         MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                
023514         CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                       
023515       END-IF                                                             
023520     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
024800 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025000*    --- ALLA UTDATA-FÄLT                                                 
025200*    MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                               
025301                                                                          
025310     MOVE 1 TO INDX                                                       
025320     PERFORM UNTIL INDX > MAX-INDX                                        
025321       MOVE MFS-RENSA-FAELT TO MOD-KDPRURSP (INDX)                        
025322                               MOD-DAPRLIST (INDX)                        
025323                               MOD-IDLEVNR (INDX)                         
025324                               MOD-PRARTBEL-PR (INDX)                     
025325                               MOD-KDVALISO (INDX)                        
025326                               MOD-FLHUVLEV (INDX)                        
025327                               MOD-KDSTATUS (INDX)                        
025328                               MOD-KDFPKPRI (INDX)                        
025330       ADD 1 TO INDX                                                      
025340     END-PERFORM                                                          
025400     .                                                                    
025501     SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025900*    --- ALLA INDATA-FÄLT                                                 
026000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
026200     .                                                                    
026300     EJECT                                                                
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
030600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
030700       MOVE '0' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GU-ARTC-ART SECTION.                                                 
031503                                                                          
031504     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
031505          DELIMITED BY SIZE INTO SSA1                                     
031506     MOVE '  GE' TO GODK-STATUSKODER                                      
031507     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
031508     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031509     PERFORM IMS-STATUSKONTROLL                                           
031510     .                                                                    
031511     EJECT                                                                
031512 IMS-GNP-ARTC-CLAG SECTION.                                               
031513                                                                          
031517     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
031518          DELIMITED BY SIZE INTO SSA1                                     
031519*    MOVE 'WLARTC11  '  TO SSA1                                           
031520     MOVE '  GE'   TO GODK-STATUSKODER                                    
031521     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-WLARTC11 SSA1                 
031522     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031523     PERFORM IMS-STATUSKONTROLL                                           
031524     .                                                                    
031525     EJECT                                                                
031526 IMS-GNP-ARTC-PRL SECTION.                                                
031527                                                                          
031531     STRING 'WLARTC21(WDK621KY=>' W-WDK621KY-X ')'                        
031532          DELIMITED BY SIZE INTO SSA1                                     
031534     MOVE '  GE' TO GODK-STATUSKODER                                      
031535     CALL CBLTDLI USING                                                   
031536                  GNP ARTC-PCB DLI-IO-WLARTC21 SSA1                       
031537     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
031538     PERFORM IMS-STATUSKONTROLL                                           
031540     .                                                                    
031600     EJECT                                                                
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

001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W3012700.                                                
001500 AUTHOR.         GAVIN SMITH.                                             
001600 DATE-WRITTEN.   99/12/09.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        TITTA PÅ 'PENDING' INVOICES PER DISTRICT OCH ARTNR.              
002100*                                                                         
002201*        PROGRAMMET LÄSER      WDA7                                       
002210*        PROGRAMMET LÄSER      WDD3                                       
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W3T127                                              
002600*        MID:         W3I12701                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W3O12701                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W3012700'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004401*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004402 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004410 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700                                                                          
004801 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004802     88  INDATA-OK                           VALUE 'J'.                   
004810     88  INDATA-FEL                          VALUE 'N'.                   
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '3127'.                
005600     88  GODK-MID                            VALUE '3127'.                
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
007501     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007502     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007503     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007510     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007601     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007610     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
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
008505     03  SPAR-IDTRANS           PIC  X(4)    VALUE '3127'.                
008506     03  SPAR-IDPTYP-ENTER      PIC  X(3)    VALUE 'FAK'.                 
008507     03  SPAR-IDPTYP-NEXT       PIC  X(3) .                               
008508     03  SPAR-IDPTYP-FIRST      PIC  X(3) .                               
008509     03  SPAR-IDDISTR-ENTER     PIC S9(5) VALUE ZERO  COMP-3.             
008510     03  SPAR-IDDISTR-NEXT      PIC S9(5)        COMP-3.                  
008511     03  SPAR-IDDISTR-FIRST     PIC S9(5)        COMP-3.                  
008512     03  SPAR-IDARTNR-ENTER     PIC S9(9) VALUE ZERO  COMP-3.             
008513     03  SPAR-IDARTNR-NEXT      PIC S9(9)        COMP-3.                  
008514     03  SPAR-IDARTNR-FIRST     PIC S9(9) VALUE ZERO  COMP-3.             
008515     03  SPAR-IDKUNDNR-ENTER    PIC S9(7) VALUE ZERO  COMP-3.             
008516     03  SPAR-IDKUNDNR-NEXT     PIC S9(7)        COMP-3.                  
008517     03  SPAR-IDKUNDNR-FIRST    PIC S9(7)        COMP-3.                  
008518     03  SPAR-IDBYTRAD-ENTER    PIC S9(5) VALUE ZERO  COMP-3.             
008520     03  SPAR-IDBYTRAD-NEXT     PIC S9(5)        COMP-3.                  
008530     03  SPAR-IDBYTRAD-FIRST    PIC S9(5)        COMP-3.                  
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W3I12701 -PRE MID-                                         
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W3O12701  -PRE MOD-                                      
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
011002     03  W-IDPTYP-MIN-X.                                                  
011003         05  W-IDPTYP-MIN       PIC  X(3)  .                              
011004                                                                          
011005     03  W-IDDISTR-MIN-X.                                                 
011006         05  W-IDDISTR-MIN      PIC S9(5)        COMP-3.                  
011007                                                                          
011011     03  W-IDARTNR-MIN-X.                                                 
011012         05  W-IDARTNR-MIN      PIC S9(9)        COMP-3.                  
011013                                                                          
011014     03  W-IDKUNDNR-MIN-X.                                                
011015         05  W-IDKUNDNR-MIN     PIC S9(7)        COMP-3.                  
011016                                                                          
011017     03  W-IDBYTRAD-MIN-X.                                                
011018         05  W-IDBYTRAD-MIN     PIC S9(5)        COMP-3.                  
011019                                                                          
011020     03  W-WDA7-X.                                                        
011021         05  W-IDPTYP            PIC X(3)    VALUE 'FAK'        .         
011022         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
011024         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011025         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
011026         05  W-IDBYTRAD          PIC S9(5)   VALUE ZERO COMP-3.           
011027     03  W-WDA7-M.                                                        
011028         05  W-IDPTYP-M          PIC X(3)    VALUE 'FAK'        .         
011029         05  W-IDDISTR-M         PIC S9(5)   VALUE ZERO COMP-3.           
011030         05  FILLER              PIC S9(9) VALUE 999999999 COMP-3.        
011031         05  FILLER              PIC S9(7) VALUE 9999999 COMP-3.          
011032         05  FILLER              PIC S9(5) VALUE 99999 COMP-3.            
011033     03  W-WDD301-X.                                                      
011034         05  W-IDARTNO           PIC S9(9)   VALUE ZERO COMP-3.           
011040     03  W-IDSKYLT-X.                                                     
011050         05  W-IDSKYLT           PIC  X(3)   VALUE 'GB ' .                
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(128).                              
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA7A1'.                      
013004 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA701'.                      
013005 01  DLI-IO-WDA701.                                                       
013006*    03  -COPY WDA701 -PRE WDA7-                                          
013010     EJECT                                                                
013011 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
013012 01  DLI-IO-WDD311.                                                       
013013*    03  -COPY WDD311   -PRE WDD3-                                        
013014     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801                                                                          
013802*01  -COPY W0008  -PRE WDA7-                                              
013803     05  FILLER                  PIC X.                                   
013804                                                                          
013805*01  -COPY W0008  -PRE WDD3-                                              
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDA7-PCB WDD3-PCB.            
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDA7-PCB WDD3-PCB.            
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014901         IF MFS-FIRST                                                     
014902             PERFORM C-FOERSTA-SIDA                                       
014903         ELSE                                                             
014904             IF MFS-NEXT                                                  
014905               PERFORM D-NAESTA-SIDA                                      
014906             ELSE                                                         
014907               PERFORM E-SAMMA-SIDA                                       
014908             END-IF                                                       
015110         END-IF                                                           
015200         PERFORM F-LAES-VISA-INFO                                         
015300       END-IF                                                             
015400*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
015500*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O12701 + 4                      
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
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I12701                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I12701                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W3O127N1' TO MFS-IDMOD                                         
018300     MOVE '3127' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '3127'            TO MSGI-IDTRANS                               
020300     IF GODK-MID                                                          
020410         MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                          
020420         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
020430         MOVE MID-IDPTYP-IN      TO MSGI-IDPTYP                           
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020700                                                                          
020710     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
020800     MOVE JA TO NYCKLAR-SW                                                
020900                                                                          
021001                                                                          
021002*    -- KONTROLL AV IDDISTR                                               
021004                                                                          
021005     IF MID-IDDISTR-IN NOT = ALL '+'                                      
021006       MOVE '7'         TO MFS-IDPFK                                      
021007       MOVE SPACE       TO MFS-KDTRTYP                                    
021008     END-IF                                                               
021009     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
021015     IF MSGI-IDDISTR NUMERIC                                              
021016       MOVE MSGI-IDDISTR TO W-IDDISTR  W-IDDISTR-M                        
021017                            SPAR-IDDISTR-FIRST MOD-IDDISTR-IN             
021018     ELSE                                                                 
021019       MOVE NEJ TO NYCKLAR-SW                                             
021020       MOVE MSGI-IDDISTR TO MOD-IDDISTR-IN                                
021021       MOVE MFS-NUM-FAELT-FEL TO MOD-IDDISTR-IN-ATTR                      
021030     END-IF                                                               
021101                                                                          
021102     IF GODK-MID OR NYCKLAR-OK                                            
021103*      MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                             
021104       MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                         
021105       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
021106     ELSE                                                                 
021107       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                             
021110     END-IF                                                               
021120*    -- KONTROLL AV IDARTNR                                               
021140                                                                          
021150     IF MID-IDARTNR-IN NOT = ALL '+'                                      
021160       MOVE '7'         TO MFS-IDPFK                                      
021170       MOVE SPACE       TO MFS-KDTRTYP                                    
021171     END-IF                                                               
021172     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
021173       IF MSGI-IDARTNR NUMERIC                                            
021174         MOVE MSGI-IDARTNR TO SPAR-IDARTNR-FIRST  MOD-IDARTNR-IN          
021180       END-IF                                                             
021191     IF MSGI-IDARTNR NUMERIC                                              
021192       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
021193*      MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                             
021194     ELSE                                                                 
021195       MOVE MSGI-IDARTNR TO MOD-IDARTNR-IN                                
021196       MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-IN-ATTR                      
021197       MOVE NEJ TO NYCKLAR-SW                                             
021198     END-IF                                                               
021199*************************************'                                    
021200*    -- KONTROLL AV IDPTYP                                                
021201                                                                          
021202     IF MID-IDPTYP-IN NOT = ALL '+'                                       
021203       MOVE '7'         TO MFS-IDPFK                                      
021204       MOVE SPACE       TO MFS-KDTRTYP                                    
021205       IF MSGI-IDPTYP  = 'FAK' OR 'fak' OR 'KRE' OR 'kre'                 
021206         MOVE MSGI-IDPTYP TO SPAR-IDPTYP-FIRST                            
021207       END-IF                                                             
021208     END-IF                                                               
021209     IF MSGI-IDPTYP  = 'FAK' OR 'fak' OR 'KRE' OR 'kre'                   
021210*      MOVE MFS-RENSA-FAELT TO MOD-IDPTYP-IN                              
021211       MOVE MSGI-IDPTYP TO W-IDPTYP  W-IDPTYP-M  MOD-IDPTYP-IN            
021212     ELSE                                                                 
021213       MOVE MSGI-IDPTYP TO MOD-IDPTYP-IN                                  
021214       MOVE MFS-ALFA-FAELT-FEL TO MOD-IDPTYP-IN-ATTR                      
021215       MOVE NEJ TO NYCKLAR-SW                                             
021216     END-IF                                                               
021217*************************************'                                    
021218     IF GODK-MID AND NYCKLAR-OK                                           
021219       MOVE MSGI-IDDISTR        TO MOD-IDDISTR-UT                         
021220       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
021221       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
021222       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
021223       MOVE MSGI-IDPTYP         TO MOD-IDPTYP-UT                          
021224       MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                             
021225       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                             
021226       MOVE MFS-RENSA-FAELT TO MOD-IDPTYP-IN                              
021227*    ELSE                                                                 
021228*      PERFORM MFS-ROER-EJ-FAELT-IN                                       
021229     END-IF                                                               
021230*************************************                                     
021300     IF NYCKLAR-FEL                                                       
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021700*      PERFORM MFS-RENSA-FAELT-IN                                         
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
022108     IF SPAR-IDTRANS = '3127' OR '0551'                                   
022109     IF SPAR-IDPTYP-FIRST = 'FAK' OR 'KRE'                                
022110       MOVE SPAR-IDPTYP-FIRST  TO W-IDPTYP W-IDPTYP-M                     
022111     ELSE                                                                 
022112       MOVE 'FAK' TO W-IDPTYP  W-IDPTYP-M                                 
022113     END-IF                                                               
022114     IF  SPAR-IDDISTR-FIRST NUMERIC                                       
022115       MOVE SPAR-IDDISTR-FIRST TO W-IDDISTR W-IDDISTR-M                   
022116     ELSE                                                                 
022117       MOVE ZERO TO W-IDDISTR W-IDDISTR-M                                 
022118     END-IF                                                               
022119     IF SPAR-IDARTNR-FIRST NUMERIC                                        
022120       MOVE SPAR-IDARTNR-FIRST TO W-IDARTNR                               
022121     ELSE                                                                 
022122       MOVE ZERO TO W-IDARTNR                                             
022123     END-IF                                                               
022124       MOVE ZERO                TO W-IDKUNDNR                             
022125       MOVE ZERO                TO W-IDBYTRAD                             
022126     END-IF                                                               
022127     PERFORM MFS-RENSA-FAELT-IN                                           
022128     .                                                                    
022129     EJECT                                                                
022130 D-NAESTA-SIDA SECTION.                                                   
022131                                                                          
022132     IF SPAR-IDTRANS = '3127'                                             
022138     IF SPAR-IDPTYP-NEXT = 'FAK' OR 'KRE'                                 
022139       MOVE SPAR-IDPTYP-NEXT  TO W-IDPTYP  W-IDPTYP-M                     
022140     ELSE                                                                 
022141       MOVE 'FAK' TO W-IDPTYP W-IDPTYP-M                                  
022142     END-IF                                                               
022143     IF  SPAR-IDDISTR-NEXT NUMERIC                                        
022144       MOVE SPAR-IDDISTR-NEXT TO W-IDDISTR W-IDDISTR-M                    
022145     ELSE                                                                 
022146       MOVE ZERO TO W-IDDISTR W-IDDISTR-M                                 
022147     END-IF                                                               
022148     IF SPAR-IDARTNR-NEXT NUMERIC                                         
022149       MOVE SPAR-IDARTNR-NEXT TO W-IDARTNR                                
022150     ELSE                                                                 
022151       MOVE ZERO TO W-IDARTNR                                             
022152     END-IF                                                               
022153     IF SPAR-IDKUNDNR-NEXT NUMERIC                                        
022154       MOVE SPAR-IDKUNDNR-NEXT  TO W-IDKUNDNR                             
022155     ELSE                                                                 
022156       MOVE ZERO                TO W-IDKUNDNR                             
022157     END-IF                                                               
022158     IF SPAR-IDBYTRAD-NEXT NUMERIC                                        
022159       MOVE SPAR-IDBYTRAD-NEXT TO W-IDBYTRAD                              
022160     ELSE                                                                 
022161       MOVE ZERO                TO W-IDBYTRAD                             
022162     END-IF                                                               
022163     PERFORM MFS-RENSA-FAELT-IN                                           
022164     END-IF                                                               
022165     .                                                                    
022166     EJECT                                                                
022167 E-SAMMA-SIDA SECTION.                                                    
022168                                                                          
022169*    IF SPAR-IDTRANS = '3127' OR '0551'                                   
022170*      MOVE SPAR-IDPTYP-ENTER  TO W-IDPTYP  W-IDPTYP-M                    
022171*      MOVE SPAR-IDDISTR-ENTER TO W-IDDISTR  W-IDDISTR-M                  
022172*      MOVE SPAR-IDARTNR-ENTER TO W-IDARTNR                               
022173*      MOVE SPAR-IDKUNDNR-ENTER TO W-IDKUNDNR                             
022174*      MOVE SPAR-IDBYTRAD-ENTER TO W-IDBYTRAD                             
022175*    ELSE                                                                 
022176       PERFORM MFS-RENSA-FAELT-IN                                         
022177       PERFORM MFS-ROER-EJ-FAELT-UT                                       
022178*    END-IF                                                               
022179     .                                                                    
022180     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500     IF MFS-IDPFK NOT = ' '                                               
022600     PERFORM IMS-GN-WDA701                                                
023415     IF SEGMENT-FINNS                                                     
023416         MOVE WDA7-ROT-IDPTYP   TO SPAR-IDPTYP-ENTER                      
023417         MOVE WDA7-ROT-IDDISTR  TO SPAR-IDDISTR-ENTER                     
023418         MOVE WDA7-ROT-IDARTNR-BYT  TO SPAR-IDARTNR-ENTER                 
023419         MOVE WDA7-ROT-IDKUNDNR TO SPAR-IDKUNDNR-ENTER                    
023421         MOVE WDA7-ROT-IDBYTRAD TO SPAR-IDBYTRAD-ENTER                    
023422*        MOVE WDA7-ROT-IDARTNR-BYT TO MOD-IDARTNR-UT                      
023423*        MOVE WDA7-ROT-IDDISTR TO MOD-IDDISTR-UT                          
023424*        MOVE WDA7-ROT-IDPTYP   TO MOD-IDPTYP-UT                          
023425*    ELSE                                                                 
023426     END-IF                                                               
023427                                                                          
023428     MOVE +1 TO INDX                                                      
023429     PERFORM UNTIL INDX > MAX-INDX                                        
023430       IF SEGMENT-FINNS                                                   
023431         MOVE WDA7-ROT-IDKUNDNR TO MOD-IDKUNDNR (INDX)                    
023432         MOVE WDA7-ROT-IDARTNR-BYT TO MOD-IDARTNR (INDX)                  
023433         MOVE WDA7-ROT-IDPTYP   TO MOD-IDPTYP   (INDX)                    
023434         MOVE WDA7-ROT-KVANTAL  TO MOD-KVANTAL  (INDX)                    
023435         MOVE WDA7-ROT-IDDC     TO MOD-IDDC     (INDX)                    
023436         MOVE WDA7-ROT-DAFAKT   TO MOD-DAREGDAT (INDX)                    
023437         MOVE WDA7-ROT-IDARTNR-BYT TO W-IDARTNO                           
023438**********************                                                    
023439         PERFORM IMS-GU-WDD311-BSEQ                                       
023440         IF SEGMENT-FINNS                                                 
023441           MOVE WDD3-TEXT-BEART TO MOD-BEART-ENG  (INDX)                  
023442         ELSE                                                             
023443           MOVE SPACE          TO MOD-BEART-ENG  (INDX)                   
023444         END-IF                                                           
023445*******************************                                           
023446         PERFORM IMS-GN-WDA701                                            
023447       ELSE                                                               
023448         MOVE MFS-RENSA-FAELT TO MOD-IDPTYP    (INDX)                     
023449                                 MOD-IDKUNDNR  (INDX)                     
023450                                 MOD-BEART-ENG (INDX)                     
023451                                 MOD-KVANTAL   (INDX)                     
023452                                 MOD-IDDC      (INDX)                     
023453                                 MOD-DAREGDAT  (INDX)                     
023454       END-IF                                                             
023455         ADD 1 TO INDX                                                    
023456       END-PERFORM                                                        
023457                                                                          
023458       IF SEGMENT-FINNS                                                   
023459         MOVE WDA7-ROT-IDPTYP   TO SPAR-IDPTYP-NEXT                       
023460         MOVE WDA7-ROT-IDDISTR  TO SPAR-IDDISTR-NEXT                      
023461         MOVE WDA7-ROT-IDARTNR-BYT TO SPAR-IDARTNR-NEXT                   
023462         MOVE WDA7-ROT-IDKUNDNR TO SPAR-IDKUNDNR-NEXT                     
023463         MOVE WDA7-ROT-IDBYTRAD TO SPAR-IDBYTRAD-NEXT                     
023464         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
023465         CALL WMEDKONV USING MED-WMEDAREA                                 
023466         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
023467       END-IF                                                             
023468*FIX                                                                      
023469       END-IF                                                             
023470       MOVE '002'      TO MSGI-KDCALL                                     
023471       MOVE '3127'   TO SPAR-IDTRANS                                      
023472       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
023480       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023600     .                                                                    
023700     EJECT                                                                
024800 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025000*    --- ALLA UTDATA-FÄLT                                                 
025110*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025200     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                               
025300                             MOD-IDARTNR-UT                               
025301                             MOD-IDPTYP-UT                                
025302     MOVE 1 TO INDX                                                       
025303      PERFORM UNTIL INDX > 14                                             
025310         MOVE MFS-RENSA-FAELT TO MOD-IDPTYP    (INDX)                     
025320                                 MOD-IDKUNDNR  (INDX)                     
025330                                 MOD-BEART-ENG (INDX)                     
025340                                 MOD-KVANTAL   (INDX)                     
025350                                 MOD-IDDC      (INDX)                     
025360                                 MOD-DAREGDAT  (INDX)                     
025361         ADD 1 TO INDX                                                    
025370      END-PERFORM                                                         
025400     .                                                                    
025501     SKIP3                                                                
025502 MFS-ROER-EJ-FAELT-UT SECTION.                                            
025503                                                                          
025504*    --- ALLA UTDATA-FÄLT                                                 
025505*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025506     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-UT                             
025507                             MOD-IDARTNR-UT                               
025508                             MOD-IDPTYP-UT                                
025509                             MOD-TEMFSFEL                                 
025510     MOVE 1 TO INDX                                                       
025511      PERFORM UNTIL INDX > 14                                             
025520         MOVE MFS-ROER-EJ-FAELT TO MOD-IDPTYP    (INDX)                   
025530                                 MOD-IDKUNDNR  (INDX)                     
025540                                 MOD-BEART-ENG (INDX)                     
025550                                 MOD-KVANTAL   (INDX)                     
025560                                 MOD-IDDC      (INDX)                     
025570                                 MOD-DAREGDAT  (INDX)                     
025571                                 MOD-IDARTNR   (INDX)                     
025580         ADD 1 TO INDX                                                    
025590      END-PERFORM                                                         
025600     .                                                                    
025610     SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025900*    --- ALLA INDATA-FÄLT                                                 
026000     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
026100                             MOD-IDARTNR-IN                               
026110                             MOD-IDPTYP-IN                                
026200     .                                                                    
026300     EJECT                                                                
026400*MFS-ROER-EJ-FAELT-IN SECTION.                                            
026500*                                                                         
026600*    --- ALLA INDATA-FÄLT                                                 
026700*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-IN                             
026800*                              MOD-IDARTNR-IN                             
026900*                              MOD-IDPTYP-IN                              
027000*    .                                                                    
027100*    EJECT                                                                
027900     EJECT                                                                
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
030600     IF MSGI-IDLAND-SPR = 'GB'                                            
030700       MOVE 'N' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GN-WDA701 SECTION.                                                   
031503                                                                          
031504     STRING 'WDA701  (WDA701KY>=' W-WDA7-X                                
031506                    '&WDA701KY<=' W-WDA7-M ')'                            
031510          DELIMITED BY SIZE INTO SSA1                                     
031511     MOVE 'GBGE' TO GODK-STATUSKODER                                      
031512     CALL CBLTDLI USING GN WDA7-PCB DLI-IO-WDA701 SSA1                    
031513     MOVE WDA7-STATUS-CODE TO STATUS-WS                                   
031514     PERFORM IMS-STATUSKONTROLL                                           
031515     .                                                                    
031516     SKIP3                                                                
031549 IMS-GU-WDD311-BSEQ SECTION.                                              
031550                                                                          
031551     STRING 'WDD301  (WDD3BSEQ =' W-WDD301-X ')'                          
031552          DELIMITED BY SIZE INTO SSA1                                     
031553     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
031554          DELIMITED BY SIZE INTO SSA2                                     
031555     MOVE '  GE' TO GODK-STATUSKODER                                      
031556     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
031557     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
031558     PERFORM IMS-STATUSKONTROLL                                           
031559     .                                                                    
031560     EJECT                                                                
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

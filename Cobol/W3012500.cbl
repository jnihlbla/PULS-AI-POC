001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W3012500.                                                
001500 AUTHOR.         MARKUS ASPFJÄLL.                                         
001600 DATE-WRITTEN.   99/11/30.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        LÄGGER UPP GOODS RECEIVING CODES PÅ WDR1-WDGX3160                
002100*        UPPDATERA REDAN BEFINTLIGA POSTER                                
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W3T125U                                             
002600*        MID:         W3I12501                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W3O12501                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W3012500'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004210 01  WS-DAGENS-DATUM             PIC 9(8).                                
004220 01  TRANS-TID                   PIC 9(9)  COMP-3.                        
004221 01  WS-KDBYTREF-NUM             PIC 9(3).                                
004230                                                                          
004300                                                                          
004401*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004402 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004410 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700                                                                          
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005310 77  INDATA-SW                  PIC X       VALUE 'J'.                    
005320     88  INDATA-OK                          VALUE 'J'.                    
005330     88  INDATA-FEL                         VALUE 'N'.                    
005340                                                                          
005350 77  UPPDAT-SW                  PIC X       VALUE ' '.                    
005360     88  REPLACE-OK                         VALUE 'R'.                    
005370     88  INSERT-OK                          VALUE 'I'.                    
005371     88  DELETE-OK                          VALUE 'D'.                    
005380                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '3125'.                
005600     88  GODK-MID                            VALUE '3121' '3122'          
005700                                                   '3123' '3124'          
005800                                                   '3125' '3126'          
005900                                                   '3127' '3128'          
006000                                                   '3129'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200     EJECT                                                                
006300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006400 01  GENERELLA-SUBPROGRAM.                                                
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007200*01 -COPY WMEDAREA                                                        
007300     SKIP3                                                                
007400 01  MESSAGE-CODES.                                                       
007601     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007610     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  ITEM-MISSING            PIC X(3)    VALUE '029'.                 
007801     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007802     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007803     03  LAST-PAGE               PIC X(3)    VALUE '106'.                 
007804     03  UPDATING-OK             PIC X(3)    VALUE '101'.                 
007805     EJECT                                                                
007810*01  -COPY WDATAREA                                                       
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
008505     03  SPAR-IDTRANS           PIC X(4)    VALUE '3125'.                 
008506     03  SPAR-KDBYTREF-ENTER       PIC X(3).                              
008510     03  SPAR-KDBYTREF-NEXT        PIC X(3).                              
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W3I12501                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W3O12501                                                 
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
011002*                                                                         
011003     03  W-3160-IDHTYP-X.                                                 
011004         05  W-3160-IDHTYP       PIC  X(4)  VALUE '3159'.                 
011005         05  W-3160-LOW-VALUE    PIC  X(26) VALUE LOW-VALUE.              
011006                                                                          
011007     03  W-KDBYTREF-MIN-X.                                                
011008         05  W-KDBYTREF-MIN     PIC X(3).                                 
011010                                                                          
011020     03  W-KDBYTREF-X.                                                    
011030         05  W-KDBYTREF         PIC X(3).                                 
011040                                                                          
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
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012810 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3160'.                    
012820 01  DLI-IO-WDGX3160.                                                     
012830*    03  -COPY WDGX3160                                                   
012840     EJECT                                                                
012900                                                                          
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013601     05  FILLER                  PIC X.                                   
013610*01  -COPY W0008   -PRE 3160-                                             
013700     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 3160-PCB.                     
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 3160-PCB.                     
014100                                                                          
014201*---- FLYTTA SUBPROGRAM CALL TILL RÄTT STÄLLE --                          
014202*                                                                         
014203*    MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
014204*    MOVE AAMMDD TO DAT-I-TIDATUM                                         
014205*                                                                         
014206*    CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
014207*                    DAT-O-TIDATUM DAT-KDSVAR                             
014208*                                                                         
014209*    IF DAT-KDSVAR-OK                                                     
014210*      ......                                                             
014211*    ELSE                                                                 
014212*      .........                                                          
014213*    END-IF                                                               
014220*------------------------                                                 
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
014907               IF MFS-UPDATE                                              
014908                 PERFORM G-KOLLA-INDATA                                   
014909                 IF INDATA-OK                                             
014910                   PERFORM H-UPPDATERA                                    
014911                 END-IF                                                   
014912               ELSE                                                       
014913                 PERFORM E-SAMMA-SIDA                                     
014914               END-IF                                                     
014915             END-IF                                                       
014920           END-IF                                                         
014930         IF INDATA-OK                                                     
015200           PERFORM F-LAES-VISA-INFO                                       
015300         END-IF                                                           
015400       END-IF                                                             
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O12501 + 4                      
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
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I12501                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I12501                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W3O125N1' TO MFS-IDMOD                                         
018300     MOVE '3125' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018500                                                                          
018600     IF EGEN-MID OR HELP-MID                                              
018700       CONTINUE                                                           
018800     ELSE                                                                 
018900       MOVE SPACE TO MFS-KDTRTYP                                          
019000       MOVE '7' TO MFS-IDPFK                                              
019100     END-IF                                                               
019200     MOVE 'GB'             TO MED-IDSKYLT                                 
019400     .                                                                    
019500     EJECT                                                                
019600 B-KOLLA-NYCKLAR SECTION.                                                 
019700                                                                          
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '3125'            TO MSGI-IDTRANS                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020601     MOVE MSGI-SPAR-AREA    TO SPAR-AREA                                  
020800     MOVE JA TO NYCKLAR-SW                                                
020900                                                                          
021200                                                                          
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
022113     IF SPAR-IDTRANS = '3125'                                             
022114       MOVE SPAR-KDBYTREF-NEXT TO W-KDBYTREF                              
022115                                  SPAR-KDBYTREF-ENTER                     
022116       PERFORM MFS-RENSA-FAELT-IN                                         
022117     ELSE                                                                 
022118       PERFORM MFS-RENSA-FAELT-IN                                         
022119     END-IF                                                               
022120     .                                                                    
022121     EJECT                                                                
022122 E-SAMMA-SIDA SECTION.                                                    
022123                                                                          
022124     IF SPAR-IDTRANS = '3125' OR '0551'                                   
022125       MOVE SPAR-KDBYTREF-ENTER TO W-KDBYTREF                             
022126                                                                          
022127       IF MID-FLUPDAT = ALL '+' AND                                       
022128          MID-KDBYTREF = ALL '+' AND                                      
022129          MID-TENOTE   = ALL '+' AND                                      
022130          MID-KVPOINT  = ALL '+'                                          
022131         PERFORM MFS-RENSA-FAELT-IN                                       
022132       ELSE                                                               
022133         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
022134         CALL WMEDKONV USING MED-WMEDAREA                                 
022135         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
022136         PERFORM EA-MID-INDATA-TILL-MOD                                   
022137       END-IF                                                             
022138     ELSE                                                                 
022139       PERFORM MFS-RENSA-FAELT-IN                                         
022140     END-IF                                                               
022141     .                                                                    
022142     EJECT                                                                
022143 EA-MID-INDATA-TILL-MOD SECTION.                                          
022144                                                                          
022145* * * * * FÖR VARJE MID-FÄLT                                              
022146* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
022147* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
022148* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
022149     IF MID-FLUPDAT NOT = ALL '+'                                         
022150       MOVE MID-FLUPDAT           TO MOD-FLUPDAT                          
022151       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLUPDAT-ATTR                     
022152     ELSE                                                                 
022153       MOVE MFS-RENSA-FAELT       TO MOD-FLUPDAT                          
022154     END-IF                                                               
022155     IF MID-KDBYTREF NOT = ALL '+'                                        
022156       MOVE MID-KDBYTREF          TO MOD-KDBYTREF                         
022157       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBYTREF-ATTR                    
022158     ELSE                                                                 
022159       MOVE MFS-RENSA-FAELT       TO MOD-KDBYTREF                         
022160     END-IF                                                               
022161     IF MID-TENOTE   NOT = ALL '+'                                        
022162       MOVE MID-TENOTE            TO MOD-TENOTE                           
022163       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TENOTE-ATTR                      
022164     ELSE                                                                 
022165       MOVE MFS-RENSA-FAELT       TO MOD-TENOTE                           
022166     END-IF                                                               
022167     IF MID-KVPOINT  NOT = ALL '+'                                        
022168       MOVE MID-KVPOINT           TO MOD-KVPOINT                          
022169       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVPOINT-ATTR                     
022170     ELSE                                                                 
022171       MOVE MFS-RENSA-FAELT       TO MOD-KVPOINT                          
022172     END-IF                                                               
022180     .                                                                    
022200     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022600     PERFORM FA-LAES-GRUNDDATA                                            
022700                                                                          
022800     IF SEGMENT-SAKNAS                                                    
022900        MOVE ITEM-MISSING TO MED-IDMFSFEL                                 
023000        CALL WMEDKONV USING MED-WMEDAREA                                  
023100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023110        MOVE W-KDBYTREF TO MOD-TEMFSINF                                   
023200        PERFORM MFS-RENSA-FAELT-UT                                        
023300     ELSE                                                                 
023401*      -- POSITIONERA FÖR LÄSNING AV DATA TILL ÖVERSTA RADEN              
023402*      -- (EJ NÖDVÄNDIGT OM -MIN NYCKLAR ANVÄNDS DIREKT I SSA)            
023403       MOVE 3160-KDBYTREF       TO W-KDBYTREF                             
023405                                   SPAR-KDBYTREF-ENTER                    
023406       MOVE +1 TO INDX                                                    
023414                                                                          
023415       PERFORM UNTIL INDX > MAX-INDX                                      
023416         IF SEGMENT-FINNS                                                 
023417           MOVE 3160-KDBYTREF TO MOD-TAB-KDBYTREF (INDX)                  
023419           MOVE 3160-TENOTE   TO MOD-TAB-TENOTE   (INDX)                  
023420           MOVE 3160-KVPOINT  TO MOD-TAB-KVPOINT  (INDX)                  
023421           PERFORM IMS-GN-WDGX3160                                        
023422         ELSE                                                             
023423           MOVE MFS-RENSA-FAELT TO MOD-TAB-KDBYTREF (INDX)                
023424                                   MOD-TAB-TENOTE   (INDX)                
023425                                   MOD-TAB-KVPOINT  (INDX)                
023426         END-IF                                                           
023427         ADD 1 TO INDX                                                    
023428                                                                          
023429       END-PERFORM                                                        
023430                                                                          
023431       IF SEGMENT-FINNS                                                   
023432         MOVE 3160-KDBYTREF TO SPAR-KDBYTREF-NEXT                         
023433         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
023434         CALL WMEDKONV USING MED-WMEDAREA                                 
023435         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
023436       ELSE                                                               
023437         IF MFS-UPDATE                                                    
023438           CONTINUE                                                       
023439         ELSE                                                             
023440           MOVE LAST-PAGE            TO MED-IDMFSINF                      
023441           CALL WMEDKONV USING MED-WMEDAREA                               
023442           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
023443           MOVE SPAR-KDBYTREF-ENTER TO SPAR-KDBYTREF-NEXT                 
023444         END-IF                                                           
023445       END-IF                                                             
023446       PERFORM MFS-RENSA-FAELT-IN                                         
023447                                                                          
023448       MOVE '002'      TO MSGI-KDCALL                                     
023449       MOVE '3125'     TO SPAR-IDTRANS                                    
023450       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
023460       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023500     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
023800 FA-LAES-GRUNDDATA SECTION.                                               
023810*    MOVE SPAR-KDBYTREF-ENTER  TO W-KDBYTREF                              
023900     IF MFS-FIRST                                                         
024000       PERFORM IMS-GN-WDGX3160                                            
024100     ELSE                                                                 
024110       MOVE SPAR-KDBYTREF-ENTER  TO W-KDBYTREF                            
024200       PERFORM IMS-GU-WDGX3160                                            
024300     END-IF                                                               
024400     .                                                                    
024501     EJECT                                                                
024502 G-KOLLA-INDATA SECTION.                                                  
024503     IF MID-FLUPDAT = 'N' OR 'D' OR 'C'                                   
024504       MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLUPDAT-ATTR                   
024505     ELSE                                                                 
024506       MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLUPDAT-ATTR                   
024507       MOVE NEJ                     TO INDATA-SW                          
024508     END-IF                                                               
024509     IF MID-KDBYTREF NOT = ALL '+'                                        
024510       MOVE MID-KDBYTREF             TO WS-KDBYTREF-NUM                   
024511       INSPECT WS-KDBYTREF-NUM REPLACING  LEADING SPACE BY ZERO           
024512       IF WS-KDBYTREF-NUM NUMERIC                                         
024513         MOVE MFS-NUM-FAELT-RAETT    TO MOD-KDBYTREF-ATTR                 
024514         MOVE WS-KDBYTREF-NUM        TO W-KDBYTREF                        
024515       ELSE                                                               
024516         MOVE MFS-NUM-FAELT-FEL      TO MOD-KDBYTREF-ATTR                 
024517         MOVE NEJ                    TO INDATA-SW                         
024518       END-IF                                                             
024519     END-IF                                                               
024520     IF MID-KVPOINT NOT = ALL '+'                                         
024521       IF MID-KVPOINT NUMERIC                                             
024522         MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVPOINT-ATTR                  
024524       ELSE                                                               
024525         MOVE MFS-NUM-FAELT-FEL      TO MOD-KVPOINT-ATTR                  
024526         MOVE NEJ                    TO INDATA-SW                         
024527       END-IF                                                             
024528     END-IF                                                               
024529     IF MID-FLUPDAT   = 'N'                                               
024530       IF MID-TENOTE = ALL '+'                                            
024532         MOVE MFS-ALFA-FAELT-FEL     TO MOD-TENOTE-ATTR                   
024533         MOVE NEJ                    TO INDATA-SW                         
024534       END-IF                                                             
024535       IF  MID-KVPOINT = ALL '+'                                          
024536         MOVE MFS-NUM-FAELT-FEL      TO MOD-KVPOINT-ATTR                  
024538         MOVE NEJ                    TO INDATA-SW                         
024539       END-IF                                                             
024540     END-IF                                                               
024541     IF MID-FLUPDAT   =  'C'                                              
024542       IF MID-TENOTE = ALL '+'  AND MID-KVPOINT = ALL '+'                 
024543         MOVE MFS-NUM-FAELT-FEL      TO MOD-KVPOINT-ATTR                  
024544         MOVE MFS-ALFA-FAELT-FEL     TO MOD-TENOTE-ATTR                   
024545         MOVE NEJ                    TO INDATA-SW                         
024546       END-IF                                                             
024547     END-IF                                                               
024548     IF INDATA-FEL                                                        
024549       MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                        
024550       CALL WMEDKONV USING MED-WMEDAREA                                   
024551       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
024552*      PERFORM MFS-LAES-IN-IGEN                                           
024553       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024554       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024555     ELSE                                                                 
024556* KOLLA OM POSTER FINNS ELLER INTE                                        
024557       IF MID-FLUPDAT = 'D'                                               
024558*        MOVE MID-KDBYTREF             TO W-KDBYTREF                      
024559         PERFORM IMS-GU-WDGX3160                                          
024560         IF SEGMENT-FINNS                                                 
024561           MOVE 'D'                    TO UPPDAT-SW                       
024562         ELSE                                                             
024563           MOVE NEJ                    TO INDATA-SW                       
024564           MOVE MFS-ALFA-FAELT-FEL     TO MOD-KDBYTREF-ATTR               
024565           MOVE ITEM-MISSING           TO MED-IDMFSFEL                    
024566           CALL WMEDKONV USING MED-WMEDAREA                               
024567           MOVE MED-MFSFEL             TO MOD-TEMFSFEL                    
024568         END-IF                                                           
024569       ELSE                                                               
024570*        MOVE MID-KDBYTREF             TO W-KDBYTREF                      
024571         PERFORM IMS-GU-WDGX3160                                          
024572         IF SEGMENT-FINNS                                                 
024573           MOVE 'R'                    TO UPPDAT-SW                       
024574         ELSE                                                             
024575           MOVE 'I'                    TO UPPDAT-SW                       
024576         END-IF                                                           
024577       END-IF                                                             
024578         IF INDATA-FEL                                                    
024579           MOVE ERR-CORR-HILITE-FLDS   TO MED-IDMFSFEL                    
024580           CALL WMEDKONV USING MED-WMEDAREA                               
024581           MOVE MED-MFSFEL             TO MOD-TEMFSFEL                    
024582*          PERFORM MFS-LAES-IN-IGEN                                       
024583           PERFORM MFS-ROER-EJ-FAELT-IN                                   
024584           PERFORM MFS-ROER-EJ-FAELT-UT                                   
024585         END-IF                                                           
024586     END-IF                                                               
024587     .                                                                    
024588     EJECT                                                                
024589 H-UPPDATERA SECTION.                                                     
024590     ACCEPT TRANS-TID FROM TIME                                           
024591     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DAGENS-DATUM                   
024592     MOVE TRANS-TID                  TO 3160-TIKLOCK                      
024593     MOVE WS-DAGENS-DATUM            TO 3160-DAREGDAT                     
024594     MOVE MSGI-IDUSER                TO 3160-IDUSER                       
024595     IF REPLACE-OK                                                        
024596        PERFORM IMS-GU-WDGX3160                                           
024597        IF SEGMENT-FINNS                                                  
024598          IF MID-TENOTE NOT = ALL '+'                                     
024599            MOVE MID-TENOTE     TO 3160-TENOTE                            
024600          END-IF                                                          
024601          IF MID-KVPOINT NOT = ALL '+'                                    
024602            MOVE MID-KVPOINT    TO 3160-KVPOINT                           
024603          END-IF                                                          
024604          PERFORM IMS-REPL-WDGX3160                                       
024605        END-IF                                                            
024606     ELSE                                                                 
024607       IF INSERT-OK                                                       
024608         MOVE W-KDBYTREF        TO 3160-KDBYTREF                          
024609         MOVE MID-TENOTE        TO 3160-TENOTE                            
024610         MOVE MID-KVPOINT       TO 3160-KVPOINT                           
024611         PERFORM IMS-ISRT-WDGX3160                                        
024612       ELSE                                                               
024613* DELETE SKER HÄR                                                         
024614*        MOVE MID-KDBYTREF      TO W-KDBYTREF                             
024615         PERFORM IMS-DLET-WDGX3160                                        
024616       END-IF                                                             
024617     END-IF                                                               
024618     MOVE UPDATING-OK          TO MED-IDMFSINF                            
024619     CALL WMEDKONV USING MED-WMEDAREA                                     
024620     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
024630     .                                                                    
024700     EJECT                                                                
024800 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025000*    --- ALLA UTDATA-FÄLT                                                 
025110*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025200     MOVE MFS-RENSA-FAELT TO MOD-KDBYTREF                                 
025300                             MOD-TENOTE                                   
025310                             MOD-KVPOINT                                  
025320                             MOD-FLUPDAT                                  
025400     .                                                                    
025501     SKIP3                                                                
025502 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
025503                                                                          
025504*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
025505     MOVE MFS-RENSA-FAELT TO MOD-TAB-KDBYTREF (INDX)                      
025506                             MOD-TAB-TENOTE   (INDX)                      
025507                             MOD-TAB-KVPOINT  (INDX)                      
025510     .                                                                    
025600     SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025900*    --- ALLA INDATA-FÄLT                                                 
026000     MOVE MFS-RENSA-FAELT TO MOD-KDBYTREF                                 
026100                             MOD-TENOTE                                   
026110                             MOD-KVPOINT                                  
026120                             MOD-FLUPDAT                                  
026200     .                                                                    
026300     EJECT                                                                
026400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026500                                                                          
026600*    --- ALLA UTDATA-FÄLT                                                 
026710*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
027001     MOVE +1 TO INDX                                                      
027002     PERFORM UNTIL INDX > MAX-INDX                                        
027003       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
027004       ADD +1 TO INDX                                                     
027005     END-PERFORM                                                          
027006     .                                                                    
027007     SKIP2                                                                
027008 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
027009                                                                          
027010*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027011     MOVE MFS-ROER-EJ-FAELT TO MOD-TAB-KDBYTREF (INDX)                    
027020                               MOD-TAB-TENOTE   (INDX)                    
027030                               MOD-TAB-KVPOINT  (INDX)                    
027040                                                                          
027050                                                                          
027100     .                                                                    
027200     SKIP3                                                                
027300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027400                                                                          
027500*    --- ALLA INDATA-FÄLT                                                 
027600     MOVE MFS-ROER-EJ-FAELT TO MOD-FLUPDAT                                
027700                               MOD-KDBYTREF                               
027800                               MOD-TENOTE                                 
027900                               MOD-KVPOINT                                
027910     .                                                                    
027920     EJECT                                                                
028000 MFS-FORM-ATTR SECTION.                                                   
028100                                                                          
028200*    --- ALLA INDATA-FÄLT                                                 
028300     MOVE MFS-FORMATETS-ATTR TO MOD-FLUPDAT-ATTR                          
028400                                MOD-KDBYTREF-ATTR                         
028410                                MOD-TENOTE-ATTR                           
028420                                MOD-KVPOINT-ATTR                          
028500     .                                                                    
028600     SKIP2                                                                
028700 MFS-LAES-IN-IGEN SECTION.                                                
028800                                                                          
028900*    --- ALLA INDATA-FÄLT                                                 
029000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLUPDAT-ATTR                       
029100                                   MOD-KDBYTREF-ATTR                      
029110                                   MOD-TENOTE-ATTR                        
029120                                   MOD-KVPOINT-ATTR                       
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
030600     IF MSGI-IDLAND-SPR = 'GB'                                            
030700       MOVE 'N' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031600     EJECT                                                                
031610 IMS-GN-WDGX3160 SECTION.                                                 
031620                                                                          
031630     STRING 'WDR101  (WDGXKEY  =' W-3160-IDHTYP-X ')'                     
031640          DELIMITED BY SIZE INTO SSA1                                     
031650     MOVE   'WDGX3160 ' TO SSA2                                           
031670     MOVE '  GEGB'             TO GODK-STATUSKODER                        
031680     CALL CBLTDLI USING GN   3160-PCB DLI-IO-WDGX3160 SSA2                
031690     MOVE 3160-STATUS-CODE TO STATUS-WS                                   
031691     PERFORM IMS-STATUSKONTROLL                                           
031692     .                                                                    
031693     SKIP3                                                                
031694 IMS-GU-WDGX3160 SECTION.                                                 
031695                                                                          
031696     STRING 'WDR101  (WDGXKEY  =' W-3160-IDHTYP-X ')'                     
031697          DELIMITED BY SIZE INTO SSA1                                     
031698     STRING 'WDGX3160(KDBYTREF =' W-KDBYTREF-X ')'                        
031699          DELIMITED BY SIZE INTO SSA2                                     
031700     MOVE '  GE'             TO GODK-STATUSKODER                          
031701     CALL CBLTDLI USING GHU  3160-PCB DLI-IO-WDGX3160 SSA2                
031702     MOVE 3160-STATUS-CODE TO STATUS-WS                                   
031703     PERFORM IMS-STATUSKONTROLL                                           
031704     .                                                                    
031705     SKIP3                                                                
031706 IMS-REPL-WDGX3160 SECTION.                                               
031707                                                                          
031708     MOVE '  '             TO GODK-STATUSKODER                            
031709     CALL CBLTDLI USING REPL 3160-PCB DLI-IO-WDGX3160                     
031710     MOVE 3160-STATUS-CODE TO STATUS-WS                                   
031711     PERFORM IMS-STATUSKONTROLL                                           
031712     .                                                                    
031713     SKIP3                                                                
031714 IMS-ISRT-WDGX3160 SECTION.                                               
031715                                                                          
031716     STRING 'WDR101  (WDGXKEY  =' W-3160-IDHTYP-X ')'                     
031717          DELIMITED BY SIZE INTO SSA1                                     
031718     MOVE   'WDGX3160 ' TO SSA2                                           
031720     MOVE '  II'  TO GODK-STATUSKODER                                     
031721     CALL CBLTDLI USING ISRT 3160-PCB DLI-IO-WDGX3160 SSA1 SSA2           
031722     MOVE 3160-STATUS-CODE TO STATUS-WS                                   
031723     PERFORM IMS-STATUSKONTROLL                                           
031724     .                                                                    
031725     SKIP3                                                                
031726 IMS-DLET-WDGX3160 SECTION.                                               
031727                                                                          
031728     MOVE '  '             TO GODK-STATUSKODER                            
031729     CALL CBLTDLI USING DLET 3160-PCB DLI-IO-WDGX3160                     
031730     MOVE 3160-STATUS-CODE TO STATUS-WS                                   
031731     PERFORM IMS-STATUSKONTROLL                                           
031732     .                                                                    
031733     SKIP3                                                                
031740 IMS-STATUSKONTROLL SECTION.                                              
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

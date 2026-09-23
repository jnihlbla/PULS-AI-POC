001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5021600.                                                
001500 AUTHOR.         MARKUS ASPFJÄLL.                                         
001600 DATE-WRITTEN.   98/06/25.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002000*        UPPDATERAR KDEKNIVA PÅ WLEEKA/WDH5 BASEN                         
002100*                                                                         
002210*        PROGRAMMET UPPDATERAR WDH5                                       
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W5T216                                              
002600*        MID:         W5I21601                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W5O21601                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W5021600'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 77  FLNIVA                      PIC X       VALUE 'N'.                   
004400                                                                          
004401*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004402 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004410 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004700                                                                          
004801 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004802     88  INDATA-OK                           VALUE 'J'.                   
004810     88  INDATA-FEL                          VALUE 'N'.                   
004820 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004830     88  ALLT-OK                             VALUE 'J'.                   
004840     88  ALLT-NOT-OK                         VALUE 'N'.                   
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '5216'.                
005600     88  GODK-MID                            VALUE '5211' '5212'          
005700                                                   '5213' '5214'          
005800                                                   '5215' '5216'          
005900                                                   '5217' '5218'          
006000                                                   '5219'.                
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
007800     03  SEGMENT-MISSING         PIC X(3)    VALUE '005'.                 
007810     03  SEG-FINNS-REDAN         PIC X(3)    VALUE '245'.                 
007820     03  LAST-PAGE               PIC X(3)    VALUE '106'.                 
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
008505     03  SPAR-IDTRANS           PIC X(4)    VALUE '5216'.                 
008506     03  SPAR-IDFTG-ENTER         PIC 9(2) VALUE ZERO.                    
008507     03  SPAR-IDFTG-NEXT          PIC 9(2) VALUE ZERO.                    
008508     03  SPAR-KDEKHHT-ENTER       PIC X(3).                               
008509     03  SPAR-KDEKHHT-NEXT        PIC X(3).                               
008510     03  SPAR-KDEKSHT-ENTER       PIC X(3).                               
008511     03  SPAR-KDEKSHT-NEXT        PIC X(3).                               
008512     03  SPAR-BEEKHHT-ENTER       PIC X(25).                              
008513     03  SPAR-BEEKHHT-NEXT        PIC X(25).                              
008514     03  SPAR-BEEKSHT-ENTER       PIC X(25).                              
008515     03  SPAR-BEEKSHT-NEXT        PIC X(25).                              
008520     03  SPAR-IDFTG               PIC 9(2).                               
008521     03  SPAR-KDEKHHT             PIC X(3).                               
008530     03  SPAR-KDEKSHT             PIC X(3).                               
008531     03  SPAR-BEEKHHT             PIC X(25).                              
008532     03  SPAR-BEEKSHT             PIC X(25).                              
008540     03  SPAR-KDEKNIVA            PIC X(5).                               
008541     03  SPAR-KDEKNIVA-NY         PIC X(5) VALUE SPACE.                   
008550     03  SPAR-KDEKNIVA-ENTER      PIC X(5).                               
008560     03  SPAR-KDEKNIVA-NEXT       PIC X(5).                               
008600     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W5I21601                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W5O21601                                                 
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
011002     03  W-WDH501KY-MIN-X.                                                
011003         05  W-WDH501KY-MIN    PIC X(5) VALUE LOW-VALUE.                  
011004                                                                          
011005     03  W-KDEKSHT-MIN-X.                                                 
011006         05  W-KDEKSHT-MIN     PIC X(3) VALUE LOW-VALUE.                  
011007                                                                          
011008     03  W-WDH501KY-X.                                                    
011009         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
011010         05  W-KDEKHHT           PIC X(3)    VALUE SPACE.                 
011011     03  W-KDEKSHT-X.                                                     
011012         05  W-KDEKSHT           PIC X(3)    VALUE SPACE.                 
011013     03  W-KDEKNIVA-X.                                                    
011020         05  W-KDEKNIVA-NY       PIC X(5)    VALUE SPACE.                 
011030     03  W-BEEKHHT               PIC X(25)   VALUE SPACE.                 
011040     03  W-BEEKSHT               PIC X(25)   VALUE SPACE.                 
011050                                                                          
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011610     88  BASEN-SLUT                          VALUE 'GB'.                  
011700     SKIP2                                                                
011800 01  GODK-STATUSKODER.                                                    
011900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012000     SKIP3                                                                
012100 01  SSA1                        PIC X(64).                               
012200 01  SSA2                        PIC X(64).                               
012210 01  SSA3                        PIC X(64).                               
012300     EJECT                                                                
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900                                                                          
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
013002 01  DLI-IO-WDH501.                                                       
013003*    03  -COPY WDH501                                                     
013004     EJECT                                                                
013005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
013006 01  DLI-IO-WDH511.                                                       
013007*    03  -COPY WDH511                                                     
013008     EJECT                                                                
013009 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH521'.                      
013010 01  DLI-IO-WDH521.                                                       
013020*    03  -COPY WDH521                                                     
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801                                                                          
013802*01  -COPY W0008  -PRE WDH5-                                              
013810     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014001 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDH5-PCB.                     
014002 MAIN SECTION.                                                            
014010     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDH5-PCB.                     
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014801         IF MFS-UPDATE                                                    
014802           PERFORM G-KOLLA-INPUT                                          
014803           IF INDATA-OK                                                   
014804             PERFORM H-UPPDATERA                                          
014805           END-IF                                                         
014810         ELSE                                                             
014901           IF MFS-FIRST                                                   
014902             PERFORM C-FOERSTA-SIDA                                       
014903           ELSE                                                           
014904             IF MFS-NEXT                                                  
014905               PERFORM D-NAESTA-SIDA                                      
014906             ELSE                                                         
014907               PERFORM E-SAMMA-SIDA                                       
014908             END-IF                                                       
014910           END-IF                                                         
015110         END-IF                                                           
015120         IF ALLT-OK                                                       
015200           PERFORM F-LAES-VISA-INFO                                       
015210         END-IF                                                           
015300       END-IF                                                             
015400*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
015500*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
015510                                                                          
015600       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21601 + 4                      
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
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I21601                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I21601                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W5O216N1' TO MFS-IDMOD                                         
018300     MOVE '5216' TO MOD-IDTRANS                                           
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
020200     MOVE '5216'            TO MSGI-IDTRANS                               
020210                                                                          
020220     IF EGEN-MID                                                          
020221       IF MID-KDEKHHT-IN NOT = ALL '+'                                    
020230         MOVE MID-KDEKHHT-IN TO MSGI-KDEKHHT                              
020240       END-IF                                                             
020241       IF MID-KDEKSHT-IN NOT = ALL '+'                                    
020244         MOVE MID-KDEKSHT-IN TO MSGI-KDEKSHT                              
020245       END-IF                                                             
020250     ELSE                                                                 
020300       IF GODK-MID                                                        
020400         IF MID-KDEKHHT-IN NUMERIC                                        
020401           MOVE MID-KDEKHHT-IN     TO MSGI-KDEKHHT                        
020405         END-IF                                                           
020406         IF MID-KDEKSHT-IN NOT = ALL '+'                                  
020408           MOVE MID-KDEKSHT-IN     TO MSGI-KDEKSHT                        
020409         END-IF                                                           
020410       END-IF                                                             
020420     END-IF                                                               
020500                                                                          
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020700                                                                          
020710     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
020800     MOVE JA TO NYCKLAR-SW                                                
020900     MOVE SPACE TO MOD-TEMFSINF                                           
021001     MOVE SPACE TO MOD-TEMFSFEL                                           
021002*    -- KONTROLL AV KDEKHHT                                               
021003     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
021004     IF EGEN-MID                                                          
021005       IF MID-KDEKHHT-IN NOT = ALL '+'                                    
021006         MOVE MID-KDEKHHT-IN TO MSGI-KDEKHHT                              
021007       END-IF                                                             
021008       IF MID-KDEKSHT-IN NOT = ALL '+'                                    
021009         MOVE MID-KDEKSHT-IN TO MSGI-KDEKSHT                              
021010       END-IF                                                             
021011     ELSE                                                                 
021012       IF GODK-MID                                                        
021013         IF MID-KDEKHHT-IN NUMERIC                                        
021014           MOVE MID-KDEKHHT-IN     TO MSGI-KDEKHHT                        
021015         END-IF                                                           
021016         IF MID-KDEKSHT-IN NOT = ALL '+'                                  
021017           MOVE MID-KDEKSHT-IN     TO MSGI-KDEKSHT                        
021018         END-IF                                                           
021019       END-IF                                                             
021020     END-IF                                                               
021034                                                                          
021035     IF EGEN-MID AND MID-KDEKHHT-IN NOT = ALL '+'                         
021036       MOVE '7'         TO MFS-IDPFK                                      
021037       MOVE SPACE       TO MFS-KDTRTYP                                    
021038     END-IF                                                               
021039     MOVE MSGI-KDEKHHT TO W-KDEKHHT                                       
021040                                                                          
021041     IF MSGI-IDFTG   NUMERIC AND MSGI-IDFTG   > ZERO                      
021042       MOVE MSGI-IDFTG      TO W-IDFTG                                    
021043                               MOD-IDFTG-UT                               
021044     END-IF                                                               
021045                                                                          
021046*    -- KONTROLL AV KDEKSHT                                               
021047     MOVE MFS-RENSA-FAELT TO MOD-KDEKSHT-IN                               
021048                                                                          
021049     IF EGEN-MID AND MID-KDEKSHT-IN NOT = ALL '+'                         
021050       MOVE '7'         TO MFS-IDPFK                                      
021051       MOVE SPACE       TO MFS-KDTRTYP                                    
021052     END-IF                                                               
021053     MOVE MSGI-KDEKSHT TO W-KDEKSHT                                       
021054                                                                          
021055     IF MSGI-KDEKHHT NUMERIC AND MSGI-KDEKSHT NOT = ALL '+'               
021056       MOVE MSGI-KDEKSHT TO MOD-KDEKSHT-UT                                
021057       MOVE MSGI-KDEKHHT TO MOD-KDEKHHT-UT                                
021058     ELSE                                                                 
021059       MOVE NEJ TO NYCKLAR-SW                                             
021060     END-IF                                                               
021062                                                                          
021063*------ KONTROLL AV KDEKNIVA-------------------*                          
021064                                                                          
021065     IF MFS-FIRST AND EGEN-MID                                            
021066       MOVE MFS-RENSA-FAELT TO MOD-KDEKNIVA-NY                            
021067     ELSE                                                                 
021068       IF EGEN-MID                                                        
021069         IF MID-KDEKNIVA-NY NOT = ALL '+'                                 
021070           MOVE MID-KDEKNIVA-NY TO SPAR-KDEKNIVA-NY                       
021071                                   W-KDEKNIVA-NY                          
021072                                   MOD-KDEKNIVA-NY                        
021073           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDEKNIVA-NY-ATTR              
021074         ELSE                                                             
021075           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDEKNIVA-NY-ATTR              
021076         END-IF                                                           
021080         IF MID-KDEKNIVA-NY = ALL '+'                                     
021090           MOVE SPACE                TO SPAR-KDEKNIVA-NY                  
021100           MOVE MFS-RENSA-FAELT      TO MOD-KDEKNIVA-NY                   
021110         END-IF                                                           
021111       END-IF                                                             
021112       IF NOT EGEN-MID                                                    
021113         MOVE SPACE                TO SPAR-KDEKNIVA-NY                    
021114         MOVE MFS-RENSA-FAELT      TO MOD-KDEKNIVA-NY                     
021115       END-IF                                                             
021116     END-IF                                                               
021120*-----SLUT PÅ KDEKNIVA-------------------------*                          
021200                                                                          
021210     IF NOT GODK-MID                                                      
021220       MOVE MFS-RENSA-FAELT TO MID-KDEKHHT-IN                             
021230       MOVE MFS-RENSA-FAELT TO MID-KDEKSHT-IN                             
021240       MOVE MFS-RENSA-FAELT TO MID-KDEKNIVA-NY                            
021250       MOVE NEJ TO ALLT-SW                                                
021260     END-IF                                                               
021270     IF GODK-MID                                                          
021300       IF NYCKLAR-FEL                                                     
021400         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
021500         CALL WMEDKONV USING MED-WMEDAREA                                 
021600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
021700         PERFORM MFS-RENSA-FAELT-IN                                       
021800         PERFORM MFS-RENSA-FAELT-UT                                       
021900       END-IF                                                             
021910     END-IF                                                               
022000     .                                                                    
022101     EJECT                                                                
022102 C-FOERSTA-SIDA SECTION.                                                  
022103                                                                          
022110     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
022111     CALL WMEDKONV USING MED-WMEDAREA                                     
022112     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
022113                                                                          
022114     PERFORM MFS-RENSA-FAELT-IN                                           
022115     .                                                                    
022116     EJECT                                                                
022117 D-NAESTA-SIDA SECTION.                                                   
022118                                                                          
022119     IF SPAR-IDTRANS = '5216'                                             
022120       IF SPAR-IDFTG-NEXT   = ZERO                                        
022121         MOVE SPAR-IDFTG-ENTER  TO W-IDFTG                                
022122                                   MOD-IDFTG-UT                           
022123       ELSE                                                               
022124         MOVE SPAR-IDFTG-NEXT   TO W-IDFTG                                
022125                                   MOD-IDFTG-UT                           
022126       END-IF                                                             
022127       IF SPAR-KDEKHHT-NEXT = SPACE                                       
022128         MOVE SPAR-KDEKHHT-ENTER TO W-KDEKHHT                             
022129                                    MOD-KDEKHHT-UT                        
022130       ELSE                                                               
022131         MOVE SPAR-KDEKHHT-NEXT TO W-KDEKHHT                              
022132                                   MOD-KDEKHHT-UT                         
022133       END-IF                                                             
022134       IF SPAR-KDEKSHT-NEXT = SPACE                                       
022135         MOVE SPAR-KDEKSHT-ENTER TO W-KDEKSHT                             
022136                                    MOD-KDEKSHT-UT                        
022137       ELSE                                                               
022138         MOVE SPAR-KDEKSHT-NEXT TO W-KDEKSHT                              
022139                                   MOD-KDEKSHT-UT                         
022140       END-IF                                                             
022141       IF SPAR-KDEKNIVA-NEXT = SPACE                                      
022142         MOVE SPAR-KDEKNIVA-ENTER TO W-KDEKNIVA-NY                        
022143       ELSE                                                               
022144         MOVE SPAR-KDEKNIVA-NEXT TO W-KDEKNIVA-NY                         
022145       END-IF                                                             
022146                                                                          
022147     ELSE                                                                 
022148       PERFORM MFS-RENSA-FAELT-IN                                         
022149     END-IF                                                               
022150     IF (SPAR-KDEKHHT-NEXT = SPACE OR SPAR-IDFTG-NEXT = ZERO)             
022151                                  AND SPAR-KDEKSHT-NEXT = SPACE           
022152                                                                          
022153       MOVE LAST-PAGE TO MED-IDMFSINF                                     
022154       CALL WMEDKONV USING MED-WMEDAREA                                   
022155       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
022156     END-IF                                                               
022157     .                                                                    
022158     EJECT                                                                
022159 E-SAMMA-SIDA SECTION.                                                    
022160                                                                          
022161     IF SPAR-IDTRANS = '5216' OR '0551'                                   
022162       MOVE SPAR-IDFTG-ENTER   TO W-IDFTG                                 
022163       MOVE SPAR-KDEKHHT-ENTER TO W-KDEKHHT                               
022164       MOVE SPAR-KDEKSHT-ENTER TO W-KDEKSHT                               
022165       MOVE SPAR-KDEKNIVA-NY   TO W-KDEKNIVA-NY                           
022166       MOVE W-IDFTG            TO MOD-IDFTG-UT                            
022167       MOVE W-KDEKHHT          TO MOD-KDEKHHT-UT                          
022168       MOVE W-KDEKSHT          TO MOD-KDEKSHT-UT                          
022169       IF SPAR-KDEKNIVA-NY = SPACE                                        
022170         MOVE MFS-RENSA-FAELT TO MOD-KDEKNIVA-NY                          
022171       END-IF                                                             
022172                                                                          
022173     ELSE                                                                 
022174         PERFORM MFS-RENSA-FAELT-IN                                       
022175         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
022176         CALL WMEDKONV USING MED-WMEDAREA                                 
022177         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
022178         PERFORM EA-MID-INDATA-TILL-MOD                                   
022179     END-IF                                                               
022180     .                                                                    
022181     EJECT                                                                
022182 EA-MID-INDATA-TILL-MOD SECTION.                                          
022183                                                                          
022184* * * * * FÖR VARJE MID-FÄLT                                              
022185* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
022186* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
022187* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
022188     IF MID-KDEKHHT-IN NOT = ALL '+'                                      
022189       MOVE MID-KDEKHHT-IN TO MOD-KDEKHHT-UT                              
022190     END-IF                                                               
022191     IF MID-KDEKSHT-IN NOT = ALL '+'                                      
022192       MOVE MID-KDEKSHT-IN TO MOD-KDEKSHT-UT                              
022193     END-IF                                                               
022194                                                                          
022195     .                                                                    
022200     EJECT                                                                
022400 F-LAES-VISA-INFO SECTION.                                                
022500                                                                          
022600     PERFORM IMS-GET-WDH5-HHT                                             
022700                                                                          
022800     IF SEGMENT-SAKNAS                                                    
022801       MOVE 'HUV.HÄNDELSE SAKNAS' TO  MOD-TEMFSFEL                        
022805     ELSE                                                                 
022809       MOVE ZERO  TO SPAR-IDFTG-NEXT                                      
022810       MOVE SPACE TO SPAR-KDEKHHT-NEXT                                    
022811       MOVE SPACE TO SPAR-BEEKHHT-NEXT                                    
022812       MOVE HHT-IDFTG   TO SPAR-IDFTG-ENTER                               
022813                           MOD-IDFTG-UT                                   
022814       MOVE HHT-KDEKHHT TO SPAR-KDEKHHT-ENTER                             
022815                           MOD-KDEKHHT-UT                                 
022816       MOVE HHT-BEEKHHT TO SPAR-BEEKHHT-ENTER                             
022817                                                                          
022818       PERFORM IMS-GET-WDH5-SHT                                           
022820       IF SEGMENT-SAKNAS                                                  
023100         MOVE 'SUB HÄNDELSE SAKNAS' TO  MOD-TEMFSFEL                      
023210       ELSE                                                               
023410         MOVE SPACE TO SPAR-KDEKSHT-NEXT                                  
023411         MOVE SPACE TO SPAR-BEEKSHT-NEXT                                  
023412         MOVE SHT-KDEKSHT TO SPAR-KDEKSHT-ENTER                           
023413                             MOD-KDEKSHT-UT                               
023414         MOVE SHT-BEEKSHT TO SPAR-BEEKSHT-ENTER                           
023415         IF MFS-NEXT                                                      
023416           PERFORM IMS-GU-WDH5-NIVA                                       
023417         ELSE                                                             
023430           PERFORM IMS-GNP-WDH5-NIVA                                      
023431         END-IF                                                           
023432         IF SPAR-KDEKNIVA-NY NOT = SPACE                                  
023433                                                                          
023434           IF MFS-UPDATE                                                  
023435             CONTINUE                                                     
023436           ELSE                                                           
023437             IF NIVA-KDEKNIVA = SPAR-KDEKNIVA-NY                          
023438               MOVE JA TO FLNIVA                                          
023439             END-IF                                                       
023440           END-IF                                                         
023441         END-IF                                                           
023447         MOVE +1 TO INDX                                                  
023448         MOVE NIVA-KDEKNIVA TO SPAR-KDEKNIVA-ENTER                        
023449         MOVE HHT-KDEKHHT   TO MOD-KDEKHHT (INDX)                         
023450         MOVE HHT-BEEKHHT   TO MOD-BEEKHHT (INDX)                         
023451         MOVE SHT-KDEKSHT   TO MOD-KDEKSHT (INDX)                         
023452         MOVE SHT-BEEKSHT   TO MOD-BEEKSHT (INDX)                         
023453         MOVE NIVA-KDEKNIVA TO MOD-KDEKNIVA (INDX)                        
023454         MOVE HHT-KDEKHHT   TO MOD-KDEKHHT-NY                             
023455         MOVE HHT-BEEKHHT   TO MOD-BEEKHHT-NY                             
023456         MOVE SHT-KDEKSHT   TO MOD-KDEKSHT-NY                             
023457         MOVE SHT-BEEKSHT   TO MOD-BEEKSHT-NY                             
023458                                                                          
023459         PERFORM UNTIL INDX > MAX-INDX OR BASEN-SLUT OR                   
023460                              SEGMENT-SAKNAS                              
023461         IF SEGMENT-FINNS                                                 
023473           IF INDX > 1                                                    
023474             MOVE NIVA-KDEKNIVA TO MOD-KDEKNIVA (INDX)                    
023475           END-IF                                                         
023476           IF SPAR-KDEKNIVA-NY NOT = SPACE                                
023477                                                                          
023478             IF MFS-UPDATE                                                
023479               CONTINUE                                                   
023480             ELSE                                                         
023481               IF NIVA-KDEKNIVA = SPAR-KDEKNIVA-NY                        
023482                 MOVE JA TO FLNIVA                                        
023484               END-IF                                                     
023485             END-IF                                                       
023486           END-IF                                                         
023487           ADD 1 TO INDX                                                  
023489           PERFORM IMS-GNP-WDH5-NIVA                                      
023491         ELSE                                                             
023492             MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT (INDX)                   
023493                                   MOD-BEEKHHT (INDX)                     
023494                                   MOD-KDEKSHT (INDX)                     
023495                                   MOD-BEEKSHT (INDX)                     
023496                                   MOD-KDEKNIVA (INDX)                    
023497             ADD 1 TO INDX                                                
023498         END-IF                                                           
023499         END-PERFORM                                                      
023500                                                                          
023501         IF SEGMENT-FINNS                                                 
023502           MOVE HHT-IDFTG     TO SPAR-IDFTG-NEXT                          
023503           MOVE HHT-KDEKHHT   TO SPAR-KDEKHHT-NEXT                        
023504           MOVE SHT-KDEKSHT   TO SPAR-KDEKSHT-NEXT                        
023505           MOVE NIVA-KDEKNIVA TO SPAR-KDEKNIVA-NEXT                       
023506           IF MOD-TEMFSINF = SPACE                                        
023507             MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                    
023508             CALL WMEDKONV USING MED-WMEDAREA                             
023509             MOVE MED-TEMFSINF TO MOD-TEMFSINF                            
023510           END-IF                                                         
023511         END-IF                                                           
023512                                                                          
023513       END-IF                                                             
023514       IF FLNIVA = 'J' AND MOD-TEMFSINF = SPACE                           
023515         MOVE 'NIVA FINNS REDAN' TO MOD-TEMFSINF                          
023516       ELSE                                                               
023517         IF FLNIVA = 'N' AND SPAR-KDEKNIVA-NY NOT = SPACE                 
023518                         AND MOD-TEMFSINF = SPACE                         
023519           MOVE 'NIVA FINNS INTE'  TO MOD-TEMFSINF                        
023520         END-IF                                                           
023521       END-IF                                                             
023522                                                                          
023523       MOVE '002'      TO MSGI-KDCALL                                     
023524       MOVE '5216'     TO SPAR-IDTRANS                                    
023525       MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                  
023526       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
023530     END-IF                                                               
023600     .                                                                    
023700     EJECT                                                                
024602 G-KOLLA-INPUT SECTION.                                                   
024603                                                                          
024604     MOVE JA  TO INDATA-SW                                                
024605     IF MID       = ALL '+'                                               
024606       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024607       CALL WMEDKONV USING MED-WMEDAREA                                   
024608       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024609       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024610       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024611       MOVE NEJ TO INDATA-SW                                              
024612       MOVE NEJ TO ALLT-SW                                                
024613     ELSE                                                                 
024614                                                                          
024615       IF W-KDEKHHT = ALL '+'                                             
024616          MOVE NEJ TO INDATA-SW                                           
024617       END-IF                                                             
024620       IF W-KDEKSHT = ALL '+'                                             
024622          MOVE NEJ TO INDATA-SW                                           
024623       END-IF                                                             
024626                                                                          
024627       IF MID-KDEKNIVA-NY NOT = ALL '+'                                   
024628         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDEKNIVA-NY-ATTR                
024629       ELSE                                                               
024630         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDEKNIVA-NY-ATTR                  
024631         MOVE NEJ TO INDATA-SW                                            
024634       END-IF                                                             
024635                                                                          
024650                                                                          
024651       IF INDATA-FEL                                                      
024652         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
024653         CALL WMEDKONV USING MED-WMEDAREA                                 
024654         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024655         PERFORM MFS-ROER-EJ-FAELT-UT                                     
024656         PERFORM MFS-ROER-EJ-FAELT-IN                                     
024657       END-IF                                                             
024668     END-IF                                                               
024669     .                                                                    
024670     EJECT                                                                
024671 H-UPPDATERA SECTION.                                                     
024672                                                                          
024673     PERFORM IMS-GET-WDH511                                               
024674     IF SEGMENT-FINNS                                                     
024675       MOVE W-IDFTG     TO MOD-IDFTG-UT                                   
024676       MOVE W-KDEKHHT   TO MOD-KDEKHHT-UT                                 
024677       MOVE SHT-KDEKSHT TO MOD-KDEKSHT-UT                                 
024678       IF MID-KDEKNIVA-NY NOT = ALL '+'                                   
024679         MOVE MID-KDEKNIVA-NY TO NIVA-KDEKNIVA                            
024680                                 W-KDEKNIVA-NY                            
024681         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDEKNIVA-NY-ATTR               
024685           PERFORM IMS-ISRT-WDH5-NIVA                                     
024686           IF SEGMENT-FINNS                                               
024687             MOVE INF-UPDATE-DONE TO MED-IDMFSINF                         
024688             MOVE MFS-RENSA-FAELT TO MOD-KDEKNIVA-NY                      
024689             CALL WMEDKONV USING MED-WMEDAREA                             
024690             MOVE MED-MFSINF TO MOD-TEMFSINF                              
024691             PERFORM MFS-FORM-ATTR                                        
024692             PERFORM MFS-RENSA-FAELT-IN                                   
024693           ELSE                                                           
024694             IF SEGMENT-FINNS-REDAN                                       
024695               MOVE SEG-FINNS-REDAN TO MED-IDMFSINF                       
024696               CALL WMEDKONV USING MED-WMEDAREA                           
024697               MOVE MED-MFSINF TO MOD-TEMFSINF                            
024698               MOVE SPACE TO W-KDEKNIVA-NY                                
024699                             SPAR-KDEKNIVA-NY                             
024700               MOVE NEJ TO ALLT-SW                                        
024701               PERFORM MFS-ROER-EJ-FAELT-UT                               
024702             END-IF                                                       
024703           END-IF                                                         
024704       ELSE                                                               
024705         MOVE MFS-ROER-EJ-FAELT TO MOD-KDEKHHT-IN                         
024706         MOVE MFS-ROER-EJ-FAELT TO MOD-KDEKSHT-IN                         
024707       END-IF                                                             
024708     ELSE                                                                 
024709       MOVE SEGMENT-MISSING TO MED-IDMFSINF                               
024710       CALL WMEDKONV USING MED-WMEDAREA                                   
024711       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
024712       PERFORM MFS-FORM-ATTR                                              
024713       PERFORM MFS-RENSA-FAELT-IN                                         
024714     END-IF                                                               
024715     .                                                                    
024720     EJECT                                                                
024800 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025000*    --- ALLA UTDATA-FÄLT                                                 
025110*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025200     MOVE MFS-RENSA-FAELT TO MOD-IDFTG-UT                                 
025210                             MOD-KDEKHHT-UT                               
025300                             MOD-KDEKSHT-UT                               
025301                             MOD-KDEKNIVA-NY                              
025310     PERFORM MFS-RENSA-RAD-FAELT-UT                                       
025320                                                                          
025400     .                                                                    
025501     SKIP3                                                                
025502 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
025503                                                                          
025504*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
025505     MOVE +1 TO INDX                                                      
025506     PERFORM UNTIL INDX > MAX-INDX                                        
025507     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT (INDX)                           
025508                             MOD-BEEKHHT (INDX)                           
025509                             MOD-KDEKSHT (INDX)                           
025510                             MOD-BEEKSHT (INDX)                           
025511                             MOD-KDEKNIVA (INDX)                          
025512       ADD +1 TO INDX                                                     
025513     END-PERFORM                                                          
025520     .                                                                    
025600     SKIP3                                                                
025700 MFS-RENSA-FAELT-IN SECTION.                                              
025800                                                                          
025900*    --- ALLA INDATA-FÄLT                                                 
026000     MOVE MFS-RENSA-FAELT TO MID-KDEKHHT-IN                               
026100                             MID-KDEKSHT-IN                               
026101                             MID-KDEKNIVA-NY                              
026110                                                                          
026120                                                                          
026200     .                                                                    
026300     EJECT                                                                
026400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026500                                                                          
026600*    --- ALLA UTDATA-FÄLT                                                 
026710*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
026800     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFTG-UT                               
026810                               MOD-KDEKHHT-UT                             
026900                               MOD-KDEKSHT-UT                             
027000                               MOD-KDEKSHT-UT                             
027001                               MOD-KDEKHHT-NY                             
027002                               MOD-BEEKHHT-NY                             
027003                               MOD-KDEKSHT-NY                             
027004                               MOD-BEEKSHT-NY                             
027005     MOVE +1 TO INDX                                                      
027006     PERFORM UNTIL INDX > MAX-INDX                                        
027007       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
027008       ADD +1 TO INDX                                                     
027009     END-PERFORM                                                          
027010     SKIP2                                                                
027011     .                                                                    
027012     EJECT                                                                
027013 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
027014                                                                          
027015*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027016     MOVE MFS-ROER-EJ-FAELT TO MOD-KDEKHHT (INDX)                         
027017                               MOD-BEEKHHT (INDX)                         
027018                               MOD-KDEKSHT (INDX)                         
027019                               MOD-BEEKSHT (INDX)                         
027020                               MOD-KDEKNIVA (INDX)                        
027030                                                                          
027100     .                                                                    
027200     SKIP3                                                                
027300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027400                                                                          
027500*    --- ALLA INDATA-FÄLT                                                 
027600     MOVE MFS-ROER-EJ-FAELT TO MOD-KDEKHHT-IN                             
027700                                MOD-KDEKSHT-IN                            
027710                                MOD-KDEKNIVA-NY                           
027800     .                                                                    
027900     EJECT                                                                
028000 MFS-FORM-ATTR SECTION.                                                   
028100                                                                          
028200*    --- ALLA INDATA-FÄLT                                                 
028300     MOVE MFS-FORMATETS-ATTR TO                                           
028400                                                                          
028410                                MOD-KDEKNIVA-NY-ATTR                      
028500     .                                                                    
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
030600*    IF MSGI-IDLAND-SPR = 'GB'                                            
030700*      MOVE 'N' TO MFS-KDHUVOMR                                           
030800*    END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-GET-WDH5-HHT SECTION.                                                
031503                                                                          
031504     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031505          DELIMITED BY SIZE INTO SSA1                                     
031506     MOVE '  GE' TO GODK-STATUSKODER                                      
031507     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH501 SSA1                    
031508     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031509     PERFORM IMS-STATUSKONTROLL                                           
031510     .                                                                    
031511     EJECT                                                                
031512 IMS-GET-WDH5-SHT SECTION.                                                
031513                                                                          
031514     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031515          DELIMITED BY SIZE INTO SSA1                                     
031516     MOVE '  GE' TO GODK-STATUSKODER                                      
031517     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH511 SSA1                    
031518     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031519     PERFORM IMS-STATUSKONTROLL                                           
031520     .                                                                    
031521     EJECT                                                                
031522 IMS-GNP-WDH5-NIVA SECTION.                                               
031523                                                                          
031524     STRING 'WDH521   '                                                   
031526          DELIMITED BY SIZE INTO SSA1                                     
031527     MOVE '  GE' TO GODK-STATUSKODER                                      
031528     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH521 SSA1                   
031529     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031530     PERFORM IMS-STATUSKONTROLL                                           
031531     .                                                                    
031532     SKIP3                                                                
031533 IMS-GU-WDH5-NIVA SECTION.                                                
031534                                                                          
031539     STRING 'WDH521  (KDEKNIVA =' W-KDEKNIVA-X ')'                        
031540          DELIMITED BY SIZE INTO SSA1                                     
031541     MOVE '  GE' TO GODK-STATUSKODER                                      
031542     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH521 SSA1                   
031543                                                                          
031544     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031545     PERFORM IMS-STATUSKONTROLL                                           
031546     .                                                                    
031547     SKIP3                                                                
031548 IMS-ISRT-WDH5-NIVA SECTION.                                              
031549                                                                          
031550     MOVE 'WDH521   ' TO SSA1                                             
031551     MOVE '  II' TO GODK-STATUSKODER                                      
031552     CALL CBLTDLI USING ISRT WDH5-PCB DLI-IO-WDH521 SSA1                  
031553     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031554     PERFORM IMS-STATUSKONTROLL                                           
031560     .                                                                    
031600     EJECT                                                                
031610 IMS-GET-WDH511   SECTION.                                                
031620                                                                          
031630     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031640          DELIMITED BY SIZE INTO SSA1                                     
031641     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031642          DELIMITED BY SIZE INTO SSA2                                     
031650     MOVE '  GE' TO GODK-STATUSKODER                                      
031660     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH511 SSA1 SSA2               
031670     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031680     PERFORM IMS-STATUSKONTROLL                                           
031690     .                                                                    
031691     EJECT                                                                
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

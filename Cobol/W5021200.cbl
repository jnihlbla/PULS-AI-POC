001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5021200.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/06/08.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
001910*        PROGRAM SOM LÄSER DETALJINFO. FRÅN DB(WDR9).                     
001920*        START FRÅN BILD 5211 OCH ÅTERHOPP TILL SAMMA BILD                
001930*        M.H.A "ENTER"-TRYCKNING.                                         
002100*                                                                         
002201*        PROGRAMMET LÄSER      WLSAPA (WDR9)                              
002210*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
002220*        PROGRAMMET LÄSER      WLEKKA (WDH5)                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W5T212                                              
002600*        MID:         W5I212N1                                            
002700*                                                                         
002800*    UTDATA.                                                              
002900*        MOD:         W5O212N1                                            
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003600 77  IDPGM                       PIC X(08)   VALUE 'W5021200'.            
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004610     88  ALLT-OK                             VALUE 'J'.                   
004700                                                                          
004800 77  BYT-SW                      PIC X       VALUE 'J'.                   
004810     88  BYT-BILD                            VALUE 'J'.                   
004820     88  BYT-EJ-BILD                         VALUE 'N'.                   
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005500     88  EGEN-MID                            VALUE '5212'.                
005600     88  GODK-MID                            VALUE '5211'                 
005700                                                   '5212'.                
006100     88  HELP-MID                            VALUE '0551'.                
006200     EJECT                                                                
006201                                                                          
006202 77  IX                          PIC S9(3)  VALUE ZERO COMP-3.            
006203 77  MAX-IX                      PIC S9(3)  VALUE +7   COMP-3.            
006212                                                                          
006213*    ---AREA FÖR ATT HÄMTA UPP MSGI-SPAR-AREA TILL PGM                    
006214 01  SPAR-AREA.                                                           
006215     03  SPAR-IDPGM              PIC X(8)    VALUE SPACE.                 
006216     03  SPAR-IDPGM-TAB          PIC X(8)    VALUE SPACE.                 
006217     03  SPAR-DAREGDAT           PIC 9(8)    VALUE ZERO.                  
006218     03  SPAR-TIKLOCK            PIC S9(9) COMP-3.                        
006219     03  SPAR-IDSEKVNR           PIC S9(3) COMP-3.                        
006220     03  SPAR-FROM-DAREGDAT      PIC 9(8)    VALUE ZERO.                  
006221     03  SPAR-TOM-DAREGDAT       PIC 9(8)    VALUE ZERO.                  
006222     03  SPAR-BILD               PIC X(4).                                
006223     03  SPAR-TOM-DATUM          PIC 9(8).                                
006230     03  SPAR-FROM-DATUM         PIC 9(8).                                
006240     03  SPAR-IDTRANS            PIC X(4).                                
006250     03  FILLER                 PIC X(16) VALUE 'ENTER-NYCKLAR'.          
006260     03  SPAR-ENTER.                                                      
006270         05  SPAR-DAREGDAT-MIN-ENTER PIC 9(8).                            
006280         05  SPAR-DAREGDAT-MAX-ENTER PIC 9(8).                            
006290         05  SPAR-IDPGM-ENTER        PIC X(8).                            
006291         05  SPAR-DAREGDAT-ENTER     PIC 9(8).                            
006292         05  SPAR-TIKLOCK-ENTER      PIC S9(9) COMP-3.                    
006293         05  SPAR-IDSEKVNR-ENTER     PIC S9(3) COMP-3.                    
006294     03  FILLER                 PIC X(16) VALUE 'NEXT-NYCKLAR'.           
006295     03  SPAR-NEXT.                                                       
006296         05  SPAR-DAREGDAT-MIN-NEXT  PIC 9(8).                            
006297         05  SPAR-DAREGDAT-MAX-NEXT  PIC 9(8).                            
006298         05  SPAR-DAREGDAT-NEXT      PIC 9(8).                            
006299         05  SPAR-TIKLOCK-NEXT       PIC 9(9) COMP-3.                     
006300         05  SPAR-IDSEKVNR-NEXT      PIC S9(3) COMP-3.                    
006301         05  SPAR-IDPGM-NEXT         PIC X(8).                            
006302     03  FEL-TABELL.                                                      
006303       05  FEL-WDR901   OCCURS 13.                                        
006304         07  FEL-WDR901KY-TAB.                                            
006305           09  FIL-IDPGM-TAB     PIC X(8).                                
006306           09  FIL-DAREGDAT-TAB  PIC 9(8).                                
006307           09  FIL-TIKLOCK-TAB   PIC S9(9)  COMP-3.                       
006308           09  FIL-IDSEKVNR-TAB  PIC S9(3)  COMP-3.                       
006309     03   SPAR-ERROR             PIC S9(7).                               
006320     EJECT                                                                
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
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008100*                                                                         
008200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008300     SKIP3                                                                
008400*01 -COPY WMSGINIT                                                        
008600     EJECT                                                                
008610 01  FILLER                      PIC X(16)   VALUE 'WWIDFTG '.            
008620     SKIP3                                                                
008630*01 -COPY WWIDFTG                                                         
008640     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W5I21201                                                   
009240     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
009700     03  MOD REDEFINES MSG-AREA.                                          
009800*      05  -COPY W5O21201                                                 
009900     EJECT                                                                
009910 01  W-PROG-TO-PROG-SW-5211.                                              
009920     03  M-SW-LL-5211            PIC S9(4)   VALUE +240 COMP SYNC.        
009930     03  M-SW-Z1-Z2-5211         PIC X(2)    VALUE LOW-VALUE.             
009940     03  M-SW-KDTRANS-5211       PIC X(8)    VALUE 'W5T211  '.            
009950     03  M-SW-IDTRANS-5211       PIC X(4)    VALUE '5212'.                
009960     03  M-SW-KDMFSTYP-5211      PIC X(1)    VALUE '2'.                   
009970                                                                          
009980*    03  MID -COPY W5I21101 -PRE 5211-                                    
009990     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010100     SKIP3                                                                
010200*01  -COPY WMFSAREA                                                       
010330     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011008     03  W-IDARTNR-WDD3-X.                                                
011009         05  W-IDARTNR-WDD3      PIC S9(9)  VALUE ZERO COMP-3.            
011010     03  W-IDSKYLT-X.                                                     
011020         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
011100     SKIP2                                                                
011137     03  W-WDR901KY-X.                                                    
011138         05  W-IDPGM             PIC X(8)    VALUE SPACE.                 
011139         05  W-DAREGDAT          PIC 9(8)    VALUE ZERO.                  
011140         05  W-TIKLOCK           PIC S9(9) COMP-3.                        
011141         05  W-IDSEKVNR          PIC S9(3) COMP-3.                        
011142         05  W-IDCPYTXT          PIC X(8)    VALUE 'W510EKFA'.            
011143                                                                          
011172     03  W-WDH501KY-X.                                                    
011173         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
011174         05  W-KDEKHHT           PIC X(3)    VALUE ZERO.                  
011175                                                                          
011180     SKIP2                                                                
011210*    --- STATUS-KOD FRÅN IMS                                              
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
012900                                                                          
013001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR901'.                      
013002 01  DLI-IO-WDR901.                                                       
013003*    03  -COPY WDR901                                                     
013004*    05  -COPY W510EKHA -RED FIL-WDR901-DATA                              
013005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
013006 01  DLI-IO-WDD301.                                                       
013010*    03  -COPY WDD311                                                     
013310 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
013320 01  DLI-IO-WDH501.                                                       
013330*    03  -COPY WDH501                                                     
013392     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500*01  -COPY W0009   -PRE MSG-                                              
013510     EJECT                                                                
013520*01  -COPY W0009   -PRE ALT-                                              
013530     EJECT                                                                
013600*01  -COPY W0008   -PRE USEA-                                             
013700     05  FILLER                  PIC X.                                   
013801                                                                          
013802*01  -COPY W0008  -PRE WDR9-                                              
013803     05  FILLER                  PIC X.                                   
013804                                                                          
013805*01  -COPY W0008  -PRE WDD3-                                              
013810     05  FILLER                  PIC X.                                   
013900                                                                          
014000*01  -COPY W0008  -PRE WDH5-                                              
014001     05  FILLER                  PIC X.                                   
014014     EJECT                                                                
014015 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
014016                           WDR9-PCB WDD3-PCB WDH5-PCB.                    
014019 MAIN SECTION.                                                            
014020     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
014021                           WDR9-PCB WDD3-PCB WDH5-PCB.                    
014100                                                                          
014300     PERFORM IMS-GET-MSG                                                  
014400     IF SEGMENT-FINNS                                                     
014500       PERFORM A-INIT                                                     
014600       PERFORM B-KOLLA-NYCKLAR                                            
014700       IF NYCKLAR-OK                                                      
014800          IF MFS-ENTER AND EGEN-MID                                       
014900             PERFORM C-BYT-BILD                                           
015000          END-IF                                                          
015100          IF ALLT-OK                                                      
015200             PERFORM F-LAES-VISA-INFO                                     
015210          END-IF                                                          
015300       END-IF                                                             
015310       IF BYT-EJ-BILD                                                     
015600          COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21201 + 4                   
015700          PERFORM IMS-INSERT-MSG                                          
015710       END-IF                                                             
015800     END-IF                                                               
016000                                                                          
016100     MOVE ZERO TO RETURN-CODE                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500 A-INIT SECTION.                                                          
016600                                                                          
016700     IF MSG-DUBBLA-TRANSKODER                                             
016800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I21201                 
016900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017100     ELSE                                                                 
017200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I21201                  
017300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017500     END-IF                                                               
017600                                                                          
017700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
017800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
017900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018000                                                                          
018100     MOVE LOW-VALUE TO MSG-AREA                                           
018200     MOVE 'W5O212N1' TO MFS-IDMOD                                         
018300     MOVE '5212' TO MOD-IDTRANS                                           
018400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018410                                                                          
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
019601                                                                          
019610     MOVE JA TO NYCKLAR-SW                                                
019700                                                                          
019710* ---HÄR GÖRS INGEN KONTROLL AV NYCKLAR EFTERSOM MAN                      
019720* ---INTE HAR NÅGRA VALBARA FÄLT. DÄRFÖR FLYTTAS                          
019730* ---MID-AREAN TILL MOD-AREAN DIREKT                                      
019800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019900     MOVE '001'             TO MSGI-KDCALL                                
020000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020200     MOVE '5212'            TO MSGI-IDTRANS                               
020300                                                                          
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020601                                                                          
020610     IF GODK-MID                                                          
020620        MOVE MSGI-SPAR-AREA      TO SPAR-AREA                             
020630        MOVE MID-FROM-DAREGDAT   TO MOD-FROM-DAREGDAT                     
020631        MOVE MID-TOM-DAREGDAT    TO MOD-TOM-DAREGDAT                      
020660        MOVE MID-IDPGM           TO MOD-IDPGM                             
020692     END-IF                                                               
020700                                                                          
020701* ---ANVÄNDS FÖR ATT SKICKA TILLBAKA MID-VÄRDEN                           
020702* ---TILL 5211-BILDEN                                                     
020703     IF NOT GODK-MID            OR                                        
020704       (GODK-MID AND                                                      
020705        SPAR-BILD NOT = '5212')                                           
020707       MOVE NEJ TO NYCKLAR-SW                                             
020708     ELSE                                                                 
020709       IF NOT EGEN-MID                                                    
020710          MOVE MID-FROM-DAREGDAT   TO SPAR-FROM-DAREGDAT                  
020711          MOVE MID-TOM-DAREGDAT    TO SPAR-TOM-DAREGDAT                   
020715          MOVE MID-IDPGM           TO SPAR-IDPGM                          
020723       END-IF                                                             
020724     END-IF                                                               
020725     IF MSGI-IDFTG = WC-IDFTG-PV                                          
020726        MOVE MSGI-IDFTG            TO W-IDFTG                             
020727     ELSE                                                                 
020728        MOVE NEJ                   TO NYCKLAR-SW                          
020729     END-IF                                                               
020730     IF MSGI-IDLAND-SPR = 'SE'                                            
020731        MOVE 'S' TO MED-IDSKYLT                                           
020732        MOVE 'S' TO W-IDSKYLT                                             
020740     ELSE                                                                 
020751        IF MSGI-IDLAND-SPR = 'GB'                                         
020754            MOVE 'GB' TO MED-IDSKYLT                                      
020755            MOVE 'GB' TO W-IDSKYLT                                        
020756        ELSE                                                              
020757            MOVE 'US' TO MED-IDSKYLT                                      
020758            MOVE 'US' TO W-IDSKYLT                                        
020759        END-IF                                                            
020760     END-IF                                                               
020770     MOVE NEJ TO BYT-SW                                                   
020810     MOVE JA TO ALLT-SW                                                   
021200                                                                          
021300     IF NYCKLAR-FEL                                                       
021400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
021500       CALL WMEDKONV USING MED-WMEDAREA                                   
021600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
021800       PERFORM MFS-RENSA-FAELT-UT                                         
021900     END-IF                                                               
022000     .                                                                    
022200     EJECT                                                                
022300 C-BYT-BILD SECTION.                                                      
022310     SKIP2                                                                
022320     MOVE JA TO BYT-SW                                                    
022330     MOVE NEJ TO ALLT-SW                                                  
022331                                                                          
022332* ---SKICKAR VÄRDE TILL 5211-MID FÖR ATT SEDAN                            
022333* ---STARTA UPP 5211-BILDEN                                               
022392     MOVE LOW-VALUE            TO 5211-MID-W5I21101                       
022394     MOVE SPAR-FROM-DAREGDAT   TO 5211-MID-FROM-DAREGDAT-IN               
022395     MOVE SPAR-TOM-DAREGDAT    TO 5211-MID-TOM-DAREGDAT-IN                
022396     IF SPAR-IDPGM = SPACE                                                
022397       MOVE '++++++++'         TO 5211-MID-IDPGM-IN                       
022398     ELSE                                                                 
022417       MOVE SPAR-IDPGM         TO 5211-MID-IDPGM-IN                       
022418     END-IF                                                               
022420                                                                          
022426     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21201 + 17                       
022427     PERFORM IMS-INSERT-ALT-MSG-5211                                      
022459     .                                                                    
022460     EJECT                                                                
022470 F-LAES-VISA-INFO SECTION.                                                
023510                                                                          
023514     MOVE SPAR-DAREGDAT        TO W-DAREGDAT                              
023518     MOVE SPAR-IDPGM-TAB       TO W-IDPGM                                 
023519     MOVE SPAR-IDSEKVNR        TO W-IDSEKVNR                              
023520     MOVE SPAR-TIKLOCK         TO W-TIKLOCK                               
023521     PERFORM IMS-GET-WDR9                                                 
023530     IF SEGMENT-FINNS                                                     
023540        MOVE FIL-DAREGDAT      TO MOD-DAREGDAT                            
023541                                  MOD-DAREGDAT-FEL                        
023550        MOVE FIL-TIKLOCK       TO MOD-TIKLOCK                             
023552        MOVE FIL-IDPGM         TO MOD-IDPGM-NY                            
023553        MOVE FIL-IDUSER        TO MOD-IDUSER                              
023555        MOVE EKH-KDEKHHT       TO MOD-KDEKHHT                             
023559        MOVE EKH-KDEKSHT       TO MOD-KDEKSHT                             
023560        MOVE EKH-KDEKNIVA      TO MOD-KDEKNIVA                            
023562        MOVE EKH-IDDISTR       TO MOD-IDDISTR                             
023563        MOVE EKH-IDKUNDNR      TO MOD-IDKUNDNR                            
023564        MOVE EKH-IDVERGL       TO MOD-IDVERGL                             
023565        MOVE EKH-DAVERDAT      TO MOD-DAVERDAT                            
023566        MOVE EKH-IDARTNR       TO MOD-IDARTNR                             
023567        MOVE EKH-IDDC-SEND     TO MOD-IDDC-SEND                           
023568        MOVE EKH-IDDC-REC      TO MOD-IDDC-REC                            
023569        MOVE EKH-KDVALISO      TO MOD-KDVALISO                            
023570        MOVE EKH-PRKURS        TO MOD-PRKURS                              
023571        MOVE EKH-FLLSBOK       TO MOD-FLLSBOK                             
023572        MOVE EKH-PRARTNTO      TO MOD-PRARTNTO                            
023573        MOVE EKH-PRARTSTD      TO MOD-PRARTSTD                            
023574        MOVE EKH-PRARTSJK      TO MOD-PRARTSJK                            
023576        MOVE EKH-PRINK         TO MOD-PRINK                               
023578        MOVE EKH-PRDIRLON      TO MOD-PRDIRLON                            
023580        MOVE EKH-PRDMTRL       TO MOD-PRDMTRL                             
023581        MOVE EKH-IDTRANS       TO MOD-IDTRANS-NY                          
023582        MOVE EKH-PROVRPAL      TO MOD-PROVRPAL                            
023583        MOVE EKH-PRHEMTAG      TO MOD-PRHEMTAG                            
023584        MOVE EKH-KVANTAL       TO MOD-KVANTAL                             
023585        MOVE EKH-SUBEL         TO MOD-SUBEL                               
023586        MOVE EKH-BEFELSAP      TO MOD-BEFEL                               
023587*---FYLLER I VÄRDE I W-KDEKHHT FÖR ATT KUNNA LÄSA WDH5-BASEN              
023588*---OCH VÄRDE I W-IDARTNR FÖR ATT KUNNA LÄSA WDD3-BASEN                   
023589        MOVE EKH-KDEKHHT       TO W-KDEKHHT                               
023590        MOVE EKH-IDARTNR       TO W-IDARTNR-WDD3                          
023591     ELSE                                                                 
023592        MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                
023593        CALL WMEDKONV USING MED-WMEDAREA                                  
023594        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
023595        PERFORM MFS-RENSA-FAELT-UT                                        
023596     END-IF                                                               
023597                                                                          
023598     PERFORM IMS-GET-WDD3                                                 
023599     IF SEGMENT-FINNS                                                     
023600        MOVE TEXT-BEART TO MOD-BEART                                      
023601     ELSE                                                                 
023602        MOVE 'UNKNOWN'  TO MOD-BEART                                      
023603     END-IF                                                               
023604                                                                          
023605     PERFORM IMS-GET-HHT                                                  
023606     IF SEGMENT-FINNS                                                     
023607        MOVE HHT-BEEKHHT  TO MOD-BEEKHHT                                  
023613     END-IF                                                               
023614     MOVE '002'      TO MSGI-KDCALL                                       
023615     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
023616     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023620     .                                                                    
024815     EJECT                                                                
024820 MFS-RENSA-FAELT-UT SECTION.                                              
024900                                                                          
025200     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT                                  
025220                             MOD-KDEKSHT                                  
025230                             MOD-KDEKNIVA                                 
025240                             MOD-BEEKHHT                                  
025250                             MOD-IDDISTR                                  
025260                             MOD-IDKUNDNR                                 
025270                             MOD-IDVERGL                                  
025280                             MOD-DAVERDAT                                 
025290                             MOD-IDARTNR                                  
025291                             MOD-BEART                                    
025292                             MOD-IDDC-SEND                                
025293                             MOD-IDDC-REC                                 
025294                             MOD-KDVALISO                                 
025295                             MOD-PRKURS                                   
025296                             MOD-FLLSBOK                                  
025297                             MOD-PRARTNTO                                 
025298                             MOD-PRARTSTD                                 
025299                             MOD-PRARTSJK                                 
025300                             MOD-PRINK                                    
025301                             MOD-DAREGDAT                                 
025302                             MOD-DAREGDAT-FEL                             
025303                             MOD-TIKLOCK                                  
025304                             MOD-PRDIRLON                                 
025305                             MOD-IDPGM-NY                                 
025306                             MOD-PRDMTRL                                  
025307                             MOD-IDTRANS-NY                               
025308                             MOD-PROVRPAL                                 
025309                             MOD-PRHEMTAG                                 
025310                             MOD-IDUSER                                   
025311                             MOD-KVANTAL                                  
025320                             MOD-BEFEL                                    
025400     .                                                                    
025600     SKIP3                                                                
029400* --- IMS SEKTIONER ---                                                   
029500                                                                          
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
030600     IF ENGLISH-TEXT                                                      
030700       MOVE 'N' TO MFS-KDHUVOMR                                           
030800     END-IF                                                               
030900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031000     MOVE SPACE TO GODK-STATUSKODER                                       
031100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031300     PERFORM IMS-STATUSKONTROLL                                           
031400     .                                                                    
031501     EJECT                                                                
031502 IMS-INSERT-ALT-MSG-5211 SECTION.                                         
031503                                                                          
031504     MOVE SPACE TO GODK-STATUSKODER                                       
031505     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW-5211               
031506     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
031507     PERFORM IMS-STATUSKONTROLL                                           
031508     .                                                                    
031509     EJECT                                                                
031518 IMS-GET-WDR9 SECTION.                                                    
031519                                                                          
031520     STRING 'WDR901  (WDR901KY =' W-WDR901KY-X ')'                        
031527          DELIMITED BY SIZE INTO SSA1                                     
031528     MOVE '  GE' TO GODK-STATUSKODER                                      
031529     CALL CBLTDLI USING GU WDR9-PCB DLI-IO-WDR901 SSA1                    
031530     MOVE WDR9-STATUS-CODE TO STATUS-WS                                   
031531     PERFORM IMS-STATUSKONTROLL                                           
031532     .                                                                    
031533     EJECT                                                                
031534 IMS-GET-WDD3 SECTION.                                                    
031535                                                                          
031536     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-WDD3-X ')'                    
031537          DELIMITED BY SIZE INTO SSA1                                     
031538     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
031539          DELIMITED BY SIZE INTO SSA2                                     
031540     MOVE '  GE' TO GODK-STATUSKODER                                      
031541     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1 SSA2               
031542     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
031543     PERFORM IMS-STATUSKONTROLL                                           
031550     .                                                                    
031600     EJECT                                                                
031610 IMS-GET-HHT SECTION.                                                     
031620                                                                          
031630     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031640          DELIMITED BY SIZE INTO SSA1                                     
031650     MOVE '  GE' TO GODK-STATUSKODER                                      
031660     CALL CBLTDLI USING GHU WDH5-PCB DLI-IO-WDH501 SSA1                   
031670     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031680     PERFORM IMS-STATUSKONTROLL                                           
031690     .                                                                    
031737                                                                          
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

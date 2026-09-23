001000*COMPOPT STDSUB=YES                                                       
001100 ID DIVISION.                                                             
001200     SKIP2                                                                
001300 PROGRAM-ID.     W611PMRK.                                                
001400*AUTHOR.         ROSEMARIE CLAESSON.                                      
001500*DATE-WRITTEN.   92/03/20.                                                
001600                                                                          
001700*    REMARKS.                                                             
001800*                                                                         
001900*    FUNKTION:                                                            
002000*        PROGRAMMET ÄR ETT SUBPROGRAM SOM PRIOMÄRKER                      
002100*        KOLLIN OCH JUSTERAR PRIOANTAL, TOTALT OCH                        
002200*        FÖRDELAT PÅ INLEVERANSREGISTRET.                                 
002300*                                                                         
002401*        PROGRAMMET UPPDATERAR W6INLA (W6D1)                              
002410*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
002500*                                                                         
002600*    ABENDKODER:                                                          
002700*        U0016 -  . . . .                                                 
002800*        U1000 -  . . . .                                                 
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003900     EJECT                                                                
003910                                                                          
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004110                                                                          
004200 FILE SECTION.                                                            
004400     EJECT                                                                
004410                                                                          
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W611PMRK'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000     SKIP2                                                                
005010                                                                          
005020 77  SLUT-SW                     PIC X       VALUE 'N'.                   
005030     88 SLUT-JA                              VALUE 'J'.                   
005031     88 SLUT-NEJ                             VALUE 'N'.                   
005040                                                                          
005041 77  TRAEFF-SW                   PIC X       VALUE 'N'.                   
005042     88 TRAEFF-JA                            VALUE 'J'.                   
005043     88 TRAEFF-NEJ                           VALUE 'N'.                   
005044                                                                          
005045 77  DIVKOLL-SW                  PIC X       VALUE 'N'.                   
005046     88 DIVKOLL-JA                           VALUE 'J'.                   
005047     88 DIVKOLL-NEJ                          VALUE 'N'.                   
005052                                                                          
005053 77  DIVKOLL-BORT-SW             PIC X       VALUE 'N'.                   
005054     88 DIVKOLL-BORT-JA                      VALUE 'J'.                   
005055     88 DIVKOLL-BORT-NEJ                     VALUE 'N'.                   
005056                                                                          
005057 77  PRIOBORT-SW                 PIC X       VALUE 'N'.                   
005058     88 PRIOBORT-JA                          VALUE 'J'.                   
005059     88 PRIOBORT-NEJ                         VALUE 'N'.                   
005060                                                                          
005070                                                                          
005080 01  WORK-SPAR-AREA.                                                      
005090     03  SPAR-B-KVAVIS-PRIO      PIC S9(7)    COMP-3.                     
005092     03  SPAR-B-KDINLPRIO        PIC S9(3)    COMP-3.                     
005093     03  SPAR-C-IDLOPNRM         PIC S9(9)    COMP-3.                     
005094     03  SPAR-C-KVAVIS-PRIO      PIC S9(7)    COMP-3.                     
005095     03  SPAR-C-KDINLPRIO        PIC S9(3)    COMP-3.                     
005097     03  SPAR-IDLOPNRM           PIC S9(9)    COMP-3.                     
005099     03  W-TOT-ANT               PIC S9(7)    COMP-3.                     
005100     03  W-TOT-ANT-UTAN-PRIO     PIC S9(7)    COMP-3.                     
005101     03  W-TOT-ANT-MED-PRIO      PIC S9(7)    COMP-3.                     
005102     03  W-PRIO-ANT-BORT         PIC S9(7)    COMP-3.                     
005103     03  W-PRIO-ANT              PIC S9(7)    COMP-3.                     
005104                                                                          
005110 01  FELTEXT.                                                             
005200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005800     EJECT                                                                
005810                                                                          
006500 01  DYNAMISKA-SUBPROGRAM.                                                
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     EJECT                                                                
007510                                                                          
007600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007700     SKIP3                                                                
007800 01  NYCKLAR-TILL-DLI.                                                    
007811                                                                          
007812*C-INDEX                                                                  
007818     03  W-W6D1CSEQ-X.                                                    
007820         05  WC-IDLEVNR          PIC  X(5)    VALUE SPACE.                
007830         05  WC-IDOKOLLI         PIC 9(9).                                
007832                                                                          
007833*C-INDEX SÖKNYCKLAR                                                       
007834     03  WCS-IDLEVNR-X.                                                   
007835         05  WCS-IDLEVNR          PIC  X(5)   VALUE SPACE.                
007836                                                                          
007837     03  WCS-IDOKOLLI-X.                                                  
007838         05  WCS-IDOKOLLI          PIC 9(9).                              
007839                                                                          
007841*C-INDEX                                                                  
007845     03  W1-IDLOPNRM-X.                                                   
007847         05  W1-IDLOPNRM         PIC S9(9)    COMP-3.                     
007848                                                                          
007849     03  W-IDRADNR-X.                                                     
007851         05  W-IDRADNR           PIC S9(5)    COMP-3.                     
007852                                                                          
007870     SKIP2                                                                
007900     EJECT                                                                
008010                                                                          
008020     SKIP2                                                                
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FINNS                       VALUE '  '.                  
008400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008700     88  IMS-EJ-OK                           VALUE 'XD'.                  
008710     EJECT                                                                
008720                                                                          
008800     SKIP2                                                                
008900 01  GODK-STATUSKODER.                                                    
009000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009001     EJECT                                                                
009010                                                                          
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009301 01  SSA3                        PIC X(64).                               
009400     EJECT                                                                
009410                                                                          
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009800                                                                          
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010100     SKIP3                                                                
010200 01  DLI-IO-AREA-1.                                                       
010300     03  IO-AREA-1               PIC X(150)  VALUE SPACE.                 
010301                                                                          
010306     03  W6INLA11 REDEFINES IO-AREA-1.                                    
010307*        05  -COPY W6D111                                                 
010308     SKIP3                                                                
010309                                                                          
010310     03  W6INLA21 REDEFINES IO-AREA-1.                                    
010311*        05  -COPY W6D121                                                 
010312     SKIP3                                                                
010313     EJECT                                                                
010720                                                                          
010797                                                                          
010800 LINKAGE SECTION.                                                         
010900                                                                          
010901*01  -COPY W611PMRK                                                       
010903     EJECT                                                                
010907                                                                          
010908*01  -COPY W0008  -PRE INLB-PMRK-                                         
010909     05  FILLER                  PIC X.                                   
010910     EJECT                                                                
010911                                                                          
010912*01  -COPY W0008  -PRE INLC-PMRK-                                         
010913     05  FILLER                  PIC X.                                   
010914     EJECT                                                                
010915                                                                          
010931                                                                          
010936 PROCEDURE DIVISION  USING PMRK-W611PMRK                                  
010937                           INLB-PMRK-PCB                                  
010938                           INLC-PMRK-PCB.                                 
010939     EJECT                                                                
012000                                                                          
012001                                                                          
012010     PERFORM A-INIT                                                       
012011     IF PMRK-IDLOPNRM > ZERO AND                                          
012012        PMRK-IDRADNR  > ZERO                                              
012013*-------PRIOMÄRKNING/UPPHÄVNING VIA IDLOPNR OCH RADNR                     
012014        PERFORM D-BEARB-VIA-PARTI-RADNR                                   
012015                                                                          
012020     ELSE                                                                 
012030*-------PRIOMÄRKNING/UPPHÄVNING VIA IDLEVNR OCH KOLLINR                   
012100        PERFORM IMS-GU-INLC-D111                                          
012102                                                                          
012120        IF SEGMENT-FINNS                                                  
012140           MOVE ART-IDLOPNRM     TO SPAR-C-IDLOPNRM                       
012141           MOVE SPAR-C-IDLOPNRM TO W1-IDLOPNRM                            
012150           MOVE ART-KDINLPRIO    TO SPAR-C-KDINLPRIO                      
012160           MOVE ART-KVAVIS-PRIO  TO SPAR-B-KVAVIS-PRIO                    
012310           PERFORM B-BEARB                                                
012311                                                                          
012317*----------OM DET ÄR ETT DIVERSEKOLLI KANSKE MAN INTE FICK                
012318*----------TRÄFF MAP STATUS I DET PARTIET  SOM LÄSTES                     
012320           IF DIVKOLL-JA OR TRAEFF-NEJ                                    
012321              PERFORM IMS-GN-INLC-D111                                    
012322                                                                          
012330              PERFORM UNTIL SEGMENT-SAKNAS OR SLUT-JA                     
012380                 IF ART-IDLOPNRM = SPAR-C-IDLOPNRM                        
012390                    CONTINUE                                              
012400*-------------------EFTERSOM MAN LÄSER MED C-IX                           
012500*-------------------MÅSTE MAN LÄSA VIDARE TILLS NÄSTA PARTI               
012600                 ELSE                                                     
012700                    MOVE ART-IDLOPNRM      TO SPAR-C-IDLOPNRM             
012710                    MOVE SPAR-C-IDLOPNRM   TO W1-IDLOPNRM                 
012800                    MOVE ART-KDINLPRIO     TO SPAR-C-KDINLPRIO            
012810                    MOVE ART-KVAVIS-PRIO  TO SPAR-B-KVAVIS-PRIO           
012900                    PERFORM B-BEARB                                       
012910                 END-IF                                                   
012922                 PERFORM IMS-GN-INLC-D111                                 
012923              END-PERFORM                                                 
012924                                                                          
012928              IF SLUT-NEJ AND                                             
012929                 PRIOBORT-JA                                              
012930*--------------- BORTTAG AV PRIO PÅ DIVKOLLI                              
012931                 PERFORM C-PRIO-DIVKOLLI-BORT                             
012932              END-IF                                                      
012933                                                                          
012934           END-IF                                                         
012940        END-IF                                                            
012950     END-IF                                                               
013300                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013710******************************************************************        
013800 A-INIT SECTION.                                                          
014430                                                                          
014501     MOVE PMRK-IDLOPNRM        TO W1-IDLOPNRM                             
014502     MOVE PMRK-IDRADNR         TO W-IDRADNR                               
014503     MOVE PMRK-IDLEVNR         TO WC-IDLEVNR                              
014504                                  WCS-IDLEVNR                             
014510     MOVE PMRK-IDOKOLLI        TO WC-IDOKOLLI                             
014511                                  WCS-IDOKOLLI                            
014520     MOVE SPACE                TO PMRK-KDSVAR                             
014521     MOVE NEJ                  TO SLUT-SW                                 
014522     MOVE NEJ                  TO DIVKOLL-SW                              
014523     MOVE NEJ                  TO DIVKOLL-BORT-SW                         
014530     MOVE NEJ                  TO PRIOBORT-SW                             
014531     MOVE NEJ                  TO TRAEFF-SW                               
014532     MOVE +0000000             TO SPAR-B-KVAVIS-PRIO                      
014533     MOVE +0000000             TO SPAR-C-KVAVIS-PRIO                      
014534     MOVE +000                 TO SPAR-B-KDINLPRIO                        
014535     MOVE +000                 TO SPAR-C-KDINLPRIO                        
014536     MOVE +000000000           TO SPAR-C-IDLOPNRM                         
014537     MOVE +0000000             TO W-TOT-ANT                               
014538     MOVE +0000000             TO W-TOT-ANT-UTAN-PRIO                     
014539     MOVE +0000000             TO W-TOT-ANT-MED-PRIO                      
014540     MOVE +0000000             TO W-PRIO-ANT                              
014541     MOVE +0000000             TO W-PRIO-ANT-BORT                         
015000     .                                                                    
015100     EJECT                                                                
015110******************************************************************        
015120*  BEARBETA KOLLI-RAD SOM SKA PRIOMÄRKAS                         *        
015121*  OCH DE ANDRA SOM SKA UPPDATERAS                               *        
015130******************************************************************        
015200 B-BEARB SECTION.                                                         
015311                                                                          
015319     MOVE NEJ                  TO DIVKOLL-BORT-SW                         
015320     MOVE +0000000             TO W-PRIO-ANT-BORT                         
015330                                                                          
015331     PERFORM IMS-GHNP-INLC-D121-SOEK                                      
015332     IF SEGMENT-FINNS                                                     
015333                                                                          
015334        PERFORM UNTIL SEGMENT-SAKNAS                                      
015351           IF RAD-KDINLSTA = 'SAK' OR 'FPK' OR SPACE                      
015352              MOVE JA             TO TRAEFF-SW                            
015353              IF RAD-FLDIVKLI = NEJ                                       
015354*----------------ORD.    KOLLI - ENDAST 1 RAD BEARBETAS                   
015355                 IF RAD-FLPRIO = NEJ                                      
015365*-------------------PRIOMÄRKA    KOLLI                                    
015370                    PERFORM BA-PRIO-MAERK                                 
015371                    PERFORM S01-BER-PARTI-PRIO-ANT                        
015372                    PERFORM S02-KONTR-PRIO-UPPFYLLT                       
015374                 ELSE                                                     
015376*-------------------BORTTAG    AV PRIOMÄRKNING PÅ KOLLI                   
015377                    PERFORM BB-BORT-PRIO-MAERK                            
015378                    PERFORM S01-BER-PARTI-PRIO-ANT                        
015379                    PERFORM BC-KONTR-PRIO-UPPFYLLT                        
015380                 END-IF                                                   
015383              ELSE                                                        
015384*----------------DIV-KOLLI    - FLERA RADER KAN BEARBETAS                 
015385                 MOVE JA       TO DIVKOLL-SW                              
015386                 IF RAD-FLPRIO = NEJ                                      
015388*-------------------PRIOMÄRKA    DIV-KOLLI                                
015390                    PERFORM BA-PRIO-MAERK                                 
015391                    PERFORM S01-BER-PARTI-PRIO-ANT                        
015392                    PERFORM S02-KONTR-PRIO-UPPFYLLT                       
015394                 ELSE                                                     
015396*-------------------BORTTAG    AV PRIOMÄRKNING PÅ DIV-KOLLI               
015397*-------------------SUMMERA ALLA DIVKOLLIS  ANTAL PER PARTI               
015398*-------------------DET KAN FINNAS FLERA / PARTI                          
015399                    MOVE JA               TO DIVKOLL-BORT-SW              
015400                    ADD  RAD-KVINLART     TO W-PRIO-ANT-BORT              
015408                 END-IF                                                   
015411              END-IF                                                      
015412           END-IF                                                         
015416*----------OM DET ÄR ETT DIV-KOLLI KAN MAN FÅ FLER TRÄFFAR                
015417           PERFORM IMS-GHNP-INLC-D121-SOEK                                
015420        END-PERFORM                                                       
015421                                                                          
015422*-------OM DET ÄR BORTTAG AV DIV-KOLLI SKER KONTROLLER FÖRST              
015423*-------EFTER ATT HELA PARTIET GÅTTS IGENOM                               
015430        IF DIVKOLL-BORT-JA                                                
015431           PERFORM S01-BER-PARTI-PRIO-ANT                                 
015432           PERFORM BC-KONTR-PRIO-UPPFYLLT                                 
015440        END-IF                                                            
015500     END-IF                                                               
015800     .                                                                    
016200     EJECT                                                                
016300******************************************************************        
016310*  PRIOMÄRKA KOLLI                                                        
016400*  UPPDATERA PRIORITERING PÅ RADEN                               *        
016403******************************************************************        
016404 BA-PRIO-MAERK SECTION.                                                   
016405                                                                          
016415     MOVE RAD-KVINLART             TO W-PRIO-ANT                          
016419     MOVE JA                       TO RAD-FLPRIO                          
016420                                                                          
016421     IF SPAR-C-KDINLPRIO NOT = RAD-KDINLPRIO                              
016422        MOVE SPAR-C-KDINLPRIO      TO RAD-KDINLPRIO                       
016423     END-IF                                                               
016424                                                                          
016429     PERFORM IMS-REPL-INLC-D121                                           
016431     .                                                                    
016432     EJECT                                                                
016601******************************************************************        
016602*  BORTTAG AV PRIOMÄRKNING PÅ KOLLI                                       
016603*  BORTTAG AV PRIORITERING PÅ RAD                                *        
016604******************************************************************        
016605 BB-BORT-PRIO-MAERK SECTION.                                              
016606                                                                          
016626     MOVE NEJ                      TO RAD-FLPRIO                          
016628     MOVE RAD-KVINLART             TO W-PRIO-ANT-BORT                     
016629     MOVE RAD-IDRADNR              TO W-IDRADNR                           
016630                                                                          
016631     IF SPAR-B-KVAVIS-PRIO > ZERO                                         
016632        MOVE +031                  TO RAD-KDINLPRIO                       
016633     ELSE                                                                 
016634        MOVE +039                  TO RAD-KDINLPRIO                       
016635     END-IF                                                               
016636                                                                          
016640     PERFORM IMS-REPL-INLC-D121                                           
016642     .                                                                    
016643     EJECT                                                                
016644******************************************************************        
016645*  BORTTAG AV PRIOMÄRKNING PÅ KOLLI                                       
016646*  KONTROLL OM PRIORITERING UPPFYLLS                                      
016647*  UPPDATERA ALLA RADER DÄR PRIO ÄR NEJ                                   
016648*  ---ORDINARIE KOLLI                                                     
016649*     W-TOT-ANT-MED-PRIO INNEHÅLLER                                       
016650*     DET TOTALA ANTALET UPPRÄKNAT MED DEN VARS PRIO TOGS BORT            
016651*  ---DIVERSE KOLLI                                                       
016652*     EFTERSOM UPPDATERING SKER FÖRST NÄR ALLA PARTI RÄKNATS              
016653*     IGENOM INNEHÅLLER W-TOT-ANT ÄVEN DET ANTAL SOM SKA                  
016654*     RÄKNAS AV VID KONTROLL AV UPPFYLLELSE                               
016655*     DÄRFÖR DRAS DET ANTALET BORT                                        
016656*     - DENNA KONTROLL GÖRS TOTALT PER PARTI                              
016657*       OCH -ALLA- MÅSTE UPPFYLLA VILLKOREN ANNARS BLIR DET               
016658*       INGET BORTTAG AV PRIOMÄRKNING                                     
016659******************************************************************        
016660 BC-KONTR-PRIO-UPPFYLLT SECTION.                                          
016661                                                                          
016670     IF DIVKOLL-BORT-JA                                                   
016671*-------DIV. KOLLI                                                        
016677        COMPUTE W-TOT-ANT-UTAN-PRIO = W-TOT-ANT - W-PRIO-ANT-BORT         
016678                                                                          
016685        IF W-TOT-ANT-UTAN-PRIO < SPAR-B-KVAVIS-PRIO                       
016686*----------UPPHÄVNING AV PRIO EJ GODKÄND ---> AVSLUTA DIREKT              
016687           MOVE NEJ               TO PRIOBORT-SW                          
016688           MOVE JA                TO SLUT-SW                              
016689           MOVE '1'               TO PMRK-KDSVAR                          
016690        ELSE                                                              
016691           MOVE JA                TO PRIOBORT-SW                          
016692        END-IF                                                            
016693                                                                          
016697     ELSE                                                                 
016698*-------ORD. KOLLI                                                        
016699        COMPUTE W-TOT-ANT-MED-PRIO =                                      
016700                W-TOT-ANT + W-PRIO-ANT-BORT                               
016701                                                                          
016702        IF  W-TOT-ANT < SPAR-B-KVAVIS-PRIO AND                            
016703           (W-TOT-ANT-MED-PRIO = SPAR-B-KVAVIS-PRIO OR                    
016704            W-TOT-ANT-MED-PRIO > SPAR-B-KVAVIS-PRIO)                      
016705                                                                          
016706           MOVE SPAR-C-IDLOPNRM    TO W1-IDLOPNRM                         
016708                                                                          
016709           PERFORM IMS-GU-INLB-D111                                       
016710           IF SEGMENT-FINNS                                               
016711              MOVE ART-KDINLPRIO TO SPAR-B-KDINLPRIO                      
016712              PERFORM IMS-GHNP-INLB-D121                                  
016713                                                                          
016714              PERFORM UNTIL SEGMENT-SAKNAS                                
016718                 IF (RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE)              
016720                    MOVE SPAR-B-KDINLPRIO TO RAD-KDINLPRIO                
016726                    PERFORM IMS-REPL-INLB-D121                            
016727                 END-IF                                                   
016728                 PERFORM IMS-GHNP-INLB-D121                               
016729              END-PERFORM                                                 
016730                                                                          
016731           END-IF                                                         
016732        ELSE                                                              
016733           IF W-TOT-ANT < SPAR-B-KVAVIS-PRIO                              
016735             PERFORM IMS-GHNP-FIRST-INLB-D121                             
016736             MOVE SPAR-C-KDINLPRIO  TO RAD-KDINLPRIO                      
016737             PERFORM IMS-REPL-INLB-D121                                   
016738           END-IF                                                         
016739        END-IF                                                            
016740     END-IF                                                               
016741     .                                                                    
016742     EJECT                                                                
016743******************************************************************        
016744*--DIV-KOLLI------- BORTTAG AV PRIOMÄRKNING PÅ KOLLI                      
016745*  RADER VARS PRIO SKA TAGAS BORT UPPDATERAS                              
016746*  OBS DETTA GÄLLER PER PARTI                                             
016747******************************************************************        
016748 C-PRIO-DIVKOLLI-BORT SECTION.                                            
016749                                                                          
016750     MOVE +000000000          TO SPAR-C-IDLOPNRM                          
016751     MOVE +000                TO SPAR-C-KDINLPRIO                         
016752     MOVE +0000000            TO SPAR-C-KVAVIS-PRIO                       
016753     PERFORM IMS-GU-INLC-D111                                             
016754                                                                          
016756     IF SEGMENT-FINNS                                                     
016761        PERFORM UNTIL SEGMENT-SAKNAS                                      
016770           IF ART-IDLOPNRM = SPAR-C-IDLOPNRM                              
016771              CONTINUE                                                    
016772*-------------EFTERSOM MAN LÄSER MED C-IX                                 
016773*-------------MÅSTE MAN LÄSA VIDARE TILLS NÄSTA PARTI                     
016774           ELSE                                                           
016775              MOVE ART-IDLOPNRM      TO SPAR-C-IDLOPNRM                   
016776              MOVE ART-KDINLPRIO     TO SPAR-C-KDINLPRIO                  
016777              MOVE ART-KVAVIS-PRIO   TO SPAR-C-KVAVIS-PRIO                
016779                                                                          
016780              PERFORM IMS-GHNP-INLC-D121-SOEK                             
016781              IF SEGMENT-FINNS                                            
016782                                                                          
016783                 PERFORM UNTIL SEGMENT-SAKNAS                             
016784                    PERFORM CA-BORT-PRIO-MAERK                            
016785*-------------------LÄS VIDARE I PARTIET, FLER DIVKOLLI KA FINNAS         
016786                    PERFORM IMS-GHNP-INLC-D121-SOEK                       
016787                 END-PERFORM                                              
016788                                                                          
016789              END-IF                                                      
016790           END-IF                                                         
016791           PERFORM IMS-GN-INLC-D111                                       
016792        END-PERFORM                                                       
016793                                                                          
016794     END-IF                                                               
016795     .                                                                    
016796     EJECT                                                                
016799******************************************************************        
016800*  BORTTAG AV PRIOMÄRKNING PÅ RAD                                         
016801******************************************************************        
016802 CA-BORT-PRIO-MAERK SECTION.                                              
016803                                                                          
016808     MOVE NEJ                      TO RAD-FLPRIO                          
016809                                                                          
016810     IF SPAR-C-KVAVIS-PRIO > ZERO                                         
016811        MOVE +031                  TO RAD-KDINLPRIO                       
016812     ELSE                                                                 
016813        MOVE SPAR-C-KDINLPRIO      TO RAD-KDINLPRIO                       
016814     END-IF                                                               
016815                                                                          
016821     PERFORM IMS-REPL-INLC-D121                                           
016823     .                                                                    
016824     EJECT                                                                
016839******************************************************************        
016840*  PRIOMÄRKNING/UPPHÄVNING VIA PARTI OCH RADNR                            
016841******************************************************************        
016842 D-BEARB-VIA-PARTI-RADNR SECTION.                                         
016843                                                                          
016850     PERFORM IMS-GU-INLB-D111                                             
016851                                                                          
016852     IF SEGMENT-FINNS                                                     
016853        MOVE ART-KDINLPRIO      TO SPAR-B-KDINLPRIO                       
016854        MOVE ART-KVAVIS-PRIO    TO SPAR-B-KVAVIS-PRIO                     
016855        PERFORM IMS-GHU-INLB-D121                                         
016857                                                                          
016858        IF SEGMENT-FINNS                                                  
016859           IF RAD-KDINLSTA = 'AVV' OR 'ANT' OR 'KVA'                      
016860             IF RAD-KVINLART < ZERO                                       
016861**DVS ÖVERLEVERANS, MER SKA PRIOMÄRKAS                                    
016862               COMPUTE W-PRIO-ANT = RAD-KVINLART * -1                     
016869               PERFORM S01-BER-PARTI-PRIO-ANT                             
016870               PERFORM S02-KONTR-PRIO-UPPFYLLT                            
016871             ELSE                                                         
016872**DVS UNDERLEVERANS, PRIOMÄRKNING SKA TAS BORT                            
016873               MOVE RAD-KVINLART TO W-PRIO-ANT-BORT                       
016874               PERFORM S01-BER-PARTI-PRIO-ANT                             
016875               PERFORM DA-KONTR-PRIO-UPPFYLLT                             
016876             END-IF                                                       
016877           ELSE                                                           
016878             IF RAD-FLPRIO = NEJ                                          
016879                MOVE JA                 TO RAD-FLPRIO                     
016880                MOVE RAD-KVINLART       TO W-PRIO-ANT                     
016881                IF SPAR-B-KDINLPRIO NOT = RAD-KDINLPRIO                   
016882                   MOVE SPAR-B-KDINLPRIO TO RAD-KDINLPRIO                 
016883                END-IF                                                    
016884                PERFORM IMS-REPL-INLB-D121                                
016885                PERFORM S01-BER-PARTI-PRIO-ANT                            
016886                PERFORM S02-KONTR-PRIO-UPPFYLLT                           
016887             ELSE                                                         
016888                MOVE NEJ           TO RAD-FLPRIO                          
016889                MOVE RAD-KVINLART       TO W-PRIO-ANT-BORT                
016890                IF SPAR-B-KVAVIS-PRIO > ZERO                              
016891                   MOVE +031             TO RAD-KDINLPRIO                 
016892                ELSE                                                      
016893                   MOVE SPAR-B-KDINLPRIO TO RAD-KDINLPRIO                 
016894                END-IF                                                    
016895                PERFORM IMS-REPL-INLB-D121                                
016896                PERFORM S01-BER-PARTI-PRIO-ANT                            
016897                PERFORM DA-KONTR-PRIO-UPPFYLLT                            
016899             END-IF                                                       
016900           END-IF                                                         
016901        END-IF                                                            
016902     END-IF                                                               
016903     .                                                                    
016910     EJECT                                                                
016913******************************************************************        
016914*  BORTTAG AV PRIOMÄRKNING PÅ PARTI OCH RAD                               
016915*  KONTROLL OM PRIORITERING UPPFYLLS, I SÅ FALL                           
016916*  UPPDATERA ALLA RADER DÄR PRIO ÄR NEJ                                   
016917*                                                                         
016918*     W-TOT-ANT-MED-PRIO INNEHÅLLER                                       
016919*     DET TOTALA ANTALET UPPRÄKNAT MED DEN VARS PRIO TOGS BORT            
016920******************************************************************        
016921 DA-KONTR-PRIO-UPPFYLLT  SECTION.                                         
016922                                                                          
016923     COMPUTE W-TOT-ANT-MED-PRIO =                                         
016924             W-TOT-ANT + W-PRIO-ANT-BORT                                  
016925                                                                          
016926     IF  W-TOT-ANT < SPAR-B-KVAVIS-PRIO AND                               
016927        (W-TOT-ANT-MED-PRIO = SPAR-B-KVAVIS-PRIO OR                       
016928         W-TOT-ANT-MED-PRIO > SPAR-B-KVAVIS-PRIO)                         
016929**OM JUST DENNA BORTTAGNING AV PRIO INNEBÄR ATT KVAVIS-PRIO EJ            
016930** ÄR UPPFYLLT                                                            
016931                                                                          
016932        PERFORM IMS-GU-INLB-D111                                          
016933                                                                          
016934        IF SEGMENT-FINNS                                                  
016935           MOVE ART-KDINLPRIO TO SPAR-B-KDINLPRIO                         
016936           PERFORM IMS-GHNP-INLB-D121                                     
016937                                                                          
016938           PERFORM UNTIL SEGMENT-SAKNAS                                   
016939              IF (RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE)                 
016940                 MOVE SPAR-B-KDINLPRIO TO RAD-KDINLPRIO                   
016941                 PERFORM IMS-REPL-INLB-D121                               
016942              END-IF                                                      
016943              PERFORM IMS-GHNP-INLB-D121                                  
016944           END-PERFORM                                                    
016945                                                                          
016946        END-IF                                                            
016947     ELSE                                                                 
016948        IF W-TOT-ANT < SPAR-B-KVAVIS-PRIO                                 
016949          PERFORM IMS-GHNP-FIRST-INLB-D121                                
016950          IF RAD-KDINLSTA = SPACE OR 'FPK' OR 'SAK'                       
016951            MOVE SPAR-B-KDINLPRIO  TO RAD-KDINLPRIO                       
016952            PERFORM IMS-REPL-INLB-D121                                    
016953          END-IF                                                          
016954        END-IF                                                            
016955     END-IF                                                               
016956     .                                                                    
016957     EJECT                                                                
016958******************************************************************        
016959*  PRIOMÄRKA  ELLER                                                       
016960*  BORTTAG AV PRIOMÄRKNING                                                
016961*  LÄSA IGENOM PARTIETS RADER FÖR ATT RÄKNA UT HUR MYCKET        *        
016962*  SOM ÄR PRIORITERAT                                            *        
016963*  OBS OBS OBS                                                   *        
016964*  OM PRIOMÄRKNING GJORDES --> INKLUDERAS DESS ANTAL I W-TOT-ANT *        
016965*  OM PRIOMÄRKNING TOGS BORT -->                                 *        
016966*     INKLUDERAS DESS ANTAL -INTE- I W-TOT-ANT                   *        
016967*  SÅVIDA DET INTE ÄR ETT DIVERSE-KOLLI                          *        
016968*     DÅ ==> INKLUDERAS DESS ANTAL  -I-  W-TOT-ANT               *        
016969*     (BORTTAG GÖRS EJ FÖRRÄN ALLA PARTIER ÄR OK)                *        
016970******************************************************************        
016971 S01-BER-PARTI-PRIO-ANT SECTION.                                          
016972                                                                          
016973     MOVE ZERO                     TO W-TOT-ANT                           
016974                                      W-TOT-ANT-UTAN-PRIO                 
016975                                      W-TOT-ANT-MED-PRIO                  
016976     PERFORM IMS-GU-INLB-D111                                             
016977                                                                          
016978     IF SEGMENT-FINNS                                                     
016979        MOVE ART-KVAVIS-PRIO     TO SPAR-B-KVAVIS-PRIO                    
016980        PERFORM IMS-GNP-INLB-D121                                         
016981                                                                          
016982        PERFORM UNTIL SEGMENT-SAKNAS                                      
016983           IF RAD-FLPRIO = JA                                             
016984              ADD RAD-KVINLART   TO W-TOT-ANT                             
016985           END-IF                                                         
016986           PERFORM IMS-GNP-INLB-D121                                      
016987        END-PERFORM                                                       
016988                                                                          
016989     END-IF                                                               
016990     .                                                                    
016991     EJECT                                                                
016992******************************************************************        
016993*  PRIOMÄRKA KOLLI                                                        
016994*  KONTROLL OM PRIO UPPFYLLS                                     *        
016995*  UPPDATERA ALLA RADER DÄR PRIO ÄR NEJ OM ANTALET UPPFYLLS      *        
016996*  W-TOT-ANT-UTAN-PRIO INNEHÅLLER                                *        
016997*  DET TOTALA ANTALET NEDRÄKNAT MED DEN SOM PRIOMÄRKTES                   
016998******************************************************************        
016999 S02-KONTR-PRIO-UPPFYLLT SECTION.                                         
017000                                                                          
017001     PERFORM IMS-GU-INLB-D111                                             
017002                                                                          
017003     IF SEGMENT-FINNS                                                     
017004        MOVE ART-KDINLPRIO         TO SPAR-B-KDINLPRIO                    
017005                                                                          
017006        COMPUTE W-TOT-ANT-UTAN-PRIO =                                     
017007                W-TOT-ANT - W-PRIO-ANT                                    
017008                                                                          
017009        PERFORM IMS-GHNP-INLB-D121                                        
017010*-------GÅ IGENOM ALLA 21-SEGMENTEN                                       
017011                                                                          
017012       IF (W-TOT-ANT = SPAR-B-KVAVIS-PRIO OR                              
017013          W-TOT-ANT > SPAR-B-KVAVIS-PRIO) AND                             
017014          W-TOT-ANT-UTAN-PRIO < SPAR-B-KVAVIS-PRIO                        
017015**OM JUST DENNA PRIOMÄRKNINGEN INNEBÄR ATT KVAVIS-PRIO UPPFYLLS           
017016                                                                          
017017          PERFORM UNTIL SEGMENT-SAKNAS                                    
017018             IF SPAR-B-KVAVIS-PRIO > ZERO                                 
017019                IF RAD-FLPRIO = NEJ  AND                                  
017020               (RAD-KDINLSTA = 'FPK' OR SPACE OR 'SAK')                   
017021                   MOVE +031      TO RAD-KDINLPRIO                        
017022                END-IF                                                    
017023             ELSE                                                         
017024                IF RAD-FLPRIO = NEJ AND                                   
017025               (RAD-KDINLSTA = 'FPK' OR SPACE OR 'SAK')                   
017026                   MOVE +039      TO RAD-KDINLPRIO                        
017027                END-IF                                                    
017028             END-IF                                                       
017029             PERFORM IMS-REPL-INLB-D121                                   
017030             PERFORM IMS-GHNP-INLB-D121                                   
017031          END-PERFORM                                                     
017032       END-IF                                                             
017033     END-IF                                                               
017034     .                                                                    
017035     EJECT                                                                
017036******************************************************************        
017037*  IMS-SECTIONER                                                 *        
017038******************************************************************        
017039******************************************************************        
017040*  INLB-PCB                                                      *        
017041******************************************************************        
017042*                                                                         
017043*----------------------------------------------------------------*        
017044 IMS-GU-INLB-D111  SECTION.                                               
017045     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X ')'                       
017046             DELIMITED BY SIZE INTO SSA1                                  
017047     MOVE '  GE'                 TO GODK-STATUSKODER                      
017048     CALL CBLTDLI USING GU       INLB-PMRK-PCB                            
017049                                 DLI-IO-AREA-1                            
017050                                 SSA1                                     
017051     MOVE INLB-PMRK-STATUS-CODE  TO STATUS-WS                             
017052     PERFORM IMS-STATUSKONTROLL                                           
017053     .                                                                    
017054     EJECT                                                                
017055     SKIP3                                                                
017056*                                                                         
017057*----------------------------------------------------------------*        
017058 IMS-GHU-INLB-D121  SECTION.                                              
017059     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X ')'                       
017060             DELIMITED BY SIZE INTO SSA1                                  
017061     STRING 'W6INLA21(IDRADNR  =' W-IDRADNR-X ')'                         
017062             DELIMITED BY SIZE INTO SSA2                                  
017063     MOVE '  GE'                 TO GODK-STATUSKODER                      
017064     CALL CBLTDLI USING GHU      INLB-PMRK-PCB                            
017065                                 DLI-IO-AREA-1                            
017066                                 SSA1                                     
017067                                 SSA2                                     
017068     MOVE INLB-PMRK-STATUS-CODE  TO STATUS-WS                             
017069     PERFORM IMS-STATUSKONTROLL                                           
017070     .                                                                    
017071     EJECT                                                                
017072     SKIP3                                                                
017073*----------------------------------------------------------------*        
017074 IMS-GNP-INLB-D121 SECTION.                                               
017075     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X ')'                       
017076             DELIMITED BY SIZE INTO SSA1                                  
017077     MOVE 'W6INLA21 '            TO SSA2                                  
017078     MOVE '  GE'              TO GODK-STATUSKODER                         
017079     CALL CBLTDLI USING GNP      INLB-PMRK-PCB                            
017080                                 DLI-IO-AREA-1                            
017081                                 SSA1                                     
017082                                 SSA2                                     
017083     MOVE INLB-PMRK-STATUS-CODE  TO STATUS-WS                             
017084     PERFORM IMS-STATUSKONTROLL                                           
017085     .                                                                    
017086     EJECT                                                                
017087     SKIP3                                                                
017088*----------------------------------------------------------------*        
017089 IMS-GHNP-INLB-D121 SECTION.                                              
017090     STRING 'W6INLA11(W6D1BSEQ =' W1-IDLOPNRM-X ')'                       
017091             DELIMITED BY SIZE INTO SSA1                                  
017092     MOVE 'W6INLA21 '            TO SSA2                                  
017093     MOVE '  GE'              TO GODK-STATUSKODER                         
017094     CALL CBLTDLI USING GHNP     INLB-PMRK-PCB                            
017095                                 DLI-IO-AREA-1                            
017096                                 SSA1                                     
017097                                 SSA2                                     
017098     MOVE INLB-PMRK-STATUS-CODE  TO STATUS-WS                             
017099     PERFORM IMS-STATUSKONTROLL                                           
017100     .                                                                    
017101     EJECT                                                                
017102     SKIP3                                                                
017103*----------------------------------------------------------------*        
017104 IMS-GHNP-FIRST-INLB-D121 SECTION.                                        
017105     STRING 'W6INLA21*F(IDRADNR  =' W-IDRADNR-X ')'                       
017106             DELIMITED BY SIZE INTO SSA1                                  
017107     MOVE '  GE'              TO GODK-STATUSKODER                         
017108     CALL CBLTDLI USING GHNP     INLB-PMRK-PCB                            
017109                                 DLI-IO-AREA-1                            
017110                                 SSA1                                     
017111     MOVE INLB-PMRK-STATUS-CODE  TO STATUS-WS                             
017112     PERFORM IMS-STATUSKONTROLL                                           
017113     .                                                                    
017114     EJECT                                                                
017115     SKIP3                                                                
017116******************************************************************        
017117*    IMS-UPPDATERING VIA INLB-PCB                                *        
017118******************************************************************        
017119*----------------------------------------------------------------*        
017120 IMS-REPL-INLB-D121 SECTION.                                              
017121                                                                          
017122     MOVE '  '               TO GODK-STATUSKODER                          
017123     CALL CBLTDLI USING REPL     INLB-PMRK-PCB                            
017124                                 DLI-IO-AREA-1                            
017125     MOVE INLB-PMRK-STATUS-CODE  TO STATUS-WS                             
017126     PERFORM IMS-STATUSKONTROLL                                           
017127     .                                                                    
017128     EJECT                                                                
017129     SKIP3                                                                
017130******************************************************************        
017131*  INLC-PCB                                                      *        
017132******************************************************************        
017133*                                                                         
017134 IMS-GU-INLC-D111  SECTION.                                               
017135     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
017136             DELIMITED BY SIZE INTO SSA1                                  
017137     MOVE '  GE'                 TO GODK-STATUSKODER                      
017138     CALL CBLTDLI USING GU       INLC-PMRK-PCB                            
017139                                 DLI-IO-AREA-1                            
017140                                 SSA1                                     
017141     MOVE INLC-PMRK-STATUS-CODE  TO STATUS-WS                             
017142     PERFORM IMS-STATUSKONTROLL                                           
017143     .                                                                    
017144     EJECT                                                                
017145     SKIP3                                                                
017146*----------------------------------------------------------------*        
017147 IMS-GN-INLC-D111  SECTION.                                               
017148     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1CSEQ-X ')'                        
017149             DELIMITED BY SIZE INTO SSA1                                  
017150     MOVE '  GE'                 TO GODK-STATUSKODER                      
017151     CALL CBLTDLI USING GN       INLC-PMRK-PCB                            
017152                                 DLI-IO-AREA-1                            
017153                                 SSA1                                     
017154     MOVE INLC-PMRK-STATUS-CODE  TO STATUS-WS                             
017155     PERFORM IMS-STATUSKONTROLL                                           
017156     .                                                                    
017157     EJECT                                                                
017158     SKIP3                                                                
017159*----------------------------------------------------------------*        
017175 IMS-GHNP-INLC-D121-SOEK   SECTION.                                       
017176     STRING 'W6INLA21(IDLEVNRK =' WCS-IDLEVNR-X                           
017177                    '&IDOKOLLI =' WCS-IDOKOLLI-X ')'                      
017178          DELIMITED BY SIZE INTO SSA1                                     
017179     MOVE '  GE'            TO GODK-STATUSKODER                           
017180     CALL CBLTDLI USING GHNP     INLC-PMRK-PCB                            
017181                                 DLI-IO-AREA-1                            
017182                                 SSA1                                     
017183     MOVE INLC-PMRK-STATUS-CODE  TO STATUS-WS                             
017184     PERFORM IMS-STATUSKONTROLL                                           
017185     .                                                                    
017186     EJECT                                                                
017187     SKIP3                                                                
017188******************************************************************        
017189*    IMS-UPPDATERING VIA INLC-PCB                                *        
017190******************************************************************        
017191*----------------------------------------------------------------*        
017192 IMS-REPL-INLC-D121 SECTION.                                              
017193                                                                          
017194     MOVE '  '               TO GODK-STATUSKODER                          
017195     CALL CBLTDLI USING REPL     INLC-PMRK-PCB                            
017196                                 DLI-IO-AREA-1                            
017197     MOVE INLC-PMRK-STATUS-CODE  TO STATUS-WS                             
017198     PERFORM IMS-STATUSKONTROLL                                           
017199     .                                                                    
017200     EJECT                                                                
017201     SKIP3                                                                
017202*----------------------------------------------------------------*        
017203     SKIP3                                                                
017204 IMS-STATUSKONTROLL SECTION.                                              
017205     SKIP2                                                                
017206     SET STATUS-IX TO 1                                                   
017207     SEARCH GODK-STATUS                                                   
017208       AT END                                                             
017209         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
017210         DELIMITED BY SIZE INTO FELTEXT                                   
017300         CALL FELLOG                                                      
017500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017600         CONTINUE                                                         
017700     END-SEARCH                                                           
017800     .                                                                    
017810     EJECT                                                                

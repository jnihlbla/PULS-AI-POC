001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W6019100.                                                
001300*AUTHOR.         ANNELIE ENGLUND.                                         
001400*DATE-WRITTEN.   92/05/04.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        PROGRAMMET ÄR EN BAKGRUNDS-MPP, SOM LÄGGER UPP UPP-              
002000*        FÖLJNINGSTRANSAR PÅ W6G3                                         
002010*        EN FÖRÄNDRING AV ANTAL, SYSTEMSTATUS, PLACERING ELLER            
002020*        ADRESS, ENSAMMA ELLER FLERA PÅ EN GÅNG GER EN UPP-               
002030*        FÖLJNINGSTRANS                                                   
002040*        PROGRAMMET HÅLLER REDA PÅ OM UPPDATERING SKA SKE ELLER           
002050*        EJ. DET KAN ALLTSÅ KOMMA MID'AR MED INFO SOM EJ RESULTE-         
002060*        RAR I UPPDATERING AV W6G3                                        
002100*                                                                         
002110*        PROGRAMMET UPPDATERAR ÄVEN W6L2 NÄR ANTAL, SYSTEMSTATUS          
002120*        PLACERING ELLER ADRESS ÄNDRATS. UPPDATERING SKER ALLTID          
002130*        AV ALL INFORMATION SOM FINNS I MID-EN. EN POST PER RAD           
002140*        I MID-EN.                                                        
002150*                                                                         
002200*                                                                         
002201*        PROGRAMMET UPPDATERAR W6FILA (W6G3)                              
002202*                              W6UPFB (W6L2)                              
002210*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
002300*                                                                         
002400*    INDATA.                                                              
002500*        TRANSAKTION: W6T191                                              
002600*        MID:         W6I19101                                            
002700*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003501                                                                          
003510*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08)   VALUE 'W6019100'.            
003700                                                                          
003800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004000                                                                          
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004900                                                                          
005000*    --- ARBETSFÄLT                                                       
005200                                                                          
005301 77  UPPDATERA-SW                PIC X       VALUE 'N'.                   
005302     88  UPPDATERA-JA                        VALUE 'J'.                   
005400                                                                          
005430                                                                          
005500 77  INDX                        PIC 9(2)    VALUE ZERO.                  
005600 77  WS-KDINLUPF                 PIC X(4)    VALUE SPACE.                 
006100                                                                          
007000     EJECT                                                                
007010*      --- VALID IDDC CODES                                               
007020*                                                                         
007030*01    -COPY WWDC99                                                       
007040       EJECT                                                              
007100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007200 01  GENERELLA-SUBPROGRAM.                                                
007300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     EJECT                                                                
008700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009000     SKIP3                                                                
009100*01  MID -COPY W6I19101                                                   
009200     EJECT                                                                
009300 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
009400     SKIP3                                                                
009500*01  -COPY WMSGAREA                                                       
009600     EJECT                                                                
010400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010500*                                                                         
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  NYCKLAR-TILL-DLI.                                                    
011007                                                                          
011008     03  W-W6GX6005-X.                                                    
011009         05  W-IDHTYP            PIC X(4)     VALUE '6005'.               
011010         05  W-IDDC              PIC X(2)     VALUE SPACE.                
011011         05  W-LOW-VALUE         PIC X(24)    VALUE LOW-VALUE.            
011012                                                                          
011013     03  W-W6GXKEY-X.                                                     
011020         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
011030         05  W-LOW-VALUE         PIC X(1)    VALUE LOW-VALUE.             
011100     SKIP2                                                                
011110                                                                          
011120     03  W-W6L201KY-X.                                                    
011130         05  W-IDLOPNRM          PIC S9(9)   COMP-3 VALUE ZERO.           
011140         05  W-IDRADNR           PIC S9(5)   COMP-3 VALUE ZERO.           
011141         05  W-DAREGDAT          PIC  9(8)          VALUE ZERO.           
011142         05  W-TIKLOCK           PIC S9(9)   COMP-3 VALUE ZERO.           
011150     SKIP2                                                                
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
012800*    ---  DLI INPUT-OUTPUT AREA W6G3                                      
012900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
013000     SKIP3                                                                
013100 01  DLI-IO-AREA-FILA.                                                    
013200     03  IO-AREA-FILA            PIC X(300)  VALUE SPACE.                 
013301     SKIP3                                                                
013302     03  W6FILA01 REDEFINES IO-AREA-FILA.                                 
013303*        05  -COPY W6G301                                                 
013304             07  W6010012 REDEFINES FIL-W6G301-DATA.                      
013305*                09  -COPY W6010012                                       
013306     SKIP3                                                                
013307                                                                          
013308*    ---  DLI INPUT-OUTPUT AREA W6G1                                      
013309 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
013310     SKIP3                                                                
013311 01  DLI-IO-AREA-PLAA.                                                    
013312     03  IO-AREA-PLAA            PIC X(150)  VALUE SPACE.                 
013313     SKIP3                                                                
013314     03  W6PLAA01 REDEFINES IO-AREA-PLAA.                                 
013315*        05  -COPY W6GX01    -PRE PLAA-                                   
013316     SKIP3                                                                
013317     03  W6PLAA11 REDEFINES IO-AREA-PLAA.                                 
013320*        05  -COPY W6GX6006  -PRE PLAA-                                   
013600     EJECT                                                                
013610*    ---  DLI INPUT-OUTPUT AREA W6L2                                      
013620 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA3'.        
013630     SKIP3                                                                
013640 01  DLI-IO-AREA-UPFB.                                                    
013650     03  IO-AREA-UPFB            PIC X(150)  VALUE SPACE.                 
013660     SKIP3                                                                
013670     03  W6L201   REDEFINES IO-AREA-UPFB.                                 
013680*        05  -COPY W6L201                                                 
013690     SKIP3                                                                
013693     EJECT                                                                
013700 LINKAGE SECTION.                                                         
013800                                                                          
013900*01  -COPY W0009   -PRE MSG-                                              
014001     EJECT                                                                
014002*01  -COPY W0008  -PRE FILA-                                              
014003     05  FILLER                  PIC X.                                   
014004     EJECT                                                                
014005*01  -COPY W0008  -PRE PLAA-                                              
014010     05  FILLER                  PIC X.                                   
014020*01  -COPY W0008  -PRE UPFB-                                              
014030     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014201 PROCEDURE DIVISION  USING MSG-PCB FILA-PCB PLAA-PCB UPFB-PCB.            
014210     ENTRY 'DLITCBL' USING MSG-PCB FILA-PCB PLAA-PCB UPFB-PCB.            
014300                                                                          
014500     PERFORM IMS-GET-MSG                                                  
014600     IF SEGMENT-FINNS                                                     
014700       PERFORM A-INIT                                                     
014800       MOVE +1 TO INDX                                                    
014900       PERFORM UNTIL INDX > MID-KVPOST                                    
015006                                                                          
015007         PERFORM B-LAEGG-UPP-TRANS                                        
015010         ADD +1 TO INDX                                                   
015020         MOVE NEJ TO UPPDATERA-SW                                         
015210       END-PERFORM                                                        
016100     END-IF                                                               
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
016710     MOVE MSG-MID-OUT TO MID-W6I19101                                     
016711     MOVE MID-IDDC    TO W-IDDC                                           
016720                                                                          
020100     .                                                                    
020200     EJECT                                                                
020300 B-LAEGG-UPP-TRANS SECTION.                                               
020400                                                                          
020500     MOVE SPACE TO DLI-IO-AREA-FILA                                       
020600                                                                          
020610     MOVE MID-IDPGM               TO FIL-IDPGM                            
020700     ACCEPT FIL-TIREGDAT FROM DATE                                        
020701     MOVE FUNCTION CURRENT-DATE (1:8) TO UPPF-DAREGDAT                    
020710     ACCEPT FIL-TIKLOCK  FROM TIME                                        
020711     ACCEPT UPPF-TIKLOCK FROM TIME                                        
020720     MOVE +1                      TO FIL-IDSEKVNR                         
020721                                                                          
020722     MOVE 'W601'                  TO FIL-CT-IDSYSTEM                      
020730     MOVE '001'                   TO FIL-CT-IDPTYP                        
020731     MOVE '2'                     TO FIL-CT-IDVTYP                        
020740                                                                          
020744                                                                          
020750     MOVE MID-IDDC                TO W601-IDDC                            
020751     MOVE MID-IDLOPNRM(INDX)      TO W601-IDLOPNRM                        
020752                                     UPPF-IDLOPNRM                        
020760     MOVE MID-IDRADNR(INDX)       TO W601-IDRADNR                         
020770                                     UPPF-IDRADNR                         
020780     MOVE MID-KDINLPRIO(INDX)     TO W601-KDINLPRIO                       
020790     MOVE MID-PRARTSTD(INDX)      TO W601-PRARTSTD                        
020791     MOVE MID-KVKOLLI (INDX)      TO W601-KVKOLLI                         
020792     MOVE MID-FLINLI  (INDX)      TO W601-FLINLI                          
020793                                                                          
020900     PERFORM BA-KOLLA-KVINLART                                            
020901                                                                          
020910     PERFORM BB-KOLLA-KDINLSTA                                            
020911                                                                          
020920     PERFORM BC-KOLLA-KDINLUPF                                            
021000                                                                          
021010     PERFORM BD-KOLLA-KDINLUPF-NXT                                        
021100                                                                          
021110     IF UPPDATERA-JA                                                      
021120       PERFORM IMS-ISRT-FILA-W6G301                                       
021121       PERFORM UNTIL SEGMENT-FINNS                                        
021122         ADD +1 TO FIL-IDSEKVNR                                           
021123         PERFORM IMS-ISRT-FILA-W6G301                                     
021124       END-PERFORM                                                        
021125     END-IF                                                               
021126                                                                          
021127**NDC                                                                     
021128     MOVE MID-IDDC      TO WS-IDDC                                        
021129     IF CDC OR NDC-CN OR NDC-US                                           
021130       MOVE MID-KVINLART-NEW(INDX)      TO UPPF-KVINLART                  
021131       MOVE MID-KDINLSTA-NEW(INDX)      TO UPPF-KDINLSTA                  
021132       MOVE MID-ADINLOMR-NEW(INDX)      TO UPPF-ADINLOMR                  
021133       MOVE MID-ADINLOMR-NXT-NEW(INDX)  TO UPPF-ADINLOMR-NXT              
021134       MOVE MSG-SIGNON-USERID           TO UPPF-IDUSER                    
021135       PERFORM IMS-ISRT-UPFB-W6L201                                       
021136     END-IF                                                               
021140     .                                                                    
029100     EJECT                                                                
029110 BA-KOLLA-KVINLART SECTION.                                               
029120                                                                          
029121     MOVE MID-KVINLART-NEW(INDX) TO W601-KVINLART                         
029122                                                                          
029126     IF MID-KVINLART-OLD(INDX) NOT = MID-KVINLART-NEW(INDX)               
029127       MOVE JA TO UPPDATERA-SW                                            
029128     END-IF                                                               
029130     .                                                                    
029150     EJECT                                                                
029160 BB-KOLLA-KDINLSTA SECTION.                                               
029170                                                                          
029180     MOVE MID-KDINLSTA-NEW(INDX) TO W601-KDINLSTA                         
029190                                                                          
029194     IF MID-KDINLSTA-OLD(INDX) NOT = MID-KDINLSTA-NEW(INDX)               
029195       MOVE JA TO UPPDATERA-SW                                            
029196     END-IF                                                               
029198     .                                                                    
029199     EJECT                                                                
029200 BC-KOLLA-KDINLUPF SECTION.                                               
029201                                                                          
029202****************************************************                      
029203* TVÅ FALL AV FÖRÄNDRAD PLACERING ORSAKAR          *                      
029204* UPPDATERING AV TRANSBASEN W6G3                   *                      
029205****************************************************                      
029206                                                                          
029207****************************************************                      
029208* I FÖRSTA FALLET ÄR EN AV PLACERINGARNA, GAMLA    *                      
029209* ELLER NYA, BLANK OCH DEN ANDRA ÄR IFYLLD.        *                      
029210* DEN ICKE-BLANKA ANVÄNDS I BÅDA LÄGEN             *                      
029211****************************************************                      
029212                                                                          
029216     IF (MID-ADINLOMR-OLD(INDX) = SPACE AND                               
029217        MID-ADINLOMR-NEW(INDX) NOT = SPACE) OR                            
029218        (MID-ADINLOMR-OLD(INDX) NOT = SPACE AND                           
029219        MID-ADINLOMR-NEW(INDX) = SPACE)                                   
029220       IF MID-ADINLOMR-OLD(INDX) = SPACE                                  
029221         MOVE MID-ADINLOMR-NEW(INDX)TO W-ADINLOMR                         
029222       ELSE                                                               
029223         MOVE MID-ADINLOMR-OLD(INDX)TO W-ADINLOMR                         
029224       END-IF                                                             
029225                                                                          
029226       PERFORM IMS-GU-PLAA-W6G130                                         
029227       MOVE PLAA-6006-KDINLUPF TO W601-KDINLUPF                           
029228                                                                          
029229       MOVE JA TO UPPDATERA-SW                                            
029230     END-IF                                                               
029231                                                                          
029232     EJECT                                                                
029233                                                                          
029234****************************************************                      
029235* I ANDRA FALLET ÄR BÅDA PLACERINGARNA IFYLLDA     *                      
029236* MEN HAR OLIKA VÄRDEN                             *                      
029237* ÄVEN OM INGEN STATUSFÖRÄNDRING SKER FYLLS AKTUELL*                      
029238* STATUS I PÅ CTEXT IFALL ANNAN W6G3-UPPDATERANDE  *                      
029239* FÖRÄNDRING SKER                                  *                      
029240****************************************************                      
029241                                                                          
029242     IF (MID-ADINLOMR-OLD(INDX) NOT = SPACE AND                           
029243        MID-ADINLOMR-NEW(INDX) NOT = SPACE)                               
029244                                                                          
029245       IF MID-ADINLOMR-OLD(INDX) NOT = MID-ADINLOMR-NEW(INDX)             
029246                                                                          
029247         MOVE MID-ADINLOMR-OLD(INDX) TO W-ADINLOMR                        
029248         PERFORM IMS-GU-PLAA-W6G101                                       
029249         PERFORM IMS-GNP-PLAA-W6G130                                      
029250         MOVE PLAA-6006-KDINLUPF TO WS-KDINLUPF                           
029251         MOVE MID-ADINLOMR-NEW(INDX) TO W-ADINLOMR                        
029252         PERFORM IMS-GNP-FIRST-PLAA-W6G130                                
029253         MOVE PLAA-6006-KDINLUPF TO W601-KDINLUPF                         
029254                                                                          
029255         IF WS-KDINLUPF NOT = PLAA-6006-KDINLUPF                          
029256           MOVE JA TO UPPDATERA-SW                                        
029257         END-IF                                                           
029258       ELSE                                                               
029259         MOVE MID-ADINLOMR-NEW(INDX) TO W-ADINLOMR                        
029260         PERFORM IMS-GU-PLAA-W6G130                                       
029261         MOVE PLAA-6006-KDINLUPF TO W601-KDINLUPF                         
029262       END-IF                                                             
029263     END-IF                                                               
029264                                                                          
029265     .                                                                    
029266     EJECT                                                                
029267 BD-KOLLA-KDINLUPF-NXT SECTION.                                           
029268                                                                          
029269****************************************************                      
029270* TVÅ FALL AV FÖRÄNDRAD ADRESS ORSAKAR             *                      
029271* UPPDATERING AV TRANSBASEN W6G3                   *                      
029272****************************************************                      
029273                                                                          
029274****************************************************                      
029275* I FÖRSTA FALLET ÄR EN AV ADRESSERNA, GAMLA       *                      
029276* ELLER NYA, BLANK OCH DEN ANDRA ÄR IFYLLD         *                      
029277* DEN NYA ADRESSEN ANVÄNDS I BÅDA LÄGEN            *                      
029278****************************************************                      
029279                                                                          
029283     IF (MID-ADINLOMR-NXT-OLD(INDX) = SPACE AND                           
029284        MID-ADINLOMR-NXT-NEW(INDX) NOT = SPACE) OR                        
029285        (MID-ADINLOMR-NXT-OLD(INDX) NOT = SPACE AND                       
029286        MID-ADINLOMR-NXT-NEW(INDX) = SPACE)                               
029287                                                                          
029288       MOVE MID-ADINLOMR-NXT-NEW(INDX)TO W-ADINLOMR                       
029289                                         W601-KDINLUPF-NXT                
029290       IF MID-ADINLOMR-NXT-NEW(INDX) NOT = SPACE                          
029291         PERFORM IMS-GU-PLAA-W6G130                                       
029292         MOVE PLAA-6006-KDINLUPF TO W601-KDINLUPF-NXT                     
029294       END-IF                                                             
029297                                                                          
029298       MOVE JA TO UPPDATERA-SW                                            
029299     END-IF                                                               
029300                                                                          
029301     EJECT                                                                
029302                                                                          
029303****************************************************                      
029304* I ANDRA FALLET ÄR BÅDA PLACERINGARNA IFYLLDA     *                      
029305* MEN HAR OLIKA VÄRDEN                             *                      
029306* ÄVEN OM INGEN STATUSFÖRÄNDRING SKER FYLLS AKTUELL*                      
029307* STATUS I PÅ CTEXT IFALL ANNAN W6G3-UPPDATERANDE  *                      
029308* FÖRÄNDRING SKER                                  *                      
029309****************************************************                      
029310                                                                          
029311     IF (MID-ADINLOMR-NXT-OLD(INDX) NOT = SPACE AND                       
029312        MID-ADINLOMR-NXT-NEW(INDX) NOT = SPACE)                           
029313                                                                          
029314       IF MID-ADINLOMR-NXT-OLD(INDX) NOT =                                
029315          MID-ADINLOMR-NXT-NEW(INDX)                                      
029316                                                                          
029317         MOVE MID-ADINLOMR-NXT-OLD(INDX) TO W-ADINLOMR                    
029318         PERFORM IMS-GU-PLAA-W6G101                                       
029319         PERFORM IMS-GNP-PLAA-W6G130                                      
029320         MOVE PLAA-6006-KDINLUPF TO WS-KDINLUPF                           
029321         MOVE MID-ADINLOMR-NXT-NEW(INDX) TO W-ADINLOMR                    
029322         PERFORM IMS-GNP-FIRST-PLAA-W6G130                                
029323         MOVE PLAA-6006-KDINLUPF TO W601-KDINLUPF-NXT                     
029324                                                                          
029325         IF WS-KDINLUPF NOT = PLAA-6006-KDINLUPF                          
029326           MOVE JA TO UPPDATERA-SW                                        
029327         END-IF                                                           
029328       ELSE                                                               
029329         MOVE MID-ADINLOMR-NXT-NEW(INDX) TO W-ADINLOMR                    
029330         PERFORM IMS-GU-PLAA-W6G130                                       
029331         MOVE PLAA-6006-KDINLUPF TO W601-KDINLUPF-NXT                     
029332       END-IF                                                             
029333     END-IF                                                               
029334                                                                          
029335     .                                                                    
029336     EJECT                                                                
029337* --- IMS SEKTIONER ---                                                   
029340     SKIP3                                                                
029400 IMS-GET-MSG SECTION.                                                     
029500                                                                          
029600     MOVE '  QC' TO GODK-STATUSKODER                                      
029700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
029800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
029900     PERFORM IMS-STATUSKONTROLL                                           
030000     .                                                                    
031301     EJECT                                                                
031311 IMS-ISRT-FILA-W6G301 SECTION.                                            
031314     MOVE 'W6FILA01 ' TO SSA1                                             
031315     MOVE '  II' TO GODK-STATUSKODER                                      
031316     CALL CBLTDLI USING ISRT FILA-PCB DLI-IO-AREA-FILA SSA1               
031317     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
031318     PERFORM IMS-STATUSKONTROLL                                           
031319     .                                                                    
031320     EJECT                                                                
031321 IMS-GU-PLAA-W6G130 SECTION.                                              
031323     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX6005-X ')'                        
031324          DELIMITED BY SIZE INTO SSA1                                     
031325     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-X ')'                         
031326          DELIMITED BY SIZE INTO SSA2                                     
031327     MOVE '  ' TO GODK-STATUSKODER                                        
031328     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA-PLAA SSA1 SSA2            
031329     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031330     PERFORM IMS-STATUSKONTROLL                                           
031331     .                                                                    
031332     SKIP3                                                                
031340 IMS-GU-PLAA-W6G101 SECTION.                                              
031350     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX6005-X ')'                        
031360          DELIMITED BY SIZE INTO SSA1                                     
031390     MOVE '  ' TO GODK-STATUSKODER                                        
031400     CALL CBLTDLI USING GU PLAA-PCB DLI-IO-AREA-PLAA SSA1                 
031410     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031420     PERFORM IMS-STATUSKONTROLL                                           
031430     .                                                                    
031431     SKIP2                                                                
031432 IMS-GNP-PLAA-W6G130 SECTION.                                             
031435     STRING 'W6PLAA11(W6GXKEY  =' W-W6GXKEY-X ')'                         
031436          DELIMITED BY SIZE INTO SSA1                                     
031437     MOVE '  ' TO GODK-STATUSKODER                                        
031438     CALL CBLTDLI USING GNP PLAA-PCB DLI-IO-AREA-PLAA SSA1                
031439     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031440     PERFORM IMS-STATUSKONTROLL                                           
031441     .                                                                    
031442     SKIP3                                                                
031444 IMS-GNP-FIRST-PLAA-W6G130 SECTION.                                       
031446     STRING 'W6PLAA11*F(W6GXKEY  =' W-W6GXKEY-X ')'                       
031447          DELIMITED BY SIZE INTO SSA1                                     
031448     MOVE '  ' TO GODK-STATUSKODER                                        
031449     CALL CBLTDLI USING GNP PLAA-PCB DLI-IO-AREA-PLAA SSA1                
031450     MOVE PLAA-STATUS-CODE TO STATUS-WS                                   
031451     PERFORM IMS-STATUSKONTROLL                                           
031452     .                                                                    
031453     SKIP3                                                                
031460     EJECT                                                                
031470 IMS-ISRT-UPFB-W6L201 SECTION.                                            
031491     MOVE 'W6UPFB01' TO SSA1                                              
031492     MOVE '  II' TO GODK-STATUSKODER                                      
031493     CALL CBLTDLI USING ISRT UPFB-PCB DLI-IO-AREA-UPFB SSA1               
031494     MOVE UPFB-STATUS-CODE TO STATUS-WS                                   
031495     PERFORM IMS-STATUSKONTROLL                                           
031496     .                                                                    
031497     EJECT                                                                
031500 IMS-STATUSKONTROLL SECTION.                                              
031600                                                                          
031700     SET STATUS-IX TO 1                                                   
031800     SEARCH GODK-STATUS                                                   
031900       AT END                                                             
032000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032100         DELIMITED BY SIZE INTO FELTEXT                                   
032200         CALL FELLOG                                                      
032300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032400         CONTINUE                                                         
032500     END-SEARCH                                                           
032600     .                                                                    

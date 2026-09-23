001300 ID DIVISION.                                                             
001400 PROGRAM-ID.     W5021500.                                                
001500 AUTHOR.         JONNY SANDSTEN.                                          
001600 DATE-WRITTEN.   98/06/24.                                                
001700 DATE-COMPILED.                                                           
001800                                                                          
001900*    FUNKTION:                                                            
002210*        PROGRAMMET SYFTE ÄR ATT HANTERA NYREGISTERING                    
002220*        AV EKONOMISK SUBHÄNDELSETYP M.H.A AV VÄRDEN                      
002230*        INMATADE FRÅN BILD 5215                                          
002300*                                                                         
002410*        PROGRAMMET UPPDATERAR WDH5                                       
002500*                                                                         
002600*    INDATA.                                                              
002700*        TRANSAKTION: W5T215                                              
002800*        MID:         W5I21501                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W5O21501                                            
003200                                                                          
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W5021500'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004601*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004602 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004610 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004700*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900                                                                          
005000 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005001     88  ALLT-OK                             VALUE 'J'.                   
005002                                                                          
005003 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005004     88  INDATA-OK                           VALUE 'J'.                   
005010     88  INDATA-FEL                          VALUE 'N'.                   
005100                                                                          
005200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005300     88  NYCKLAR-OK                          VALUE 'J'.                   
005400     88  NYCKLAR-FEL                         VALUE 'N'.                   
005500                                                                          
005600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005700     88  EGEN-MID                            VALUE '5215'.                
005800     88  GODK-MID                            VALUE '5211' '5212'          
005900                                                   '5213' '5214'          
006000                                                   '5215' '5216'          
006100                                                   '5217' '5218'          
006200                                                   '5219'.                
006300     88  HELP-MID                            VALUE '0551'.                
006400     EJECT                                                                
006500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006600 01  GENERELLA-SUBPROGRAM.                                                
006700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007400*01 -COPY WMEDAREA                                                        
007500     SKIP3                                                                
007600 01  MESSAGE-CODES.                                                       
007701     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007702     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007703     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007710     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007801     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007810     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
007900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007910     03  RAD-FINNS-REDAN         PIC X(3)    VALUE '245'.                 
008000     03  KDEKHHT-MISSING         PIC X(3)    VALUE '269'.                 
008010     03  KDEKSHT-MISSING         PIC X(3)    VALUE '270'.                 
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008500     SKIP3                                                                
008600*01 -COPY WMSGINIT                                                        
008701     EJECT                                                                
008702*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
008703*                                                                         
008704 01  SPAR-AREA.                                                           
008705     03  SPAR-IDTRANS             PIC X(4)    VALUE '5215'.               
008706     03  SPAR-IDFTG-ENTER         PIC 9(2).                               
008707     03  SPAR-IDFTG-NEXT          PIC 9(2).                               
008708     03  SPAR-KDEKHHT-ENTER       PIC X(3).                               
008709     03  SPAR-KDEKHHT-NEXT        PIC X(3).                               
008710     03  SPAR-KDEKSHT-ENTER       PIC X(3).                               
008720     03  SPAR-KDEKSHT-NEXT        PIC X(3).                               
008800     EJECT                                                                
008900*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009000*                                                                         
009100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009200     SKIP3                                                                
009300*01  MID -COPY W5I21501                                                   
009400     EJECT                                                                
009500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009600     SKIP3                                                                
009700*01  -COPY WMSGAREA                                                       
009800     EJECT                                                                
009900     03  MOD REDEFINES MSG-AREA.                                          
010000*      05  -COPY W5O21501                                                 
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010300     SKIP3                                                                
010400*01  -COPY WMFSAREA                                                       
010500     EJECT                                                                
010600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010700*                                                                         
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000     SKIP3                                                                
011100 01  NYCKLAR-TILL-DLI.                                                    
011201*    --- VÄRDE PÅ BLÄDDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN            
011204                                                                          
011211     03  W-WDH501KY-X.                                                    
011212         05  W-IDFTG             PIC 9(2)    VALUE ZERO.                  
011213         05  W-KDEKHHT           PIC X(3)    VALUE SPACE.                 
011214                                                                          
011215     03  W-KDEKSHT-X.                                                     
011220         05  W-KDEKSHT           PIC X(3)    VALUE SPACE.                 
011221                                                                          
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
011800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011900     SKIP2                                                                
012000 01  GODK-STATUSKODER.                                                    
012100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012200     SKIP3                                                                
012300 01  SSA1                        PIC X(64).                               
012400 01  SSA2                        PIC X(64).                               
012410 01  SSA3                        PIC X(64).                               
012500     EJECT                                                                
012600*    --- IMS FUNKTIONSKODER                                               
012700*01  -COPY W0003                                                          
012900     EJECT                                                                
013000*    ---  DLI INPUT-OUTPUT AREA                                           
013100                                                                          
013201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH501'.                      
013202 01  DLI-IO-WDH501.                                                       
013203*    03  -COPY WDH501                                                     
013204     EJECT                                                                
013205 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDH511'.                      
013206 01  DLI-IO-WDH511.                                                       
013210*    03  -COPY WDH511                                                     
013500     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700*01  -COPY W0009   -PRE MSG-                                              
013800*01  -COPY W0008   -PRE USEA-                                             
013900     05  FILLER                  PIC X.                                   
014001                                                                          
014002*01  -COPY W0008  -PRE WDH5-                                              
014010     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014201 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDH5-PCB.                     
014202 MAIN SECTION.                                                            
014210     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDH5-PCB.                     
014300                                                                          
014500     PERFORM IMS-GET-MSG                                                  
014600     IF SEGMENT-FINNS                                                     
014700       PERFORM A-INIT                                                     
014800       PERFORM B-KOLLA-NYCKLAR                                            
014900       IF NYCKLAR-OK                                                      
015001         IF MFS-UPDATE                                                    
015002           PERFORM G-KOLLA-INPUT                                          
015003           IF INDATA-OK                                                   
015004             PERFORM H-UPPDATERA                                          
015005           END-IF                                                         
015010         ELSE                                                             
015101           IF MFS-FIRST                                                   
015102             PERFORM C-FOERSTA-SIDA                                       
015103           ELSE                                                           
015104             IF MFS-NEXT                                                  
015105               PERFORM D-NAESTA-SIDA                                      
015106             ELSE                                                         
015107               PERFORM E-SAMMA-SIDA                                       
015108             END-IF                                                       
015110           END-IF                                                         
015310         END-IF                                                           
015320         IF ALLT-OK                                                       
015400           PERFORM F-LAES-VISA-INFO                                       
015410         END-IF                                                           
015500       END-IF                                                             
015800       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O21501 + 4                      
015900       PERFORM IMS-INSERT-MSG                                             
016000     END-IF                                                               
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT SECTION.                                                          
016800                                                                          
016900     IF MSG-DUBBLA-TRANSKODER                                             
017000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I21501                 
017100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
017200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
017300     ELSE                                                                 
017400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I21501                  
017500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
017600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017700     END-IF                                                               
017800                                                                          
017900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
018200                                                                          
018300     MOVE LOW-VALUE TO MSG-AREA                                           
018400     MOVE 'W5O215N1' TO MFS-IDMOD                                         
018500     MOVE '5215' TO MOD-IDTRANS                                           
018600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
018700                                                                          
018800     IF EGEN-MID OR HELP-MID                                              
018900       CONTINUE                                                           
019000     ELSE                                                                 
019100       MOVE SPACE TO MFS-KDTRTYP                                          
019200       MOVE '7' TO MFS-IDPFK                                              
019300     END-IF                                                               
019600     .                                                                    
019700     EJECT                                                                
019800 B-KOLLA-NYCKLAR SECTION.                                                 
019900                                                                          
020000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020100     MOVE '001'             TO MSGI-KDCALL                                
020200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
020300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
020400     MOVE '5215'            TO MSGI-IDTRANS                               
020410     IF EGEN-MID                                                          
020411       MOVE MID-KDEKHHT-IN  TO MSGI-KDEKHHT                               
020450     END-IF                                                               
020460     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020461     MOVE MSGI-SPAR-AREA      TO SPAR-AREA                                
020462                                                                          
020463     IF MSGI-IDLAND-SPR = 'GB'                                            
020464       MOVE 'GB' TO MED-IDSKYLT                                           
020465     ELSE                                                                 
020466       MOVE 'S' TO MED-IDSKYLT                                            
020467     END-IF                                                               
020470                                                                          
020480     MOVE JA TO ALLT-SW                                                   
021000     MOVE JA TO NYCKLAR-SW                                                
021010     MOVE SPACE TO MED-IDMFSINF                                           
021020     MOVE SPACE TO MED-IDMFSFEL                                           
021100                                                                          
021201                                                                          
021202*    -- KONTROLL AV KDEKHHT                                               
021203     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
021204                                                                          
021211     IF MSGI-KDEKHHT NUMERIC AND MSGI-KDEKHHT > ZERO                      
021212       MOVE MSGI-KDEKHHT    TO W-KDEKHHT                                  
021213     ELSE                                                                 
021214       MOVE NEJ             TO NYCKLAR-SW                                 
021215     END-IF                                                               
021216                                                                          
021217     IF MSGI-IDFTG   NUMERIC AND MSGI-IDFTG   > ZERO                      
021218       MOVE MSGI-IDFTG      TO W-IDFTG                                    
021219     END-IF                                                               
021301                                                                          
021302     IF GODK-MID OR NYCKLAR-OK                                            
021303       MOVE MSGI-IDFTG          TO MOD-IDFTG-UT                           
021304       MOVE MSGI-KDEKHHT        TO MOD-KDEKHHT-UT                         
021305       INSPECT MOD-KDEKHHT-UT REPLACING LEADING ZERO BY SPACE             
021307     ELSE                                                                 
021308       MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-UT                             
021309                               MOD-IDFTG-UT                               
021310     END-IF                                                               
021400                                                                          
021500     IF NYCKLAR-FEL                                                       
021510*---FÖR ATT INTE FÅ NYCKLAR FEL NÄR MAN KOMMER FRÅN EN ICKE               
021520*---GODKÄND BILD                                                          
021530       IF GODK-MID                                                        
021600         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
021700         CALL WMEDKONV USING MED-WMEDAREA                                 
021800         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
021900         PERFORM MFS-RENSA-FAELT-IN                                       
022000         PERFORM MFS-RENSA-FAELT-UT                                       
022100       END-IF                                                             
022110     END-IF                                                               
022200     .                                                                    
022301     EJECT                                                                
022302 C-FOERSTA-SIDA SECTION.                                                  
022303                                                                          
022304     MOVE INF-FIRST-PAGE TO MED-IDMFSFEL                                  
022305     CALL WMEDKONV USING MED-WMEDAREA                                     
022306     MOVE MED-MFSFEL TO MOD-TEMFSFEL                                      
022307                                                                          
022308     PERFORM MFS-RENSA-FAELT-IN                                           
022309     .                                                                    
022310     EJECT                                                                
022311 D-NAESTA-SIDA SECTION.                                                   
022312                                                                          
022313     IF SPAR-IDTRANS = '5215'                                             
022316       MOVE SPAR-KDEKSHT-NEXT TO W-KDEKSHT                                
022318     ELSE                                                                 
022319       PERFORM MFS-RENSA-FAELT-IN                                         
022320     END-IF                                                               
022321     .                                                                    
022322     EJECT                                                                
022323 E-SAMMA-SIDA SECTION.                                                    
022324                                                                          
022325     IF SPAR-IDTRANS = '5215' OR '0551'                                   
022326*---OM MID-KDEKHHT-IN ÄNDRATS OCH 'ENTER' AKTIVERATS                      
022327*---MÅSTE FÖLJANDE IF-SATS ANVÄNDAS FÖR ATT FÅ UT NÅGON DATA              
022328       IF MID-KDEKSHT-NY NOT = ALL '+'                                    
022329        AND MID-BEEKSHT-NY NOT = ALL '+'                                  
022331         MOVE SPAR-KDEKSHT-ENTER TO W-KDEKSHT                             
022332         MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                              
022333         CALL WMEDKONV USING MED-WMEDAREA                                 
022334         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
022335         PERFORM MFS-ROER-EJ-FAELT-IN                                     
022336         PERFORM MFS-LAES-IN-IGEN                                         
022337         MOVE NEJ TO ALLT-SW                                              
022338       END-IF                                                             
022339     ELSE                                                                 
022340       PERFORM MFS-RENSA-FAELT-IN                                         
022341     END-IF                                                               
022342     .                                                                    
022350     EJECT                                                                
022600 F-LAES-VISA-INFO SECTION.                                                
022700                                                                          
022720     PERFORM IMS-GU-HHT                                                   
023674                                                                          
023676     IF SEGMENT-SAKNAS                                                    
023677       IF MED-IDMFSFEL = SPACE                                            
023678         MOVE KDEKHHT-MISSING TO MED-IDMFSFEL                             
023679       END-IF                                                             
023681       CALL WMEDKONV USING MED-WMEDAREA                                   
023682       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
023683       PERFORM MFS-RENSA-FAELT-UT                                         
023684     ELSE                                                                 
023685       MOVE HHT-KDEKHHT   TO MOD-KDEKHHT                                  
023686                             MOD-KDEKHHT-NY                               
023688       MOVE HHT-BEEKHHT   TO MOD-BEEKHHT                                  
023689                             MOD-BEEKHHT-NY                               
023690                                                                          
023691*---MFS-NEXT ANVÄNDS NÄR MAN SKALL LÄSA NÄSTA SIDAS POSTER                
023692*---1:A POSTEN LÄSES DÅ UNIKT                                             
023693       IF MFS-NEXT                                                        
023694         PERFORM IMS-GU-SHT                                               
023695       ELSE                                                               
023696         PERFORM IMS-GNP-SHT                                              
023697       END-IF                                                             
023698       IF SEGMENT-SAKNAS                                                  
023699         IF MED-IDMFSFEL = SPACE                                          
023700           MOVE KDEKSHT-MISSING TO MED-IDMFSFEL                           
023701         END-IF                                                           
023702         CALL WMEDKONV USING MED-WMEDAREA                                 
023703         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
023704*        PERFORM MFS-RENSA-FAELT-UT                                       
023705       END-IF                                                             
023706       MOVE +1 TO INDX                                                    
023707       PERFORM UNTIL INDX > MAX-INDX OR SEGMENT-SAKNAS                    
023708         IF SEGMENT-FINNS                                                 
023709           MOVE SHT-KDEKSHT   TO MOD-KDEKSHT (INDX)                       
023710           MOVE SHT-BEEKSHT   TO MOD-BEEKSHT (INDX)                       
023711         ELSE                                                             
023712           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
023713         END-IF                                                           
023714         ADD +1 TO INDX                                                   
023715         PERFORM IMS-GNP-SHT                                              
023716       END-PERFORM                                                        
023717                                                                          
023718       IF SEGMENT-FINNS                                                   
023719         MOVE SHT-KDEKSHT   TO SPAR-KDEKSHT-NEXT                          
023720         IF MED-IDMFSINF = SPACE                                          
023721           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
023722         END-IF                                                           
023723         CALL WMEDKONV USING MED-WMEDAREA                                 
023724         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
023725       END-IF                                                             
023726     END-IF                                                               
023727                                                                          
023728     MOVE '002'    TO MSGI-KDCALL                                         
023729     MOVE '5215'     TO SPAR-IDTRANS                                      
023730     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
023740     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
023800     .                                                                    
023900     EJECT                                                                
024814 G-KOLLA-INPUT SECTION.                                                   
024815                                                                          
024816     MOVE JA  TO INDATA-SW                                                
024817     IF MID-KDEKHHT-IN = ALL '+'                                          
024818      AND MID-KDEKSHT-NY = ALL '+'                                        
024819      AND MID-BEEKSHT-NY = ALL '+'                                        
024820       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
024821       CALL WMEDKONV USING MED-WMEDAREA                                   
024822       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
024823       PERFORM MFS-ROER-EJ-FAELT-IN                                       
024824       PERFORM MFS-ROER-EJ-FAELT-UT                                       
024825       MOVE NEJ TO INDATA-SW                                              
024826     ELSE                                                                 
024827*---KDEKSHT FÅR EJ VARA TOMT                                              
024828       IF MID-KDEKSHT-NY = ALL '+'                                        
024829          MOVE MFS-ALFA-FAELT-FEL TO MOD-KDEKSHT-NY-ATTR                  
024830          MOVE NEJ TO INDATA-SW                                           
024831       ELSE                                                               
024832          MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDEKSHT-NY-ATTR                
024840       END-IF                                                             
024852                                                                          
024853*---BEEKSHT FÅR EJ VARA TOMT                                              
024854       IF MID-BEEKSHT-NY = ALL '+'                                        
024855          MOVE MFS-ALFA-FAELT-FEL TO MOD-BEEKSHT-NY-ATTR                  
024856          MOVE NEJ TO INDATA-SW                                           
024857       ELSE                                                               
024858          MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEEKSHT-NY-ATTR                
024859       END-IF                                                             
024860                                                                          
024861       IF INDATA-FEL                                                      
024862         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
024863         CALL WMEDKONV USING MED-WMEDAREA                                 
024864         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
024865         PERFORM MFS-ROER-EJ-FAELT-UT                                     
024866         PERFORM MFS-ROER-EJ-FAELT-IN                                     
024867       ELSE                                                               
024868*---KOLLAR ATT KDEKHHT FINNS REGISTRERAD I BASEN                          
024869         PERFORM IMS-GU-HHT                                               
024870         IF SEGMENT-SAKNAS                                                
024872           IF MED-IDMFSFEL = SPACE                                        
024873             MOVE KDEKHHT-MISSING TO MED-IDMFSFEL                         
024874           END-IF                                                         
024876           CALL WMEDKONV USING MED-WMEDAREA                               
024877           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
024878           PERFORM MFS-RENSA-FAELT-IN                                     
024879           PERFORM MFS-RENSA-FAELT-UT                                     
024880*---OM SEGMENT SAKNAS KAN INGEN UPPDATERING SKE                           
024881*---DÄRFÖR ANVÄNDS DENNA SWITCH                                           
024882           MOVE NEJ TO INDATA-SW                                          
024883*---OM INTE SEGMENT FINNS BEHÖVER MAN EJ GÅ IN I                          
024884*---F-SECTIONEN. DÄRFÖR ANVÄNDS DENNA SWITCH                              
024885           MOVE NEJ TO ALLT-SW                                            
024887         END-IF                                                           
024888       END-IF                                                             
024889     END-IF                                                               
024890     .                                                                    
024891     EJECT                                                                
024892 H-UPPDATERA SECTION.                                                     
024893                                                                          
024894     MOVE MID-KDEKSHT-NY TO SHT-KDEKSHT                                   
024895     MOVE MID-BEEKSHT-NY TO SHT-BEEKSHT                                   
024896     PERFORM IMS-ISRT-SHT                                                 
024897     IF SEGMENT-FINNS-REDAN                                               
024898       MOVE RAD-FINNS-REDAN TO MED-IDMFSINF                               
024899     ELSE                                                                 
024901       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
024903     END-IF                                                               
024904                                                                          
024905     CALL WMEDKONV USING MED-WMEDAREA                                     
024906     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
024907     PERFORM MFS-FORM-ATTR                                                
024908     PERFORM MFS-RENSA-FAELT-IN                                           
024909     MOVE MFS-ADD-SAETT-CURSOR TO MOD-KDEKSHT-NY-ATTR                     
024910     .                                                                    
024920     EJECT                                                                
025000 MFS-RENSA-FAELT-UT SECTION.                                              
025100                                                                          
025200*    --- ALLA UTDATA-FÄLT                                                 
025310*    --- INKL. BLÄDDRINGSNYCKLAR                                          
025400     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-NY                               
025501                             MOD-BEEKHHT-NY                               
025502                             MOD-KDEKSHT-NY                               
025503                             MOD-BEEKSHT-NY                               
025504                             MOD-KDEKHHT                                  
025505                             MOD-BEEKHHT                                  
025510     MOVE +1 TO INDX                                                      
025520     PERFORM UNTIL INDX > MAX-INDX                                        
025530       MOVE MFS-RENSA-FAELT TO MOD-KDEKSHT (INDX)                         
025560                               MOD-BEEKSHT (INDX)                         
025592     ADD +1 TO INDX                                                       
025593     END-PERFORM                                                          
025600     .                                                                    
025701     SKIP3                                                                
025702 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
025703                                                                          
025704*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
025705     PERFORM UNTIL INDX > MAX-INDX                                        
025706       MOVE MFS-RENSA-FAELT TO MOD-KDEKSHT (INDX)                         
025709                               MOD-BEEKSHT (INDX)                         
025712     END-PERFORM                                                          
025720     .                                                                    
025800     SKIP3                                                                
025900 MFS-RENSA-FAELT-IN SECTION.                                              
026000                                                                          
026100*    --- ALLA INDATA-FÄLT                                                 
026200     MOVE MFS-RENSA-FAELT TO MOD-KDEKHHT-IN                               
026300                             MOD-KDEKSHT-NY                               
026310                             MOD-BEEKSHT-NY                               
026400     .                                                                    
026500     EJECT                                                                
026600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
026700                                                                          
026800*    --- ALLA UTDATA-FÄLT                                                 
026910*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
027000     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFTG-UT                               
027010                               MOD-KDEKHHT-UT                             
027100                               MOD-KDEKHHT-NY                             
027200                               MOD-BEEKHHT-NY                             
027201                               MOD-KDEKSHT-NY                             
027202                               MOD-BEEKSHT-NY                             
027203     MOVE +1 TO INDX                                                      
027204     PERFORM UNTIL INDX > MAX-INDX                                        
027205       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
027206       ADD +1 TO INDX                                                     
027207     END-PERFORM                                                          
027208     SKIP2                                                                
027209     .                                                                    
027210 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
027211                                                                          
027212*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
027213     MOVE MFS-ROER-EJ-FAELT TO MOD-KDEKSHT (INDX)                         
027216                               MOD-BEEKSHT (INDX)                         
027300     .                                                                    
027400     SKIP3                                                                
027500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
027600                                                                          
027700*    --- ALLA INDATA-FÄLT                                                 
027800     MOVE MFS-ROER-EJ-FAELT TO MOD-KDEKSHT-NY                             
027910                               MOD-BEEKSHT-NY                             
028000     .                                                                    
028100     EJECT                                                                
028200 MFS-FORM-ATTR SECTION.                                                   
028300                                                                          
028400*    --- ALLA INDATA-FÄLT                                                 
028500     MOVE MFS-FORMATETS-ATTR TO MOD-KDEKSHT-NY-ATTR                       
028600                                MOD-BEEKSHT-NY-ATTR                       
028700     .                                                                    
028800     SKIP2                                                                
028900 MFS-LAES-IN-IGEN SECTION.                                                
029000                                                                          
029100*    --- ALLA INDATA-FÄLT                                                 
029200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDEKSHT-NY-ATTR                    
029210                                   MOD-BEEKSHT-NY-ATTR                    
029400     .                                                                    
029500     EJECT                                                                
029600* --- IMS SEKTIONER ---                                                   
029700     SKIP3                                                                
029800 IMS-GET-MSG SECTION.                                                     
029900                                                                          
030000     MOVE '  QC' TO GODK-STATUSKODER                                      
030100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030300     PERFORM IMS-STATUSKONTROLL                                           
030400     .                                                                    
030500     SKIP3                                                                
030600 IMS-INSERT-MSG SECTION.                                                  
030700                                                                          
031100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031200     MOVE SPACE TO GODK-STATUSKODER                                       
031300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031500     PERFORM IMS-STATUSKONTROLL                                           
031600     .                                                                    
031701     EJECT                                                                
031702 IMS-GU-HHT SECTION.                                                      
031703                                                                          
031704     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031705          DELIMITED BY SIZE INTO SSA1                                     
031706     MOVE '  GE' TO GODK-STATUSKODER                                      
031707     CALL CBLTDLI USING GU WDH5-PCB DLI-IO-WDH501 SSA1                    
031708     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031709     PERFORM IMS-STATUSKONTROLL                                           
031710     .                                                                    
031711     EJECT                                                                
031712 IMS-GU-SHT SECTION.                                                      
031713                                                                          
031714     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031715          DELIMITED BY SIZE INTO SSA1                                     
031716     STRING 'WDH511  (KDEKSHT  =' W-KDEKSHT-X ')'                         
031717          DELIMITED BY SIZE INTO SSA2                                     
031718     MOVE '  GE' TO GODK-STATUSKODER                                      
031719     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH511 SSA1 SSA2              
031720     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031721     PERFORM IMS-STATUSKONTROLL                                           
031722     .                                                                    
031723     EJECT                                                                
031749 IMS-GNP-SHT SECTION.                                                     
031750                                                                          
031751     MOVE 'WDH511   ' TO SSA1                                             
031756     MOVE '  GE' TO GODK-STATUSKODER                                      
031757     CALL CBLTDLI USING GNP WDH5-PCB DLI-IO-WDH511 SSA1                   
031758     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031759     PERFORM IMS-STATUSKONTROLL                                           
031760     .                                                                    
031761     EJECT                                                                
031762 IMS-ISRT-SHT SECTION.                                                    
031763                                                                          
031764     STRING 'WDH501  (WDH501KY =' W-WDH501KY-X ')'                        
031765          DELIMITED BY SIZE INTO SSA1                                     
031769     MOVE 'WDH511   ' TO SSA2                                             
031770     MOVE '  II' TO GODK-STATUSKODER                                      
031771     CALL CBLTDLI USING ISRT WDH5-PCB DLI-IO-WDH511 SSA1                  
031772                                                    SSA2                  
031774     MOVE WDH5-STATUS-CODE TO STATUS-WS                                   
031775     PERFORM IMS-STATUSKONTROLL                                           
031780     .                                                                    
031800     EJECT                                                                
031900 IMS-STATUSKONTROLL SECTION.                                              
032000                                                                          
032100     SET STATUS-IX TO 1                                                   
032200     SEARCH GODK-STATUS                                                   
032300       AT END                                                             
032400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
032500         DELIMITED BY SIZE INTO FELTEXT                                   
032600         CALL FELLOG                                                      
032700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
032800         CONTINUE                                                         
032900     END-SEARCH                                                           
033000     .                                                                    

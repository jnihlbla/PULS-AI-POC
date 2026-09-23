000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6019C00.                                                
000400*AUTHOR.         ROS-MARIE CLASON  -  GUIDE DATAKONSULT AB                
000500*DATE-WRITTEN.   92/08/13.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR EN BAKGRUNDSTRANS SOM FÅR INDATA VIA FAST          
001100*        SCANNER.                                                         
001510*                                                                         
001520*                                                                         
001600*        PROGRAMMET LÄSER/UPD  W6INLA (W6D1)                              
001700*        PROGRAMMET LÄSER      W6PLAA (W6G1)                              
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002010*        U0033 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003520     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
007470     EJECT                                                                
007480                                                                          
007500 WORKING-STORAGE SECTION.                                                 
007600     SKIP2                                                                
007601                                                                          
007610*    -- CHECKED BY WY2000                                                 
007700 77  IDPGM                       PIC X(8)    VALUE 'W6019C00'.            
007800 77  JA                          PIC X       VALUE 'J'.                   
007900 77  NEJ                         PIC X       VALUE 'N'.                   
008000*77  W-KVINLART-TOT              PIC S9(8)   VALUE ZERO COMP-3.           
008010*77  W-UTPOST-RAKN               PIC S9(2)   VALUE ZERO COMP-3.           
008100     SKIP2                                                                
008200                                                                          
008300*     --- ARBETSFÄLT FÖR SWITCHAR ---                                     
008400                                                                          
008500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008600     88  NYCKLAR-OK                          VALUE 'J'.                   
008700     88  NYCKLAR-FEL                         VALUE 'N'.                   
008800                                                                          
009000 77  T91-SW                      PIC X       VALUE 'N'.                   
009100     88  T91-JA                              VALUE 'J'.                   
009200     88  T91-NEJ                             VALUE 'N'.                   
009300                                                                          
009400 77  FOERSTA-T91-SW              PIC X       VALUE 'J'.                   
009500     88  FOERSTA-T91                         VALUE 'J'.                   
010500                                                                          
010510 77  TRAEFF-SW                   PIC X       VALUE 'N'.                   
010520     88  TRAEFF                              VALUE 'J'.                   
010530                                                                          
010600 01  SPAR-AREA.                                                           
010801   03  SPAR-IDLOPNRM             PIC S9(9)    COMP-3  VALUE ZERO.         
010810   03  SPAR-PRARTSTD             PIC S9(7)V99 COMP-3  VALUE ZERO.         
010912   03  SPAR-BEFT                 PIC S9(3)    COMP-3  VALUE ZERO.         
010913   03  SPAR-KDINLSTA-OLD         PIC X(3)             VALUE SPACE.        
010914   03  SPAR-ADINLOMR-OLD         PIC X(4)             VALUE SPACE.        
010915   03  SPAR-ADINLOMR-NXT-OLD     PIC X(4)             VALUE SPACE.        
010920   03  W-PTOP1-OCC-LL            PIC S9(4)    COMP-3  VALUE ZERO.         
010921*                                                                         
010930   03  SPAR-ADLAGOMR             PIC 9(3).                                
010960   03  SPAR-ADLAGOMR-X REDEFINES SPAR-ADLAGOMR.                           
010970       05  FILLER                PIC X(1).                                
010980       05  SPAR-ADINLOMR         PIC X(2).                                
011000*                                                                         
011100   03  TORG-ADINLOMR             PIC X(4)   VALUE SPACE.                  
011200   03  SPAR-ADGANG               PIC S9(3)  VALUE +0.                     
011300   03  SPAR-ADPLATS              PIC S9(5)  VALUE +0.                     
012000                                                                          
013500*INDEX                                                                    
013600 01  FILLER                      PIC X(16)   VALUE 'INDEX '.              
013700 01  T91-IX                      PIC S9(4)   VALUE +0  COMP SYNC.         
013710 01  T91-MAX                     PIC S9(4)   VALUE +24 COMP SYNC.         
013800     EJECT                                                                
013900                                                                          
013920 01  FELTEXT.                                                             
013930     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013940     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013950 01  RKOD-ABEND                  PIC S9(4)   VALUE +33 COMP SYNC.         
013960                                                                          
014000 01  DYNAMISKA-SUBPROGRAM.                                                
014100*                                                                         
014200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
014400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014410     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
014420     03  W6019100                PIC X(8)    VALUE 'W6019100'.            
014700     EJECT                                                                
014800                                                                          
014801*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014802*                                                                         
014803 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014804     SKIP3                                                                
014805*01  MID -COPY W6I19C02                                                   
014806     EJECT                                                                
014807 01  FILLER                      PIC X(16)  VALUE 'MSG-AREA'.             
014808     SKIP3                                                                
014809*01  -COPY WMSGAREA                                                       
014810     EJECT                                                                
014811                                                                          
014900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015000 01  NYCKLAR-TILL-DLI.                                                    
015100*INLA                                                                     
015110*C-INDEX NYCKLAR                                                          
015200     03  W-W6D1C1KY-X.                                                    
015300         05  WC-IDLEVNR           PIC X(5)    VALUE SPACE.                
015400         05  WC-IDOKOLLI          PIC 9(9).                               
015800                                                                          
015801*C-INDEX SÖKNYCKLAR                                                       
015802     03  WCS-IDLEVNR-X.                                                   
015803         05  WCS-IDLEVNR          PIC X(5)    VALUE SPACE.                
015804                                                                          
015805     03  WCS-IDOKOLLI-X.                                                  
015806         05  WCS-IDOKOLLI          PIC 9(9).                              
015807*PLAA                                                                     
015830     03  W-W6GXKEY-X.                                                     
015840         05  W-ADINLOMR          PIC X(4)    VALUE SPACE.                 
015850         05  W-LOW-VALUE         PIC X(1)    VALUE LOW-VALUE.             
015860                                                                          
015861     03  W-W6GX6005-X.                                                    
015862         05  FILLER              PIC X(4)    VALUE '6005'.                
015863         05  W-W6GXKEY-6005-IDDC PIC X(2)    VALUE '11'.                  
015864         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
015900                                                                          
016000     03  W-ADINLOMR-PAR          PIC X(4).                                
016210                                                                          
016220     03  W-IDDC                  PIC X(2)     VALUE '11'.                 
016230                                                                          
016300     EJECT                                                                
016310*      --- VALID IDDC CODES                                               
016320*                                                                         
016330*01    -COPY WWDCKONS                                                     
016340       EJECT                                                              
016400                                                                          
016500*    --- STATUS-KOD FRÅN IMS                                              
016600 01  STATUS-WS                   PIC XX.                                  
016700     88  SEGMENT-FINNS                       VALUE '  '.                  
016800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017000     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017100     88  IMS-EJ-OK                           VALUE 'XD'.                  
017200     SKIP2                                                                
017300                                                                          
017400 01  GODK-STATUSKODER.                                                    
017500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017600     SKIP3                                                                
017700 01  SSA1                        PIC X(128).                              
017800 01  SSA2                        PIC X(64).                               
017900 01  SSA3                        PIC X(64).                               
018000     EJECT                                                                
018100                                                                          
018200*    --- AREOR FÖR BAKGRUNDS MPP:ER / BMP:ER                              
018300*    --- COPYTEXTER FÖR W60191                                            
018500*                                                                         
018600 01  P-TO-P-T91.                                                          
018700*----TILL W60191                                                          
018800     03  PTOP1-LL              PIC S9(4)   COMP SYNC.                     
018900     03  PTOP1-Z1              PIC X(1)    VALUE LOW-VALUE.               
019000     03  PTOP1-Z2              PIC X(1)    VALUE LOW-VALUE.               
019100     03  PTOP1-TRANSKOD        PIC X(7)    VALUE 'W6T191X'.               
019200     03  FILLER                PIC X(1)    VALUE SPACE.                   
019300     03  PTOP1-IDTRANS         PIC X(4)    VALUE '6161'.                  
019400     03  PTOP1-KDMFSFOR        PIC X(1).                                  
019500*    03  MID -COPY W6I19101       -PRE T91-                               
019600     EJECT                                                                
019700*                                                                         
019710     SKIP3                                                                
019800 01  FILLER                      PIC X(16)  VALUE 'KOM-IO-AREA'.          
019900     SKIP3                                                                
020000*01  -COPY WMSGKOM                                                        
020100     EJECT                                                                
020800                                                                          
021400*    --- IMS FUNKTIONSKODER                                               
021500*01  -COPY W0003                                                          
021600     EJECT                                                                
021700                                                                          
021800*    ---  DLI INPUT-OUTPUT AREA                                           
021900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
022000     SKIP3                                                                
023100                                                                          
023110 01  DLI-IO-AREA.                                                         
023120     03  IO-AREA               PIC X(150)  VALUE SPACE.                   
023121                                                                          
023130     SKIP3                                                                
023200     03  W6INLA01 REDEFINES IO-AREA.                                      
023300*        05  -COPY W6D101                                                 
023400     EJECT                                                                
023500     03  W6INLA11 REDEFINES IO-AREA.                                      
023600*        05  -COPY W6D111                                                 
023700     EJECT                                                                
023800     03  W6INLA21 REDEFINES IO-AREA.                                      
023900*        05  -COPY W6D121                                                 
024000     EJECT                                                                
024110                                                                          
024120 01  DLI-IO-AREA-PLAA.                                                    
024130     03  IO-AREA-PLAA            PIC X(150)  VALUE SPACE.                 
024140     SKIP3                                                                
024150     03  W6PLAA01 REDEFINES IO-AREA-PLAA.                                 
024160*        05  -COPY W6GX01                                                 
024170     SKIP3                                                                
024180     03  W6PLAA11 REDEFINES IO-AREA-PLAA.                                 
024181*        05  -COPY W6GX6006                                               
024182     EJECT                                                                
024190                                                                          
024197*                                                                         
024200 LINKAGE SECTION.                                                         
024300                                                                          
024400*01  -COPY W0009   -PRE MSG-                                              
024500     EJECT                                                                
024504                                                                          
024510*01  -COPY W0009   -PRE ALT1-                                             
024520     EJECT                                                                
024530                                                                          
025241*01  -COPY W0008  -PRE PLAA-                                              
025242     05  FILLER                  PIC X.                                   
025243     EJECT                                                                
025244                                                                          
025250*01  -COPY W0008  -PRE INLC-                                              
025260     05  FILLER                  PIC X.                                   
025270     EJECT                                                                
025280                                                                          
025290*01  -COPY W0008  -PRE HANA-                                              
025291     05  FILLER                  PIC X.                                   
025292     EJECT                                                                
025293                                                                          
025400 PROCEDURE DIVISION  USING MSG-PCB                                        
025501                           ALT1-PCB                                       
025520                           PLAA-PCB                                       
025700                           INLC-PCB.                                      
025800                                                                          
025900     ENTRY 'DLITCBL' USING MSG-PCB                                        
026000                           ALT1-PCB                                       
026002                           PLAA-PCB                                       
026200                           INLC-PCB.                                      
026220     EJECT                                                                
026300                                                                          
026310                                                                          
026320                                                                          
026400     PERFORM IMS-GET-MSG                                                  
026500     IF SEGMENT-FINNS                                                     
026600        PERFORM A-INIT                                                    
026710        PERFORM B-KOLLA-NYCKLAR                                           
026720        IF NYCKLAR-OK                                                     
026760           PERFORM C-BEARB                                                
026782           IF T91-JA                                                      
026783              PERFORM S50-SKICKA-W60191                                   
026784           END-IF                                                         
026790           MOVE ZERO TO RETURN-CODE                                       
026791        ELSE                                                              
026792*---------ABENDA - U0033                                                  
026794          PERFORM PGM-ABEND                                               
026795        END-IF                                                            
026796     END-IF                                                               
026799     GOBACK                                                               
027100     .                                                                    
027200     EJECT                                                                
027300*----------------------------------------------------------------*        
027400 A-INIT SECTION.                                                          
027500     SKIP2                                                                
027600                                                                          
027700     MOVE MSG-MID-OUT TO MID-W6I19C02                                     
028900     .                                                                    
029000     EJECT                                                                
029100*----------------------------------------------------------------*        
029200 B-KOLLA-NYCKLAR        SECTION.                                          
029300                                                                          
029301*----FORMELLA KONTROLLER                                                  
029302                                                                          
029303     MOVE JA                TO NYCKLAR-SW                                 
029700                                                                          
029704     INSPECT MID-IDOKOLLI      REPLACING LEADING SPACE                    
029705                               BY ZERO                                    
029706                                                                          
030300     IF MID-IDOKOLLI = ZERO OR                                            
030400        MID-IDOKOLLI NOT NUMERIC                                          
030500        MOVE NEJ            TO NYCKLAR-SW                                 
030510        MOVE 'FORMELT FEL PÅ IDOKOLLI' TO FELTEXT-STR                     
030600     END-IF                                                               
031200                                                                          
032000     .                                                                    
032100     EJECT                                                                
032200*----------------------------------------------------------------*        
032300 C-BEARB        SECTION.                                                  
032404                                                                          
032405     MOVE MID-IDLEVNR-KOLLI        TO WC-IDLEVNR                          
032406                                      WCS-IDLEVNR                         
032407     MOVE MID-IDOKOLLI             TO WC-IDOKOLLI                         
032408                                      WCS-IDOKOLLI                        
032409                                                                          
032415                                                                          
032419     PERFORM IMS-GU-INLC-D111                                             
032420     IF SEGMENT-SAKNAS                                                    
032421*-------INGEN BEHANDLING                                                  
032422        CONTINUE                                                          
032423     ELSE                                                                 
032424        MOVE ART-IDLOPNRM         TO SPAR-IDLOPNRM                        
032425        MOVE ART-PRARTSTD         TO SPAR-PRARTSTD                        
032426        MOVE ART-ADLAGOMR         TO SPAR-ADLAGOMR                        
032427        MOVE ART-ADGANG           TO SPAR-ADGANG                          
032428        MOVE ART-ADPLATS          TO SPAR-ADPLATS                         
032429        MOVE ART-BEFT             TO SPAR-BEFT                            
032432                                                                          
032433        PERFORM UNTIL SEGMENT-SAKNAS                                      
032435          PERFORM CA-LAES-BEARB-INLA                                      
032436          PERFORM IMS-GN-INLC-D111                                        
032437          PERFORM UNTIL SEGMENT-SAKNAS OR                                 
032438            (ART-IDLOPNRM NOT = SPAR-IDLOPNRM)                            
032440            PERFORM IMS-GN-INLC-D111                                      
032441          END-PERFORM                                                     
032442          IF SEGMENT-FINNS                                                
032443            MOVE ART-IDLOPNRM TO SPAR-IDLOPNRM                            
032444            MOVE ART-PRARTSTD TO SPAR-PRARTSTD                            
032445            MOVE ART-ADLAGOMR TO SPAR-ADLAGOMR                            
032446            MOVE ART-ADGANG   TO SPAR-ADGANG                              
032447            MOVE ART-ADPLATS  TO SPAR-ADPLATS                             
032448            MOVE ART-BEFT     TO SPAR-BEFT                                
032449          END-IF                                                          
032450        END-PERFORM                                                       
032451     END-IF                                                               
032452     .                                                                    
032453     EJECT                                                                
032454*----------------------------------------------------------------*        
032455 CA-LAES-BEARB-INLA SECTION.                                              
032460                                                                          
032700     MOVE NEJ                     TO T91-SW                               
032900                                                                          
033000     PERFORM IMS-GHNP-INLC-D121                                           
033200                                                                          
033422     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
033430        IF RAD-KDINLSTA = 'FPK' OR 'SAK' OR SPACE                         
033440           IF MID-IDPTYP = '001' AND (SPAR-ADLAGOMR = 21 OR               
033441                                      SPAR-ADLAGOMR = 22)                 
033442              IF RAD-FLDIVKLI = JA OR RAD-FLPRIO = JA                     
033443                MOVE RAD-ADINLOMR TO SPAR-ADINLOMR-OLD                    
033444                IF SPAR-ADLAGOMR = 10                                     
033445                  PERFORM S01-KOLLA-EV-TORG                               
033446                END-IF                                                    
033447                IF SPAR-ADINLOMR = '00' OR '10'                           
033448                  MOVE '10'         TO RAD-ADINLOMR                       
033449                ELSE                                                      
033450                  MOVE SPAR-ADINLOMR TO RAD-ADINLOMR                      
033451                END-IF                                                    
033452                IF MID-IDPTYP = '002' OR '003'                            
033453                  CONTINUE                                                
033454                ELSE                                                      
033455                  PERFORM CAA-UPD-INLA                                    
033456                END-IF                                                    
033457                PERFORM S40-TRANS-W60191                                  
033458              ELSE                                                        
033459                MOVE RAD-ADINLOMR TO SPAR-ADINLOMR-OLD                    
033460                MOVE 'HL'         TO RAD-ADINLOMR                         
033461                PERFORM CAA-UPD-INLA                                      
033462                PERFORM S40-TRANS-W60191                                  
033464              END-IF                                                      
033465           ELSE                                                           
033470              IF MID-IDPTYP = '002' OR MID-IDPTYP = '003'                 
033900                MOVE RAD-ADINLOMR TO SPAR-ADINLOMR-OLD                    
033901                IF SPAR-ADLAGOMR = 10                                     
033902                  PERFORM S01-KOLLA-EV-TORG                               
033903                END-IF                                                    
033904                IF SPAR-ADINLOMR = '00' OR '10'                           
033905                  MOVE '10'         TO RAD-ADINLOMR                       
033911                ELSE                                                      
033912                  MOVE SPAR-ADINLOMR TO RAD-ADINLOMR                      
033914                END-IF                                                    
033920                IF MID-IDPTYP = '002' OR '003'                            
033930                  CONTINUE                                                
033940                ELSE                                                      
034000                  PERFORM CAA-UPD-INLA                                    
034010                END-IF                                                    
034100                PERFORM S40-TRANS-W60191                                  
034200              ELSE                                                        
034300                 MOVE RAD-ADINLOMR TO SPAR-ADINLOMR-OLD                   
034400                 MOVE 'HL'        TO RAD-ADINLOMR                         
034500                 PERFORM CAA-UPD-INLA                                     
034600                 PERFORM S40-TRANS-W60191                                 
034700              END-IF                                                      
035800           END-IF                                                         
035924        END-IF                                                            
036700        PERFORM IMS-GHNP-INLC-D121                                        
036800     END-PERFORM                                                          
036805     .                                                                    
036806     EJECT                                                                
036807*----------------------------------------------------------------*        
036808 CAA-UPD-INLA SECTION.                                                    
036809                                                                          
036841     IF RAD-KDINLSTA = 'SAK'                                              
036842        MOVE RAD-KDINLSTA             TO SPAR-KDINLSTA-OLD                
036843        MOVE SPACE                    TO RAD-KDINLSTA                     
036844     END-IF                                                               
036845                                                                          
036846     MOVE RAD-ADINLOMR-NXT            TO SPAR-ADINLOMR-NXT-OLD            
036850     MOVE SPACE                       TO RAD-ADINLOMR-NXT                 
036860     MOVE +000                        TO RAD-IDINLVGN                     
036861                                                                          
036870     PERFORM IMS-REPL-INLC                                                
037500     .                                                                    
037600     EJECT                                                                
037601 S01-KOLLA-EV-TORG     SECTION.                                           
037602     SKIP2                                                                
037603     MOVE SPACE TO TORG-ADINLOMR                                          
037604     MOVE SPAR-ADINLOMR TO W-ADINLOMR-PAR                                 
037605     PERFORM IMS-GU-PLAA                                                  
037606     PERFORM IMS-GNP-PLAA-PAR                                             
037607     PERFORM UNTIL SEGMENT-SAKNAS OR TRAEFF                               
037608       IF 6006-ADGANG-TOM  > +0 OR                                        
037609          6006-ADPLATS-TOM > +0                                           
037610          IF 6006-ADGANG-TOM > +0                                         
037611            IF 6006-ADGANG-FOM <= SPAR-ADGANG AND                         
037612               6006-ADGANG-TOM >= SPAR-ADGANG                             
037613              MOVE 6006-ADINLOMR TO TORG-ADINLOMR                         
037614              MOVE JA                 TO TRAEFF-SW                        
037615            END-IF                                                        
037616          ELSE                                                            
037617            IF 6006-ADPLATS-FOM <= SPAR-ADPLATS AND                       
037618               6006-ADPLATS-TOM >= SPAR-ADPLATS                           
037619              MOVE 6006-ADINLOMR TO TORG-ADINLOMR                         
037620              MOVE JA                 TO TRAEFF-SW                        
037621            END-IF                                                        
037622          END-IF                                                          
037623       END-IF                                                             
037624       PERFORM IMS-GNP-PLAA-PAR                                           
037625     END-PERFORM                                                          
037626     MOVE NEJ                TO TRAEFF-SW                                 
037627     .                                                                    
037628     EJECT                                                                
037629 S40-TRANS-W60191      SECTION.                                           
037630                                                                          
037631     MOVE JA                     TO T91-SW                                
037640     IF T91-MID-KVPOST = ZERO                                             
037650        MOVE SPACE               TO T91-MID-W6I19101                      
037660        MOVE +1                  TO T91-MID-KVPOST                        
037670                                                                          
037680        MOVE 'W6019C00'          TO T91-MID-IDPGM                         
037681        MOVE WC-CDC-SE           TO T91-MID-IDDC                          
037690                                                                          
037691        MOVE SPAR-IDLOPNRM         TO T91-MID-IDLOPNRM(1)                 
037692        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(1)                 
037693        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(1)                  
037694        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(1)                
037695        MOVE +0                    TO T91-MID-KVKOLLI  (1)                
037696        MOVE 'N'                   TO T91-MID-FLINLI   (1)                
037697        MOVE SPAR-ADINLOMR-OLD     TO T91-MID-ADINLOMR-OLD(1)             
037698        IF TORG-ADINLOMR = SPACE                                          
037699          MOVE RAD-ADINLOMR          TO T91-MID-ADINLOMR-NEW(1)           
037700        ELSE                                                              
037701          MOVE TORG-ADINLOMR         TO T91-MID-ADINLOMR-NEW(1)           
037702        END-IF                                                            
037703        MOVE SPAR-ADINLOMR-NXT-OLD TO T91-MID-ADINLOMR-NXT-OLD(1)         
037704        MOVE RAD-ADINLOMR-NXT      TO T91-MID-ADINLOMR-NXT-NEW(1)         
037705        MOVE RAD-KVINLART          TO T91-MID-KVINLART-OLD(1)             
037706                                      T91-MID-KVINLART-NEW(1)             
037710        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(1)             
037720        MOVE RAD-KDINLSTA          TO T91-MID-KDINLSTA-NEW(1)             
037730        MOVE +1                    TO T91-IX                              
037740     ELSE                                                                 
037750        COMPUTE T91-IX = T91-MID-KVPOST + 1                               
037760        ADD 1                         TO T91-MID-KVPOST                   
037761                                                                          
037762        MOVE 'W6019C00'               TO T91-MID-IDPGM                    
037763        MOVE WC-CDC-SE                TO T91-MID-IDDC                     
037770                                                                          
037780        MOVE SPAR-IDLOPNRM         TO T91-MID-IDLOPNRM(T91-IX)            
037790        MOVE SPAR-PRARTSTD         TO T91-MID-PRARTSTD(T91-IX)            
037791        MOVE RAD-IDRADNR           TO T91-MID-IDRADNR(T91-IX)             
037792        MOVE RAD-KDINLPRIO         TO T91-MID-KDINLPRIO(T91-IX)           
037793        MOVE +0                    TO T91-MID-KVKOLLI  (T91-IX)           
037794        MOVE 'N'                   TO T91-MID-FLINLI   (T91-IX)           
037795        MOVE SPAR-ADINLOMR-OLD     TO T91-MID-ADINLOMR-OLD(T91-IX)        
037796        IF TORG-ADINLOMR = SPACE                                          
037797          MOVE RAD-ADINLOMR        TO T91-MID-ADINLOMR-NEW(T91-IX)        
037798        ELSE                                                              
037799          MOVE TORG-ADINLOMR       TO T91-MID-ADINLOMR-NEW(T91-IX)        
037800        END-IF                                                            
037801        MOVE SPAR-ADINLOMR-NXT-OLD                                        
037802             TO T91-MID-ADINLOMR-NXT-OLD(T91-IX)                          
037803        MOVE RAD-ADINLOMR-NXT                                             
037804             TO T91-MID-ADINLOMR-NXT-NEW(T91-IX)                          
037805        MOVE RAD-KVINLART          TO T91-MID-KVINLART-OLD(T91-IX)        
037806                                      T91-MID-KVINLART-NEW(T91-IX)        
037810        MOVE SPAR-KDINLSTA-OLD     TO T91-MID-KDINLSTA-OLD(T91-IX)        
037820        MOVE RAD-KDINLSTA          TO T91-MID-KDINLSTA-NEW(T91-IX)        
037821        IF T91-IX = T91-MAX                                               
037822           PERFORM S50-SKICKA-W60191                                      
037823           MOVE ZERO TO T91-MID-KVPOST                                    
037824                        T91-IX                                            
037825           MOVE NEJ TO T91-SW                                             
037826                       FOERSTA-T91-SW                                     
037827        END-IF                                                            
037830     END-IF                                                               
037840     .                                                                    
037850     EJECT                                                                
037860*----------------------------------------------------------------*        
037870 S50-SKICKA-W60191 SECTION.                                               
037880                                                                          
037890     COMPUTE W-PTOP1-OCC-LL = T91-MID-KVPOST * 64                         
037891     COMPUTE PTOP1-LL = W-PTOP1-OCC-LL + 35                               
037892     MOVE 1                  TO PTOP1-KDMFSFOR                            
037893                                                                          
037894     IF FOERSTA-T91                                                       
037895       PERFORM IMS-ISRT-MSG-ALT1-6191                                     
037896     ELSE                                                                 
037897       PERFORM IMS-PURG-MSG-ALT1-6191                                     
037898     END-IF                                                               
037899     .                                                                    
037900     EJECT                                                                
056400*----------------------------------------------------------------*        
056500* --- IMS SEKTIONER ---                                                   
056600*----------------------------------------------------------------*        
056610 IMS-GET-MSG SECTION.                                                     
056620                                                                          
056630     MOVE '  QC' TO GODK-STATUSKODER                                      
056640     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
056650     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056660     PERFORM IMS-STATUSKONTROLL                                           
056670     .                                                                    
056680     EJECT                                                                
056700******************************************************************        
056800*    ALT1-PCB  (TRANS W60191)                                   *         
056900******************************************************************        
057000*----------------------------------------------------------------*        
057100 IMS-ISRT-MSG-ALT1-6191 SECTION.                                          
057200                                                                          
057300     MOVE SPACE              TO GODK-STATUSKODER                          
057400     CALL CBLTDLI USING      ISRT ALT1-PCB                                
057500                                  P-TO-P-T91                              
057600     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
057700     PERFORM IMS-STATUSKONTROLL                                           
057800     .                                                                    
057900     EJECT                                                                
058000     SKIP3                                                                
058010*----------------------------------------------------------------*        
058020 IMS-PURG-MSG-ALT1-6191 SECTION.                                          
058030                                                                          
058040     MOVE SPACE              TO GODK-STATUSKODER                          
058050     CALL CBLTDLI USING      PURG ALT1-PCB                                
058060                                  P-TO-P-T91                              
058070     MOVE ALT1-STATUS-CODE   TO STATUS-WS                                 
058080     PERFORM IMS-STATUSKONTROLL                                           
058090     .                                                                    
058091     EJECT                                                                
058092     SKIP3                                                                
058100                                                                          
059720******************************************************************        
059800*    INLC-PCB                                                    *        
059900******************************************************************        
060000     SKIP3                                                                
060100*----------------------------------------------------------------*        
060200 IMS-GU-INLC-D111  SECTION.                                               
060300     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1C1KY-X                            
060310                    '&IDDC     =' W-IDDC ')'                              
060400             DELIMITED BY SIZE INTO SSA1                                  
060500     MOVE '  GE'                 TO GODK-STATUSKODER                      
060600     CALL CBLTDLI USING GU       INLC-PCB                                 
060700                                 DLI-IO-AREA                              
060800                                 SSA1                                     
060900     MOVE INLC-STATUS-CODE       TO STATUS-WS                             
061000     PERFORM IMS-STATUSKONTROLL                                           
061100     .                                                                    
061200     EJECT                                                                
061300     SKIP3                                                                
061400*----------------------------------------------------------------*        
061500 IMS-GN-INLC-D111  SECTION.                                               
061600     STRING 'W6INLA11(W6D1CSEQ =' W-W6D1C1KY-X ')'                        
061700             DELIMITED BY SIZE INTO SSA1                                  
061800     MOVE '  GE'                 TO GODK-STATUSKODER                      
061900     CALL CBLTDLI USING GN       INLC-PCB                                 
062000                                 DLI-IO-AREA                              
062100                                 SSA1                                     
062200     MOVE INLC-STATUS-CODE       TO STATUS-WS                             
062300     PERFORM IMS-STATUSKONTROLL                                           
062400     .                                                                    
062500     EJECT                                                                
062510     SKIP3                                                                
062600*----------------------------------------------------------------*        
062700     SKIP3                                                                
062800 IMS-GHNP-INLC-D121    SECTION.                                           
062810     STRING 'W6INLA21(IDLEVNRK =' WCS-IDLEVNR-X                           
062820                    '&IDOKOLLI =' WCS-IDOKOLLI-X ')'                      
062830          DELIMITED BY SIZE INTO SSA1                                     
063300     MOVE '  GE'            TO GODK-STATUSKODER                           
063400     CALL CBLTDLI USING GHNP INLC-PCB                                     
063500                             DLI-IO-AREA                                  
063600                             SSA1                                         
063800     MOVE INLC-STATUS-CODE  TO STATUS-WS                                  
063900     PERFORM IMS-STATUSKONTROLL                                           
064000     .                                                                    
064100     EJECT                                                                
064200*----------------------------------------------------------------*        
064300     SKIP3                                                                
064400 IMS-REPL-INLC SECTION.                                                   
064500                                                                          
064600     MOVE '  '               TO GODK-STATUSKODER                          
064700     CALL CBLTDLI USING REPL INLC-PCB                                     
064800                             DLI-IO-AREA                                  
064900     MOVE INLC-STATUS-CODE   TO STATUS-WS                                 
065000     PERFORM IMS-STATUSKONTROLL                                           
065100     .                                                                    
065200     EJECT                                                                
065300     SKIP3                                                                
065400 IMS-GU-PLAA        SECTION.                                              
065500                                                                          
065510     STRING 'W6PLAA01(W6GXKEY  =' W-W6GX6005-X ')'                        
065520             DELIMITED BY SIZE INTO SSA1                                  
065530     MOVE '  GE'                 TO GODK-STATUSKODER                      
065540     CALL CBLTDLI USING GU       PLAA-PCB                                 
065550                                 DLI-IO-AREA-PLAA                         
065560                                 SSA1                                     
065570     MOVE PLAA-STATUS-CODE       TO STATUS-WS                             
065580     PERFORM IMS-STATUSKONTROLL                                           
065600     .                                                                    
065700     EJECT                                                                
065710 IMS-GNP-PLAA-PAR    SECTION.                                             
065720                                                                          
065750     STRING 'W6PLAA11(ADINLOMP =' W-ADINLOMR-PAR ')'                      
065760             DELIMITED BY SIZE INTO SSA1                                  
065770     MOVE '  GE'                 TO GODK-STATUSKODER                      
065780     CALL CBLTDLI USING GNP      PLAA-PCB                                 
065790                                 DLI-IO-AREA-PLAA                         
065791                                 SSA1                                     
065792     MOVE PLAA-STATUS-CODE       TO STATUS-WS                             
065793     PERFORM IMS-STATUSKONTROLL                                           
065794     .                                                                    
065795     EJECT                                                                
065800     SKIP3                                                                
067800*----------------------------------------------------------------*        
067900 IMS-STATUSKONTROLL SECTION.                                              
068000     SKIP2                                                                
068100     SET STATUS-IX TO 1                                                   
068200     SEARCH GODK-STATUS                                                   
068300       AT END                                                             
068400         MOVE 'STATUS-KOD FRÅN IMS' TO FELTEXT-STR                        
068600         CALL FELLOG                                                      
068700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
068800     END-SEARCH                                                           
068900     .                                                                    
068910*----------------------------------------------------------------*        
068920 PGM-ABEND  SECTION.                                                      
068930     SKIP2                                                                
068990     CALL ABEND USING RKOD-ABEND                                          
068993     .                                                                    
071440*----------------------------------------------------------------*        

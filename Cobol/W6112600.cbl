000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6112600.                                                
000400*AUTHOR.         MÅNS SAMUELSSON.                                         
000500*DATE-WRITTEN.   92/08/07.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER W6D1 OCH SKRIVER FIL MED DENNA VECKANS                     
001100*        R31,310 POSTER OCH AVSLUTADE PARTIER DENNA VECKAN.               
001200*                                                                         
001300*        PROGRAMMET LÄSER      W6INLA (W6D1)                              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- FIL MED DENNA VECKANS R31,310, R32                         
002800     SELECT W61126                     ASSIGN TO W61126D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W61126                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  POST -COPY W6112601 -PRE  UT-  -L.                                   
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W6112600'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500     SKIP2                                                                
004600 01  FL-MAKULERAD                PIC X       VALUE 'N'.                   
004700 01  FL-RETUR                    PIC X       VALUE 'N'.                   
004800 01  FL-KVALITETSFEL             PIC X       VALUE 'N'.                   
004900*                                                                         
005000 01  FL-FOERSTA-ARTIKELN         PIC X       VALUE 'J'.                   
005100     EJECT                                                                
005200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005300 01  FILLER REDEFINES DAGENS-DATUM.                                       
005400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005700 01  IDAG.                                                                
005800     03  IDAG-TIAA               PIC 9(2).                                
005900     03  IDAG-TIVV               PIC 9(2).                                
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006800     SKIP2                                                                
006900*    --- PARAMETRAR TILL ABEND                                            
007000                                                                          
007100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  FELTEXT.                                                             
007500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200*01  -COPY WDATAREA                                                       
008300     EJECT                                                                
008400 01  FILLER                      PIC X(16) VALUE 'SPAR-AREOR'.            
008500     SKIP2                                                                
008600 01  SPAR-KVINLART-RAPP          PIC S9(7) COMP-3 VALUE +0.               
008700 01  SPAR-KVKOLLI                PIC S9(5) COMP-3 VALUE +0.               
008800 01  SPAR-KVKOLLI-RAPP           PIC S9(5) COMP-3 VALUE +0.               
008900 01  SPAR-KVINLART-RETUR         PIC S9(7) COMP-3 VALUE +0.               
008910 01  SPAR-ART-KVAVIS             PIC S9(7) COMP-3 VALUE +0.               
008920 01  SPAR-ART-FLKLAR             PIC  X(1).                               
008930 01  SPAR-UT-KDINL               PIC  X(3).                               
008940     EJECT                                                                
009400 01  UT-AREA-START               PIC X(24)   VALUE                        
009500                                 'UT-AREA-START  '.                       
009600     SKIP2                                                                
009700                                                                          
009800*01  AREA -COPY W6112601   -PRE UT-                                       
009900     EJECT                                                                
009910 01  SPAR-INLA01-AREA            PIC X(24)   VALUE                        
009920                                 'SPAR-INLA01    '.                       
009930     SKIP2                                                                
009940                                                                          
009950*01  AREA -COPY W6D101     -PRE SPAR-INLA-                                
009960     EJECT                                                                
010000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010100*                                                                         
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400     SKIP3                                                                
010500*    --- STATUS-KOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010900     SKIP2                                                                
011000 01  GODK-STATUSKODER.                                                    
011100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011200     SKIP3                                                                
011300 01  SSA1                        PIC X(64).                               
011400 01  SSA2                        PIC X(64).                               
011500     EJECT                                                                
011600*    --- IMS FUNKTIONSKODER                                               
011700*01  -COPY W0003                                                          
011800     EJECT                                                                
011900*    ---  DLI INPUT-OUTPUT AREA                                           
012000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012100     SKIP3                                                                
012200 01  DLI-IO-AREA.                                                         
012300     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
012400     SKIP3                                                                
012500     03  W6INLA01 REDEFINES IO-AREA.                                      
012600*        05  -COPY W6D101  -PRE INLA-                                     
012700     SKIP3                                                                
012800     03  W6INLA11 REDEFINES IO-AREA.                                      
012900*        05  -COPY W6D111  -PRE INLA-                                     
013000     SKIP3                                                                
013100     03  W6INLA21 REDEFINES IO-AREA.                                      
013200*        05  -COPY W6D121  -PRE INLA-                                     
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500                                                                          
013600     EJECT                                                                
013700*01  -COPY W0008  -PRE INLA-                                              
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING INLA-PCB.                                      
014100     ENTRY 'DLITCBL' USING INLA-PCB.                                      
014200                                                                          
014300     SKIP2                                                                
014400     PERFORM A-INIT                                                       
014500                                                                          
014600     PERFORM IMS-GET-INLA                                                 
014700                                                                          
014800     PERFORM UNTIL SEGMENT-SLUT                                           
014900                                                                          
015000       EVALUATE INLA-SEG-NAME-FB                                          
015100         WHEN 'W6D101'                                                    
015900           PERFORM B-FLYTTA-01-ELEMENT-SPAR                               
016000         WHEN 'W6D111'                                                    
016001           IF FL-FOERSTA-ARTIKELN = JA                                    
016002             MOVE NEJ TO FL-FOERSTA-ARTIKELN                              
016003           ELSE                                                           
016004             IF SPAR-UT-KDINL = 'R31' OR '310'                            
016005               PERFORM E-REDIGERA-OCH-SKRIV-W61126                        
016006             END-IF                                                       
016008           END-IF                                                         
016009           PERFORM BA-FLYTTA-01-ELEMENT-UT                                
016010           PERFORM S01-NOLLSTAELL-FLAGGOR                                 
016011           PERFORM C-FLYTTA-11-ELEMENT                                    
016012           MOVE INLA-ART-FLKLAR TO SPAR-ART-FLKLAR                        
016020           MOVE INLA-ART-KVAVIS TO SPAR-ART-KVAVIS                        
016200         WHEN 'W6D121'                                                    
016300           PERFORM D-ACC-DELRAPPORTERINGAR                                
016400       END-EVALUATE                                                       
016500                                                                          
016600       PERFORM IMS-GET-INLA                                               
016700     END-PERFORM                                                          
016800                                                                          
016900     IF SPAR-UT-KDINL = 'R31' OR '310'                                    
017000       PERFORM E-REDIGERA-OCH-SKRIV-W61126                                
017100     END-IF                                                               
017200                                                                          
017300     PERFORM Z-FINIT                                                      
017400                                                                          
017500     MOVE ZERO TO RETURN-CODE                                             
017600     GOBACK                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 A-INIT SECTION.                                                          
018000                                                                          
018100     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
018200                                                                          
018300     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
018400                     DAT-O-TIDATUM DAT-KDSVAR                             
018500                                                                          
018600     IF DAT-KDSVAR-OK                                                     
018700       CONTINUE                                                           
018900     ELSE                                                                 
019000       MOVE 'FEL I DATKONV' TO FELTEXT-STR                                
019100       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
019200     END-IF                                                               
019600                                                                          
019700     OPEN OUTPUT W61126                                                   
019800     SKIP2                                                                
019900     ACCEPT DAGENS-DATUM  FROM DATE                                       
020000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020100     .                                                                    
020200     EJECT                                                                
031110 B-FLYTTA-01-ELEMENT-SPAR  SECTION.                                       
031120                                                                          
031195     MOVE INLA-INL-W6D101       TO SPAR-INLA-INL-W6D101                   
031256     .                                                                    
031257     EJECT                                                                
031258 BA-FLYTTA-01-ELEMENT-UT SECTION.                                         
031259                                                                          
031260     MOVE SPAR-INLA-INL-IDDC         TO UT-IDDC                           
031261     MOVE SPAR-INLA-INL-IDLEVNR      TO UT-IDLEVNR-INL                    
031262     MOVE SPAR-INLA-INL-TIAVIDAT     TO UT-TIAVIDAT                       
031263     MOVE SPAR-INLA-INL-IDFTG        TO UT-IDFTG                          
031264     MOVE SPAR-INLA-INL-IDKONTO      TO UT-IDKONTO                        
031266     MOVE SPAR-INLA-INL-IDANALYS     TO UT-IDANALYS                       
031267     MOVE SPAR-INLA-INL-IDKST        TO UT-IDKST                          
031268     MOVE SPAR-INLA-INL-IDFS         TO UT-IDFS                           
031269     MOVE SPAR-INLA-INL-KDINL        TO SPAR-UT-KDINL                     
031270     .                                                                    
031271     EJECT                                                                
031272 C-FLYTTA-11-ELEMENT    SECTION.                                          
031273                                                                          
031274     MOVE INLA-ART-IDLOPNRM     TO UT-IDLOPNRM                            
031275     IF INLA-ART-FLKLAR = JA                                              
031276       MOVE 'R32'               TO UT-IDPTYP                              
031277     ELSE                                                                 
031278       MOVE SPAR-UT-KDINL       TO UT-IDPTYP                              
031279     END-IF                                                               
031280     MOVE INLA-ART-IDARTNR      TO UT-IDARTNR                             
031281     MOVE INLA-ART-KDRT         TO UT-KDRT                                
031282     MOVE INLA-ART-KVAVIS       TO UT-KVAVIS                              
031283                                                                          
031284     MOVE INLA-ART-ADLAGOMR     TO UT-ADLAGOMR                            
031285     MOVE INLA-ART-BEFT         TO UT-BEFT                                
031286     MOVE INLA-ART-PRARTSTD     TO UT-PRARTSTD                            
031290     .                                                                    
031291     EJECT                                                                
031292 D-ACC-DELRAPPORTERINGAR   SECTION.                                       
031293                                                                          
031294     EVALUATE INLA-RAD-KDINLSTA                                           
031295       WHEN 'INL'                                                         
031296         IF INLA-RAD-IDOKOLLI > +0                                        
031297          ADD +1                TO SPAR-KVKOLLI                           
031298                                   SPAR-KVKOLLI-RAPP                      
031299         END-IF                                                           
031300         ADD INLA-RAD-KVINLART TO SPAR-KVINLART-RAPP                      
031301       WHEN 'VOR'                                                         
031302         ADD INLA-RAD-KVINLART TO SPAR-KVINLART-RAPP                      
031303       WHEN '   '                                                         
031304         IF INLA-RAD-IDOKOLLI > +0                                        
031305           ADD +1                 TO SPAR-KVKOLLI                         
031306         END-IF                                                           
031307       WHEN 'FPK'                                                         
031308         IF INLA-RAD-IDOKOLLI > +0                                        
031309           ADD +1                 TO SPAR-KVKOLLI                         
031310         END-IF                                                           
031311       WHEN 'SAK'                                                         
031312         IF INLA-RAD-IDOKOLLI > +0                                        
031313           ADD +1                 TO SPAR-KVKOLLI                         
031314         END-IF                                                           
031315       WHEN 'KVA'                                                         
031316         MOVE JA                TO FL-KVALITETSFEL                        
031317       WHEN 'RET'                                                         
031318         MOVE JA                TO FL-RETUR                               
031319         ADD INLA-RAD-KVINLART  TO SPAR-KVINLART-RETUR                    
031320       WHEN 'MAK'                                                         
031321         MOVE JA                TO FL-MAKULERAD                           
031322     END-EVALUATE                                                         
031323     .                                                                    
031324     EJECT                                                                
031325 E-REDIGERA-OCH-SKRIV-W61126 SECTION.                                     
031326                                                                          
031327     MOVE DAT-TIAAVVD           TO UT-TIAAVVD                             
031329     MOVE DAT-TIRP              TO UT-TIPP                                
031330     MOVE SPAR-KVINLART-RAPP    TO UT-KVANTMOT                            
031331     IF SPAR-ART-FLKLAR = JA                                              
031332       IF SPAR-ART-KVAVIS NOT = SPAR-KVINLART-RAPP                        
031333         IF FL-MAKULERAD = JA                                             
031334           MOVE +2              TO UT-KDAVVANT                            
031335         ELSE                                                             
031336           MOVE +1              TO UT-KDAVVANT                            
031337         END-IF                                                           
031338       ELSE                                                               
031339         MOVE +0                TO UT-KDAVVANT                            
031340       END-IF                                                             
031341     ELSE                                                                 
031342       MOVE +0                  TO UT-KDAVVANT                            
031343     END-IF                                                               
031344     IF FL-KVALITETSFEL = JA                                              
031345       IF FL-RETUR = JA                                                   
031346         MOVE +2                TO UT-KDAVVKV                             
031347       ELSE                                                               
031348         MOVE +1                TO UT-KDAVVKV                             
031349       END-IF                                                             
031350     ELSE                                                                 
031351       MOVE +0                  TO UT-KDAVVKV                             
031352     END-IF                                                               
031353                                                                          
031354     MOVE SPAR-KVINLART-RETUR   TO UT-KVRETUR                             
031355                                                                          
031356     MOVE +0                    TO UT-KDFORP                              
031357                                   UT-KVPB-SEP                            
031358                                   UT-KVPB-SATS                           
031359                                   UT-KDEMBKOD-0                          
031360                                   UT-KDEMBKOD-1                          
031361                                   UT-KDEMBKOD-2                          
031362                                   UT-KVQPACK-0                           
031363                                   UT-KVQPACK-1                           
031364                                   UT-KVQPACK-2                           
031365     MOVE SPAR-KVKOLLI          TO UT-KVKOLLI                             
031366     MOVE SPAR-KVKOLLI-RAPP     TO UT-KVKOLLI-RAPP                        
031367     MOVE SPAR-KVINLART-RAPP    TO UT-KVRAPP                              
031368                                                                          
031369     PERFORM S11-SKRIV-W61126                                             
031370     .                                                                    
031371     EJECT                                                                
031372 Z-FINIT SECTION.                                                         
031380     CLOSE W61126                                                         
031400     SKIP2                                                                
031500     MOVE 'S' TO POSTSUM-OPKOD                                            
031600     CALL POSTSUM USING POSTSUM-PARM                                      
031700     .                                                                    
031800     EJECT                                                                
031810 S01-NOLLSTAELL-FLAGGOR SECTION.                                          
031820                                                                          
031830     MOVE NEJ                   TO FL-KVALITETSFEL                        
031840                                   FL-RETUR                               
031850                                   FL-MAKULERAD                           
031860                                                                          
031870     MOVE +0                    TO SPAR-KVINLART-RAPP                     
031880                                   SPAR-KVKOLLI                           
031890                                   SPAR-KVKOLLI-RAPP                      
031891                                   SPAR-KVINLART-RETUR                    
031892     .                                                                    
031893     EJECT                                                                
031900 S11-SKRIV-W61126 SECTION.                                                
032000     SKIP2                                                                
032100     WRITE UT-POST FROM UT-AREA                                           
032200                                                                          
032300     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
032400     MOVE 'W61126' TO POSTSUM-FDNAMN                                      
032500     MOVE 'W61126D1' TO POSTSUM-DDNAMN2                                   
032600     CALL POSTSUM USING POSTSUM-PARM                                      
032700     .                                                                    
032800     EJECT                                                                
032900* --- IMS SEKTIONER ---                                                   
033000     SKIP3                                                                
033100 IMS-GET-INLA   SECTION.                                                  
033200     SKIP2                                                                
033300     CALL CBLTDLI USING GN INLA-PCB DLI-IO-AREA                           
033400     MOVE INLA-STATUS-CODE TO STATUS-WS                                   
033500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
033600     PERFORM IMS-STATUSKONTROLL                                           
033700     .                                                                    
033800     EJECT                                                                
033900 IMS-STATUSKONTROLL SECTION.                                              
034000     SKIP2                                                                
034100     SET STATUS-IX TO 1                                                   
034200     SEARCH GODK-STATUS                                                   
034300       AT END                                                             
034400         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
034500         DISPLAY FELTEXT                                                  
034600         CALL FELLOG                                                      
034700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034800         CONTINUE                                                         
034900     END-SEARCH                                                           
035000     .                                                                    

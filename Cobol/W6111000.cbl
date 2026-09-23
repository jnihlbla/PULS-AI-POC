000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6111000.                                                
000400*AUTHOR.         BERT ANDERSSON.                                          
000500*DATE-WRITTEN.   92/03/20.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET ÄR EN SB SOM LÄSER IGENOM W6D1 OCH                    
001100*        SKRIVER UT FILER.                                                
001400*                                                                         
001500*        PROGRAMMET LÄSER      W6INLA (W6D1)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800                                                                          
002900     SELECT W61110                     ASSIGN TO W61110D1.                
003000     SELECT W61120                     ASSIGN TO W61110D2.                
003110     SELECT W61130                     ASSIGN TO W61110D3.                
003200                                                                          
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600                                                                          
003700 FD  W61110                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  POST -COPY W6111001 -PRE  UT10-  -L                                  
004200                                                                          
004900 FD  W61120                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  POST -COPY W6112001 -PRE  UT20-  -L                                  
005400                                                                          
005410 FD  W61130                                                               
005420     RECORDING       V                                                    
005430     BLOCK CONTAINS  0.                                                   
005440                                                                          
005450*01  POST -COPY W6113001 -PRE  UT30-001-  -L                              
005451*01  POST -COPY W6113002 -PRE  UT30-002-  -L                              
005460                                                                          
005500 WORKING-STORAGE SECTION.                                                 
005600     SKIP2                                                                
005601                                                                          
005610*    -- CHECKED BY WY2000                                                 
005700 77  IDPGM                       PIC X(8)    VALUE 'W6111000'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000     SKIP2                                                                
006100 01  SPAR-TIINLMOT               PIC S9(7)   COMP-3.                      
006200     SKIP2                                                                
006210 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006220 01  DAGENS-TID.                                                          
006230     03  DAGENS-TIHHMMSS         PIC 9(6)    VALUE ZERO.                  
006240     03  FILLER                  PIC 9(2)    VALUE ZERO.                  
006250                                                                          
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400*                                                                         
006500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006810     03  W611STYR                PIC X(8)    VALUE 'W611STYR'.            
006900     SKIP2                                                                
007000*    --- PARAMETRAR TILL ABEND                                            
007100                                                                          
007200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007400     SKIP2                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008210*    --- PARAMETRAR TILL W611STYR                                         
008220*                                                                         
008230*01  -COPY W611STYR                                                       
008240     EJECT                                                                
008300 01  UT10-AREA-START             PIC X(24)   VALUE                        
008400                                 'UT10-AREA-START  '.                     
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W6111001   -PRE UT10-                                     
008800     EJECT                                                                
009500 01  UT20-AREA-START             PIC X(24)   VALUE                        
009600                                 'UT20-AREA-START  '.                     
009700     SKIP2                                                                
009800                                                                          
009900*01  AREA -COPY W6112001   -PRE UT20-                                     
010000     EJECT                                                                
010100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010200*                                                                         
010300     EJECT                                                                
010301 01  UT30-AREA-START             PIC X(24)   VALUE                        
010302                                 'UT30-AREA-START  '.                     
010303     SKIP2                                                                
010310*01  -COPY W6113001   -PRE UT30-                                          
010320     EJECT                                                                
010360*01  -COPY W6113002   -PRE UT30-                                          
010370     EJECT                                                                
010380*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010390*                                                                         
010391     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500     SKIP3                                                                
010600 01  NYCKLAR-TILL-DLI.                                                    
010700     03  W-W6D101KY-X.                                                    
010800         05  W-W6D101KY          PIC S9(18)   VALUE ZERO COMP-3.          
010900     03  W-IDARTNR-X.                                                     
011000         05  W-IDARTNR           PIC S9(05)   VALUE ZERO COMP-3.          
011100     03  W-IDRADNR-X.                                                     
011200         05  W-IDRADNR           PIC S9(03)   VALUE ZERO COMP-3.          
011300     SKIP2                                                                
011400*    --- STATUS-KOD FRÅN IMS                                              
011500 01  STATUS-WS                   PIC XX.                                  
011600     88  SEGMENT-FINNS                       VALUE '  '.                  
011700     88  END-OF-DATA                         VALUE 'GB'.                  
011800     SKIP2                                                                
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200*    --- IMS FUNKTIONSKODER                                               
012300*01  -COPY W0003                                                          
012400     EJECT                                                                
012500*    ---  DLI INPUT-OUTPUT AREA                                           
012600 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012700     SKIP3                                                                
012800 01  DLI-IO-AREA.                                                         
012900     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
013000     SKIP3                                                                
013100     03  W6INLA01 REDEFINES IO-AREA.                                      
013200*        05  -COPY W6D101                                                 
013300     SKIP3                                                                
013400     03  W6INLA11 REDEFINES IO-AREA.                                      
013500*        05  -COPY W6D111                                                 
013600     SKIP3                                                                
013700     03  W6INLA21 REDEFINES IO-AREA.                                      
013800*        05  -COPY W6D121                                                 
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100                                                                          
014200     EJECT                                                                
014300*01  -COPY W0008  -PRE W6D1-                                              
014400     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014510*01  -COPY W0008  -PRE STYR-HANB-                                         
014520     05  FILLER                  PIC X.                                   
014530     EJECT                                                                
014540*01  -COPY W0008  -PRE STYR-PLAA-                                         
014550     05  FILLER                  PIC X.                                   
014560     EJECT                                                                
014600 PROCEDURE DIVISION  USING W6D1-PCB STYR-HANB-PCB STYR-PLAA-PCB.          
014700     ENTRY 'DLITCBL' USING W6D1-PCB STYR-HANB-PCB STYR-PLAA-PCB.          
014800                                                                          
014900     PERFORM A-INIT                                                       
015000     PERFORM  IMS-GN-W6D1                                                 
015100     PERFORM UNTIL END-OF-DATA                                            
015200       EVALUATE W6D1-SEG-NAME-FB                                          
015300         WHEN 'W6D101'                                                    
015310           MOVE INL-TIINLMOT TO SPAR-TIINLMOT                             
015400           PERFORM B-W6D101-SKAPA-UT10                                    
015500           PERFORM E-W6D101-SKAPA-UT20                                    
015600                                                                          
015700         WHEN 'W6D111'                                                    
015800           PERFORM C-W6D111-SKAPA-UT10                                    
015900           IF SPAR-TIINLMOT = +0                                          
016000             PERFORM F-W6D111-SKAPA-UT20                                  
016100           END-IF                                                         
016200                                                                          
016310           PERFORM G-W6D111-SKAPA-UT30                                    
016400                                                                          
016500         WHEN 'W6D121'                                                    
016600           PERFORM D-W6D121-SKAPA-UT10                                    
016700           PERFORM S11-SKRIV-W61110                                       
016710           IF SPAR-TIINLMOT > +0 AND                                      
016720             (RAD-KDINLSTA = SPACE OR 'FPK')                              
016730             PERFORM H-W6D121-SKAPA-UT30                                  
016740             PERFORM S41-SKRIV-W61130-002                                 
016750           END-IF                                                         
016800                                                                          
016900       END-EVALUATE                                                       
017000       PERFORM  IMS-GN-W6D1                                               
017100     END-PERFORM                                                          
017200                                                                          
017300     PERFORM Z-FINIT                                                      
017400                                                                          
017500     MOVE ZERO TO RETURN-CODE                                             
017600     GOBACK                                                               
017700     .                                                                    
017800     SKIP2                                                                
017900 A-INIT SECTION.                                                          
018000                                                                          
018100     OPEN OUTPUT W61110                                                   
018300                 W61120                                                   
018310                 W61130                                                   
018400                                                                          
018500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018501                                                                          
018502     ACCEPT DAGENS-DATUM FROM DATE                                        
018503     ACCEPT DAGENS-TID   FROM TIME                                        
018504                                                                          
018505     MOVE '001'           TO UT30-001-IDPTYP                              
018506     MOVE DAGENS-DATUM    TO UT30-001-TIAAMMDD                            
018507     MOVE DAGENS-TIHHMMSS TO UT30-001-TIHHMMSS                            
018508                                                                          
018509     PERFORM S31-SKRIV-W61130-001                                         
018600     .                                                                    
018700     SKIP2                                                                
018800 B-W6D101-SKAPA-UT10    SECTION.                                          
018900                                                                          
019000     MOVE INL-FLFEL                 TO UT10-INL-FLFEL                     
019100     MOVE INL-IDDC                  TO UT10-IDDC                          
019110     MOVE INL-IDLEVNR               TO UT10-IDLEVNR                       
019200     MOVE INL-IDFS                  TO UT10-IDFS                          
019300     MOVE INL-TIAVIDAT              TO UT10-TIAVIDAT                      
019400     MOVE INL-IDLBBET               TO UT10-IDLBBET                       
019500     MOVE INL-KDINL                 TO UT10-KDINL                         
019600     MOVE INL-TIANKDAG              TO UT10-TIANKDAG                      
019700     MOVE INL-TIINLMOT              TO UT10-TIINLMOT                      
019800     .                                                                    
019900     SKIP2                                                                
020000 C-W6D111-SKAPA-UT10    SECTION.                                          
020100                                                                          
020200     MOVE ART-IDARTNR               TO UT10-IDARTNR                       
020210     MOVE ART-IDRADNR-INL           TO UT10-IDRADNR-INL                   
020300     MOVE ART-BEFT                  TO UT10-BEFT                          
020400     MOVE ART-FLETIKETT             TO UT10-FLETIKETT                     
020500     MOVE ART-FLFEL                 TO UT10-ART-FLFEL                     
020600     MOVE ART-FLKLAR                TO UT10-FLKLAR                        
020610     MOVE ART-FLANNULL              TO UT10-FLANNULL                      
020700     MOVE ART-FLKVAFEL              TO UT10-FLKVAFEL                      
020800     MOVE ART-FLKVAKAR              TO UT10-FLKVAKAR                      
020900     MOVE ART-IDLOPNRM              TO UT10-IDLOPNRM                      
021000     MOVE ART-KDINLPRIO             TO UT10-ART-KDINLPRIO                 
021100     MOVE ART-KDKVAANT              TO UT10-KDKVAANT                      
021200     MOVE ART-KDLAGEMB              TO UT10-KDLAGEMB                      
021300     MOVE ART-KDRT                  TO UT10-KDRT                          
021400     MOVE ART-KDSORT                TO UT10-KDSORT                        
021500     MOVE ART-KVAVIS                TO UT10-KVAVIS                        
021600     MOVE ART-KVAVIS-KIT            TO UT10-KVAVIS-KIT                    
021610     MOVE ART-ADTRDEST-KIT          TO UT10-ADTRDEST-KIT                  
021700     MOVE ART-KVAVIS-PRIO           TO UT10-KVAVIS-PRIO                   
021800     MOVE ART-KVKVAPRIM-BER         TO UT10-KVKVAPRIM-BER                 
021900     MOVE ART-KVKVAPRIM-VER         TO UT10-KVKVAPRIM-VER                 
022000     MOVE ART-KVKVASEK-BER          TO UT10-KVKVASEK-BER                  
022100     MOVE ART-KVKVASEK-VER          TO UT10-KVKVASEK-VER                  
022200     MOVE ART-PRARTSTD              TO UT10-PRARTSTD                      
022300     MOVE ART-TIUPPDAT              TO UT10-ART-TIUPPDAT                  
022310     MOVE ART-FLSPLPART             TO UT10-FLSPLPART                     
022350     MOVE ART-ADTRDEST              TO UT10-ADTRDEST                      
022400     .                                                                    
022500     SKIP2                                                                
022600 D-W6D121-SKAPA-UT10    SECTION.                                          
022700                                                                          
022800     MOVE RAD-IDRADNR               TO UT10-IDRADNR                       
022900     MOVE RAD-ADINLOMR              TO UT10-ADINLOMR                      
023000     MOVE RAD-ADINLOMR-NXT          TO UT10-ADINLOMR-NXT                  
023100     MOVE RAD-FLDIVKLI              TO UT10-FLDIVKLI                      
023200     MOVE RAD-FLKVAANT              TO UT10-FLKVAANT                      
023300     MOVE RAD-FLPRIO                TO UT10-FLPRIO                        
023400     MOVE RAD-FLSATS                TO UT10-FLSATS                        
023500     MOVE RAD-FLINLFB               TO UT10-FLINLFB                       
023600     MOVE RAD-FLINLFP               TO UT10-FLINLFP                       
023700     MOVE RAD-IDANSTNR              TO UT10-IDANSTNR                      
023800     MOVE RAD-IDILIRAD              TO UT10-IDILIRAD                      
023900     MOVE RAD-IDILIST               TO UT10-IDILIST                       
024000     MOVE RAD-IDINLVGN              TO UT10-IDINLVGN                      
024100     MOVE RAD-IDLEVNR-KOLLI         TO UT10-RAD-IDLEVNR                   
024200     MOVE RAD-IDOKOLLI              TO UT10-IDOKOLLI                      
024300     MOVE RAD-KDINLPRIO             TO UT10-RAD-KDINLPRIO                 
024400     MOVE RAD-KDINLSTA              TO UT10-KDINLSTA                      
024500     MOVE RAD-KVINLART              TO UT10-KVINLART                      
024600     MOVE RAD-TIUPPDAT              TO UT10-RAD-TIUPPDAT                  
024700     .                                                                    
024800     EJECT                                                                
024900 E-W6D101-SKAPA-UT20    SECTION.                                          
025000                                                                          
025100     MOVE INL-IDDC                  TO UT20-IDDC                          
025110     MOVE INL-IDLEVNR               TO UT20-IDLEVNR                       
025200     MOVE INL-IDFS                  TO UT20-IDFS                          
025300     MOVE INL-TIAVIDAT              TO UT20-TIAVIDAT                      
025400     MOVE INL-KDINL                 TO UT20-KDINL                         
025500     MOVE INL-TIANKDAG              TO UT20-TIANKDAG                      
025700     .                                                                    
025800     SKIP2                                                                
025900 F-W6D111-SKAPA-UT20    SECTION.                                          
026000                                                                          
026010     MOVE UT20-IDDC                 TO STYR-IDDC                          
026011     MOVE UT20-IDLEVNR              TO STYR-IDLEVNR                       
026020     MOVE +0                        TO STYR-IDARTNR                       
026030                                       STYR-IDFKNGRP                      
026040                                       STYR-BEFT                          
026050                                                                          
026060     CALL W611STYR USING STYR-W611STYR STYR-HANB-PCB STYR-PLAA-PCB        
026070     IF STYR-KDSVAR-OK                                                    
026080       MOVE STYR-ADINLOMR-FB        TO UT20-ADINLOMR                      
026090     ELSE                                                                 
026091       DISPLAY 'FEL FRÅN W611STYR'                                        
026092       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
026093     END-IF                                                               
026094                                                                          
026100     MOVE ART-ADLAGOMR              TO UT20-ADLAGOMR                      
026200     MOVE ART-IDARTNR               TO UT20-IDARTNR                       
026300     MOVE ART-BEFT                  TO UT20-BEFT                          
026400     MOVE ART-FLFEL                 TO UT20-FLFEL                         
026500     MOVE ART-KVAVIS                TO UT20-KVAVIS                        
026600     PERFORM S21-SKRIV-W61120                                             
026700     .                                                                    
026800     EJECT                                                                
027640 G-W6D111-SKAPA-UT30 SECTION.                                             
027650     MOVE '002'              TO UT30-002-IDPTYP                           
027651     MOVE ART-IDDC           TO UT30-002-IDDC                             
027660     MOVE ART-IDLOPNRM       TO UT30-002-IDLOPNRM                         
027670     MOVE ART-PRARTSTD       TO UT30-002-PRARTSTD                         
027680     .                                                                    
027690 H-W6D121-SKAPA-UT30 SECTION.                                             
027691     MOVE RAD-IDRADNR        TO UT30-002-IDRADNR                          
027692     MOVE RAD-IDOKOLLI       TO UT30-002-IDOKOLLI                         
027693     MOVE RAD-KVINLART       TO UT30-002-KVINLART                         
027694     MOVE RAD-KDINLPRIO      TO UT30-002-KDINLPRIO                        
027695     IF RAD-ADINLOMR-NXT NOT = SPACE                                      
027696       MOVE RAD-ADINLOMR-NXT   TO UT30-002-ADINLOMR                       
027697     ELSE                                                                 
027698       MOVE RAD-ADINLOMR       TO UT30-002-ADINLOMR                       
027699     END-IF                                                               
027700     .                                                                    
027710 Z-FINIT SECTION.                                                         
027800                                                                          
027900     CLOSE W61110                                                         
028100           W61120                                                         
028110           W61130                                                         
028200                                                                          
028300     MOVE 'S' TO POSTSUM-OPKOD                                            
028400                                                                          
028500     CALL POSTSUM USING POSTSUM-PARM                                      
028600     .                                                                    
028700     SKIP2                                                                
028800 S11-SKRIV-W61110 SECTION.                                                
028900                                                                          
029000     WRITE UT10-POST FROM UT10-AREA                                       
029100                                                                          
029200     MOVE 'W61110' TO POSTSUM-FDNAMN                                      
029300     MOVE 'W61110D1' TO POSTSUM-DDNAMN2                                   
029400     CALL POSTSUM USING POSTSUM-PARM                                      
029500     .                                                                    
029600     SKIP2                                                                
029700 S21-SKRIV-W61120 SECTION.                                                
029800                                                                          
029900     WRITE UT20-POST FROM UT20-AREA                                       
030000                                                                          
030100     MOVE 'W61120' TO POSTSUM-FDNAMN                                      
030200     MOVE 'W61110D2' TO POSTSUM-DDNAMN2                                   
030300     CALL POSTSUM USING POSTSUM-PARM                                      
030400     .                                                                    
030500     SKIP2                                                                
031410 S31-SKRIV-W61130-001 SECTION.                                            
031420                                                                          
031422     WRITE UT30-001-POST FROM UT30-001-W6113001                           
031440                                                                          
031450     MOVE 'W61130' TO POSTSUM-FDNAMN                                      
031460     MOVE 'W61110D4' TO POSTSUM-DDNAMN2                                   
031470     CALL POSTSUM USING POSTSUM-PARM                                      
031480     .                                                                    
031490     SKIP2                                                                
031491 S41-SKRIV-W61130-002 SECTION.                                            
031492                                                                          
031496     WRITE UT30-002-POST FROM UT30-002-W6113002                           
031498                                                                          
031499     MOVE 'W61130' TO POSTSUM-FDNAMN                                      
031500     MOVE 'W61110D4' TO POSTSUM-DDNAMN2                                   
031501     CALL POSTSUM USING POSTSUM-PARM                                      
031502     .                                                                    
031503     SKIP2                                                                
031510* --- IMS SEKTIONER ---                                                   
031600     SKIP2                                                                
031700 IMS-GN-W6D1   SECTION.                                                   
031800     SKIP2                                                                
031900     MOVE '  GAGKGB'       TO GODK-STATUSKODER                            
032000     CALL CBLTDLI USING GN W6D1-PCB DLI-IO-AREA                           
032100     MOVE W6D1-STATUS-CODE TO STATUS-WS                                   
032200     PERFORM IMS-STATUSKONTROLL                                           
032300     .                                                                    
032400     SKIP2                                                                
032500 IMS-STATUSKONTROLL SECTION.                                              
032600     SKIP2                                                                
032700     SET STATUS-IX TO 1                                                   
032800     SEARCH GODK-STATUS                                                   
032900       AT END                                                             
033000         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
033100         DISPLAY FELTEXT                                                  
033200         CALL FELLOG                                                      
033300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033400         CONTINUE                                                         
033500     END-SEARCH                                                           
033600     .                                                                    

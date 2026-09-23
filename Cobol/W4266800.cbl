001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4266800.                                                
001300*AUTHOR.         ANN WESTBERG.                                            
001400*DATE-WRITTEN.   92/11/10.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001801*                                                                         
001803*        PROGRAMMET LÄSER      W6UPFA (W6L1)                              
001810*                                                                         
001900*        SKAPAR TVÅ UTFILER OM                                            
001901*        ANTALET ARBETSDAGAR ÄR STÖRRE ÄN 30,                             
001902*        OCH PARTIET ÄR R32-RAPPORTERAT.                                  
001903*        UTFILER:                                                         
001910*        1) GÅR TILL UPPFÖLJNINGSREG. PÅ HIST.FIL                         
001911*           PGM W42670                                                    
001920*        2) SAMMA LÖPNR SOM PÅ INFIL1 FÖR RENSNING                        
001930*           AV W6L1                                                       
001940*           PGM W42669                                                    
002200*                                                                         
002300*    ABENDCODES:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- SLUTRAPP INLEV. TILL UPPFÖLJN. PÅ HIST FIL                 
003403     SELECT W42668                     ASSIGN TO W42668D1.                
003404     SKIP2                                                                
003405*          --- NYCKLAR FÖR BORTTAG AV W6D2                                
003410     SELECT W42669                     ASSIGN TO W42668D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W42668                                                               
004003     RECORDING       V                                                    
004004     BLOCK CONTAINS  0.                                                   
004005     SKIP2                                                                
004006*01  POST -COPY W4266801 -PRE  6801- -L.                                  
004007     SKIP3                                                                
004008*01  POST -COPY W4266802 -PRE  6811- -L.                                  
004009     SKIP3                                                                
004010*01  POST -COPY W4266803 -PRE  6812- -L.                                  
004011     EJECT                                                                
004012 FD  W42669                                                               
004013     RECORDING       F                                                    
004014     BLOCK CONTAINS  0.                                                   
004015     SKIP2                                                                
004020*01  POST -COPY W4266901 -PRE  69-  -L.                                   
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W4266800'.            
004500 77  YES                         PIC X       VALUE 'Y'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004900     EJECT                                                                
004901 77  R32-SW                      PIC X       VALUE 'N'.                   
004910     88  R32-RAPPORTERAD                     VALUE 'Y'.                   
004920                                                                          
005000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES TODAYS-DATE.                                        
005200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005400     03  TODAYS-DATE-DAY         PIC 9(2).                                
005500     EJECT                                                                
005600 01  GENERAL-SUBPROGRAM.                                                  
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
006210     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
006300     SKIP2                                                                
006400*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006500                                                                          
006600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  ERRTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
007100     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200     EJECT                                                                
007300*    --- PARAMETERS FOR SUBPROGRAM DATKORT                                
007400*                                                                         
007500 01  PROGRAM-NAME                PIC X(6)    VALUE 'W42668'.              
007600     SKIP2                                                                
007700 01  DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.              
007800     SKIP2                                                                
007900*01  -COPY WDATKORT                                                       
008001     EJECT                                                                
008002*    --- PARAMETRAR TILL POSTSUM                                          
008003*                                                                         
008010*01  -COPY W0005   -PRE  POSTSUM-                                         
008101     EJECT                                                                
008110*01  -COPY WORKAREA                                                       
008201     EJECT                                                                
008202 01  68-AREA-START          PIC X(24)   VALUE                             
008203                                 '68-AREA-START  '.                       
008205                                                                          
008206*01  AREA -COPY W4266801     -PRE 6801-                                   
008207     EJECT                                                                
008208*01  AREA -COPY W4266802     -PRE 6811-                                   
008209     SKIP2                                                                
008210*01  AREA -COPY W4266803     -PRE 6812-                                   
008211     SKIP2                                                                
008212                                                                          
008213 01  69-AREA-START          PIC X(24)   VALUE                             
008214                                 '69-AREA-START  '.                       
008216                                                                          
008220*01  AREA -COPY W4266901     -PRE 69-                                     
008300     EJECT                                                                
008400*    --- AREAS FOR IMS-SECTIONS                                           
008500*                                                                         
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
009110*                                                                         
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FOUND                       VALUE '  '.                  
009410     88  SEGMENT-NOT-FOUND                   VALUE 'GE'.                  
009500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009600     88  SEGMENT-MISSING                     VALUE 'GB'.                  
009700     SKIP3                                                                
009701 01  NYCKLAR-TILL-DLI.                                                    
009702     03  W-W6D1BSEQ-X.                                                    
009710         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
009720     SKIP3                                                                
009800 01  GOOD-STATUSCODES.                                                    
009900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010000     SKIP3                                                                
010100 01  SSA1                        PIC X(64).                               
010200 01  SSA2                        PIC X(64).                               
010210 01  SSA3                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNCTION CODES                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010800*    ---  DLI INPUT-OUTPUT AREA                                           
010900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011000     SKIP3                                                                
011100 01  DLI-IO-AREA.                                                         
011200     03  IO-AREA                 PIC X(290)  VALUE SPACE.                 
011301     SKIP3                                                                
011302     03  W6UPFA01 REDEFINES IO-AREA.                                      
011303*        05  -COPY W6L101  -PRE UPFA-                                     
011304     SKIP3                                                                
011305     03  W6UPFA11 REDEFINES IO-AREA.                                      
011306*        05  -COPY W6L111  -PRE UPFA-                                     
011307     SKIP3                                                                
011308     03  W6UPFA12 REDEFINES IO-AREA.                                      
011309*        05  -COPY W6L112  -PRE UPFA-                                     
011600     EJECT                                                                
011610 01  DLI-IO-AREA2.                                                        
011620     03  IO-AREA2                PIC X(290)  VALUE SPACE.                 
011630     SKIP3                                                                
011640     03  W6INLA01 REDEFINES IO-AREA2.                                     
011650*        05  -COPY W6D111  -PRE INLA-                                     
011700 LINKAGE SECTION.                                                         
011800                                                                          
011901     EJECT                                                                
011902*01  -COPY W0008  -PRE UPFA-                                              
011903     05  FILLER                  PIC X.                                   
011904*01  -COPY W0008  -PRE INLABSEQ-                                          
011905     05  FILLER                  PIC X.                                   
011906     EJECT                                                                
012101 PROCEDURE DIVISION  USING UPFA-PCB INLABSEQ-PCB.                         
012110     ENTRY 'DLITCBL' USING UPFA-PCB INLABSEQ-PCB.                         
012200                                                                          
012400     SKIP2                                                                
012500     PERFORM A-INIT                                                       
012610     PERFORM IMS-GET-W6UPFA01                                             
012700     PERFORM UNTIL SEGMENT-MISSING                                        
012733         MOVE UPFA-UPPF-TIREGDAT TO WORK-TIAAMMDD-FOM                     
012735         MOVE +001 TO WORK-KDCALL                                         
012736         MOVE '11' TO WORK-IDDC                                           
012740         MOVE TODAYS-DATE        TO WORK-TIAAMMDD-TOM                     
012760         CALL WORKDAY USING WORK-KDCALL,                                  
012780                             WORK-DATE-AREA,                              
012781                             WORK-KDSVAR                                  
012791         IF WORK-KDSVAR-OK                                                
012797            IF WORK-KVWORKD > 30                                          
012798               PERFORM S01-KOLLA-OM-R32-RAPPORTERAD                       
012799               IF R32-RAPPORTERAD                                         
012803                 PERFORM S12-WRITE-W42669                                 
012804                 PERFORM S11-WRITE-W42668-01                              
012805                                                                          
012806                 PERFORM IMS-GET-W6UPFA11                                 
012807                 PERFORM UNTIL SEGMENT-NOT-FOUND                          
012808                   PERFORM S11-WRITE-W42668-11                            
012809                   PERFORM IMS-GET-W6UPFA11                               
012810                 END-PERFORM                                              
012811                                                                          
012812                 PERFORM IMS-GET-W6UPFA12                                 
012813                 PERFORM UNTIL SEGMENT-NOT-FOUND                          
012814                   PERFORM S11-WRITE-W42668-12                            
012815                   PERFORM IMS-GET-W6UPFA12                               
012816                 END-PERFORM                                              
012817               END-IF                                                     
012818            END-IF                                                        
012819         ELSE                                                             
012820           DISPLAY 'FELAKTIGT SVAR: ' WORK-KVWORKD                        
012821         END-IF                                                           
012840         PERFORM IMS-GET-W6UPFA01                                         
013500     END-PERFORM                                                          
013700                                                                          
013800     PERFORM Z-FINIT                                                      
013900                                                                          
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014601                                                                          
014602     OPEN OUTPUT W42668                                                   
014610                 W42669                                                   
014800     ACCEPT TODAYS-DATE  FROM DATE                                        
015400     .                                                                    
015500     EJECT                                                                
015510 S01-KOLLA-OM-R32-RAPPORTERAD SECTION.                                    
015511                                                                          
015512     MOVE UPFA-UPPF-IDLOPNRM TO W-IDLOPNRM                                
015513     PERFORM IMS-GU-INLBSEQ-W6D111                                        
015514     IF SEGMENT-FOUND                                                     
015515       IF INLA-ART-FLKLAR = 'J'                                           
015516         MOVE YES TO R32-SW                                               
015517       ELSE                                                               
015518         MOVE NOO TO R32-SW                                               
015519       END-IF                                                             
015520     ELSE                                                                 
015521       MOVE YES TO R32-SW                                                 
015522     END-IF                                                               
015523                                                                          
015524     .                                                                    
015525     EJECT                                                                
015526 S11-WRITE-W42668-01 SECTION.                                             
015527     SKIP2                                                                
015528     MOVE 101                     TO 6801-IDPTYP                          
015529     MOVE UPFA-UPPF-IDLOPNRM      TO 6801-IDLOPNRM                        
015530     MOVE UPFA-UPPF-ADKVAULG      TO 6801-ADKVAULG                        
015531     MOVE UPFA-UPPF-BEANST        TO 6801-BEANST                          
015532     MOVE UPFA-UPPF-FLANNULL      TO 6801-FLANNULL                        
015533     MOVE UPFA-UPPF-FLKVARED      TO 6801-FLKVARED                        
015534     MOVE UPFA-UPPF-FLKVAUTV-ANT  TO 6801-FLKVAUTV-ANT                    
015535     MOVE UPFA-UPPF-FLKVAUTV-KVAL TO 6801-FLKVAUTV-KVAL                   
015536     MOVE UPFA-UPPF-FLSKPSAK      TO 6801-FLSKPSAK                        
015537     MOVE UPFA-UPPF-IDARTNR       TO 6801-IDARTNR                         
015538     MOVE UPFA-UPPF-IDLEVNR       TO 6801-IDLEVNR                         
015539     MOVE UPFA-UPPF-IDUSER-PRI    TO 6801-IDUSER-PRI                      
015540     MOVE UPFA-UPPF-IDUSER-SEK    TO 6801-IDUSER-SEK                      
015541     MOVE UPFA-UPPF-KDKVATYP      TO 6801-KDKVATYP                        
015542     MOVE UPFA-UPPF-IDPROVPL-PRI  TO 6801-IDPROVPL-PRI                    
015543     MOVE UPFA-UPPF-IDPROVPL-SEK  TO 6801-IDPROVPL-SEK                    
015544     MOVE UPFA-UPPF-KDKVAULG      TO 6801-KDKVAULG                        
015545     MOVE UPFA-UPPF-KDKVASTA-ANT  TO 6801-KDKVASTA-ANT                    
015546     MOVE UPFA-UPPF-KDKVASTA-PRI  TO 6801-KDKVASTA-PRI                    
015547     MOVE UPFA-UPPF-KDKVASTA-SEK  TO 6801-KDKVASTA-SEK                    
015548     MOVE UPFA-UPPF-KVKVAPRIM     TO 6801-KVKVAPRIM                       
015549     MOVE UPFA-UPPF-KVKVASEK      TO 6801-KVKVASEK                        
015550     MOVE UPFA-UPPF-TIREGDAT      TO 6801-TIREGDAT                        
015551                                                                          
015552     WRITE 6801-POST FROM 6801-AREA                                       
015553     MOVE 6801-IDPTYP TO POSTSUM-TRANSTYP                                 
015554     MOVE 'W42668' TO POSTSUM-FDNAMN                                      
015555     MOVE 'W42668D1' TO POSTSUM-DDNAMN2                                   
015556     CALL POSTSUM USING POSTSUM-PARM                                      
015557     .                                                                    
015558     EJECT                                                                
015559                                                                          
015560 S11-WRITE-W42668-11 SECTION.                                             
015561     SKIP2                                                                
015562     MOVE 111                     TO 6811-IDPTYP                          
015563     MOVE UPFA-RAPP-IDKR          TO 6811-IDKR                            
015564     MOVE UPFA-RAPP-BEKRFEL(1)    TO 6811-BEKRFEL(1)                      
015565     MOVE UPFA-RAPP-BEKRFEL(2)    TO 6811-BEKRFEL(2)                      
015566     MOVE UPFA-RAPP-BEKRFEL(3)    TO 6811-BEKRFEL(3)                      
015567     MOVE UPFA-RAPP-KDKVASTA-PRI  TO 6811-KDKVASTA-PRI                    
015568     MOVE UPFA-RAPP-TEKRFEL       TO 6811-TEKRFEL                         
015569                                                                          
015570     WRITE 6811-POST FROM 6811-AREA                                       
015571     MOVE 6811-IDPTYP TO POSTSUM-TRANSTYP                                 
015572     MOVE 'W42668' TO POSTSUM-FDNAMN                                      
015573     MOVE 'W42668D1' TO POSTSUM-DDNAMN2                                   
015574     CALL POSTSUM USING POSTSUM-PARM                                      
015575     .                                                                    
015576     EJECT                                                                
015577 S11-WRITE-W42668-12 SECTION.                                             
015578                                                                          
015579     MOVE 112                     TO 6812-IDPTYP                          
015580     MOVE UPFA-SPEC-IDKVAINF      TO 6812-IDKVAINF                        
015581     MOVE UPFA-SPEC-KDKVASTA-PRI  TO 6812-KDKVASTA-PRI                    
015582     MOVE UPFA-SPEC-TEKVAINF(1)   TO 6812-TEKVAINF(1)                     
015583     MOVE UPFA-SPEC-TEKVAINF(2)   TO 6812-TEKVAINF(2)                     
015584     MOVE UPFA-SPEC-TEKVAINF(3)   TO 6812-TEKVAINF(3)                     
015585                                                                          
015586     WRITE 6812-POST FROM 6812-AREA                                       
015587     MOVE 6812-IDPTYP TO POSTSUM-TRANSTYP                                 
015588     MOVE 'W42668' TO POSTSUM-FDNAMN                                      
015589     MOVE 'W42668D1' TO POSTSUM-DDNAMN2                                   
015590     CALL POSTSUM USING POSTSUM-PARM                                      
015591     .                                                                    
015592     EJECT                                                                
015593 S12-WRITE-W42669 SECTION.                                                
015594     SKIP2                                                                
015595     MOVE UPFA-UPPF-IDLOPNRM      TO 69-IDLOPNRM                          
015596     WRITE 69-POST FROM 69-AREA                                           
015597                                                                          
015598     MOVE 'W42669' TO POSTSUM-FDNAMN                                      
015599     MOVE 'W42668D2' TO POSTSUM-DDNAMN2                                   
015600     CALL POSTSUM USING POSTSUM-PARM                                      
015601     .                                                                    
015610     EJECT                                                                
015700 Z-FINIT SECTION.                                                         
015701     CLOSE W42668                                                         
015710           W42669                                                         
015801     SKIP2                                                                
015802     MOVE 'S' TO POSTSUM-OPKOD                                            
015810     CALL POSTSUM USING POSTSUM-PARM                                      
015900     .                                                                    
016101     EJECT                                                                
017000* --- IMS SECTIONS  ---                                                   
017100     SKIP3                                                                
017202 IMS-GET-W6UPFA01 SECTION.                                                
017203     MOVE 'W6UPFA01 '  TO SSA1                                            
017207     MOVE '  GB' TO GOOD-STATUSCODES                                      
017208     CALL CBLTDLI USING GN UPFA-PCB DLI-IO-AREA SSA1                      
017209     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
017210     PERFORM IMS-STATUSCHECK                                              
017211     .                                                                    
017212     EJECT                                                                
017213 IMS-GET-W6UPFA11 SECTION.                                                
017214     MOVE 'W6UPFA11 '  TO SSA1                                            
017216     MOVE '  GE' TO GOOD-STATUSCODES                                      
017217     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA SSA1                     
017218     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
017219     PERFORM IMS-STATUSCHECK                                              
017220     .                                                                    
017221     EJECT                                                                
017222 IMS-GET-W6UPFA12 SECTION.                                                
017223     MOVE 'W6UPFA12 '  TO SSA1                                            
017224     MOVE '  GE' TO GOOD-STATUSCODES                                      
017225     CALL CBLTDLI USING GNP UPFA-PCB DLI-IO-AREA SSA1                     
017226     MOVE UPFA-STATUS-CODE TO STATUS-WS                                   
017227     PERFORM IMS-STATUSCHECK                                              
017228     .                                                                    
017229     EJECT                                                                
017230 IMS-GU-INLBSEQ-W6D111 SECTION.                                           
017231     STRING 'W6INLA11(W6D1BSEQ =' W-W6D1BSEQ-X ')'                        
017232          DELIMITED BY SIZE INTO SSA1                                     
017233     MOVE '  GE' TO GOOD-STATUSCODES                                      
017234     CALL CBLTDLI USING GU INLABSEQ-PCB DLI-IO-AREA2 SSA1                 
017235     MOVE INLABSEQ-STATUS-CODE TO STATUS-WS                               
017236     PERFORM IMS-STATUSCHECK                                              
017237     .                                                                    
017240     EJECT                                                                
017400 IMS-STATUSCHECK SECTION.                                                 
017500     SKIP2                                                                
017600     SET STATUS-IX TO 1                                                   
017700     SEARCH GOOD-STATUS                                                   
017800       AT END                                                             
017900         MOVE 'XXXXXXXXXXXXXX' TO ERRTEXT-STR                             
018000         DISPLAY ERRTEXT                                                  
018100         CALL FELLOG                                                      
018200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
018300         CONTINUE                                                         
018400     END-SEARCH                                                           
018500     .                                                                    

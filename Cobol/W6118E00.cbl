001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W6118E00.                                                
001300 AUTHOR.         UMESH JAIN.                                              
001400 DATE-WRITTEN.   09/12/15.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        TO FIND THE NEW GATE FOR THE PART NUMBERS                        
001900*                                                                         
002001*        THE PROGRAM READS     WDT2                                       
002010*        THE PROGRAM READS     WDK6                                       
002100*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- PART NUMBERS FOR WHICH NEW GATE IS TO BE FOUND             
003303     SELECT W6118D                     ASSIGN TO W6118ED1.                
003304     SKIP2                                                                
003305*          --- PART NUMBERS WITH NEW GATE                                 
003310     SELECT W6118E                     ASSIGN TO W6118ED2.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W6118D                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003906*01  -COPY W6118H01      -L.                                              
003907     SKIP3                                                                
003908 FD  W6118E                                                               
003909     RECORDING       F                                                    
003910     BLOCK CONTAINS  0.                                                   
003911                                                                          
003920*01  RECORD -COPY W6118C01 -PRE  UT-  -L.                                 
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W6118E00'.            
004400 77  YES                         PIC X       VALUE 'Y'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  W-ADINPORT-201              PIC X(08)   VALUE SPACES.                
004700 77  W-ADINPORT                  PIC X(08)   VALUE SPACES.                
004701                                                                          
004702 77  W6118D-EOF-SW               PIC X       VALUE 'N'.                   
004710     88  END-OF-W6118D                       VALUE 'J'.                   
004800     EJECT                                                                
004900 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES TODAYS-DATE.                                        
005100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005300     03  TODAYS-DATE-DAY         PIC 9(2).                                
005400     EJECT                                                                
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     SKIP2                                                                
006200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  ERROR-TEXT.                                                          
006900     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  IN-AREA-START               PIC X(24)   VALUE                        
007303                                 'IN-AREA-START  '.                       
007304     SKIP2                                                                
007305                                                                          
007306*01  AREA -COPY W6118H01     -PRE IN-                                     
007307     EJECT                                                                
007308 01  UT-AREA-START               PIC X(24)   VALUE                        
007309                                 'UT-AREA-START  '.                       
007310     SKIP2                                                                
007311                                                                          
007320*01  AREA -COPY W6118C01     -PRE UT-                                     
007400     EJECT                                                                
007500*    --- AREAS FOR IMS-SECTIONS                                           
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  KEYS-FOR-DLI.                                                        
008100     03  W-BEFT-X.                                                        
008101         05  W-BEFT              PIC 9(02)    VALUE ZERO.                 
008102     03  W-IDARTNR-X.                                                     
008103         05  W-IDARTNR           PIC S9(09)   VALUE ZERO COMP-3.          
008106     03  W-IDFKNGRF-X.                                                    
008107         05  W-IDFKNGRF          PIC S9(5)    VALUE ZERO COMP-3.          
008108     03  W-IDFKNGRT-X.                                                    
008109         05  W-IDFKNGRT          PIC S9(5)    VALUE ZERO COMP-3.          
008113                                                                          
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FOUND                       VALUE '  '.                  
008700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008800     SKIP2                                                                
008900 01  GOOD-STATUSCODES.                                                    
009000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNCTION CODES                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT201'.                      
010002 01  DLI-IO-WDT201.                                                       
010003*    03  -COPY WDT201                                                     
010004     EJECT                                                                
010005 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT211'.                      
010006 01  DLI-IO-WDT211.                                                       
010007*    03  -COPY WDT211                                                     
010008     EJECT                                                                
010009 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT212'.                      
010010 01  DLI-IO-WDT212.                                                       
010011*    03  -COPY WDT212                                                     
010012 01  FILLER         PIC X(16)   VALUE 'WDK601-11    '.                    
010013 01  DLI-IO-WDK601-11.                                                    
010014     03  DLI-IO-WDK601.                                                   
010015*        05 -COPY WDK601                                                  
010016     03  DLI-IO-WDK611.                                                   
010017*        05 -COPY WDK611                                                  
010018     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010601                                                                          
010602*01  -COPY W0008  -PRE WDT2-                                              
010603     05  FILLER                  PIC X.                                   
010604                                                                          
010605*01  -COPY W0008  -PRE WDK6-                                              
010610     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010801 PROCEDURE DIVISION  USING WDT2-PCB WDK6-PCB.                             
010802 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING WDT2-PCB WDK6-PCB.                             
011100                                                                          
011200     PERFORM A-INIT                                                       
011420                                                                          
011430     PERFORM S01-READ-W6118D                                              
011500     PERFORM UNTIL END-OF-W6118D                                          
011510       MOVE IN-BEFT    TO W-BEFT                                          
011520       MOVE IN-IDARTNR TO W-IDARTNR                                       
011600       PERFORM IMS-GU-WDT201                                              
011601       IF SEGMENT-FOUND                                                   
011604         MOVE GDC-ADINPORT TO W-ADINPORT-201                              
011620         PERFORM IMS-GNP-WDT211                                           
011630         IF SEGMENT-FOUND                                                 
011640           MOVE GLO-ADINPORT-LO TO W-ADINPORT                             
011650         ELSE                                                             
011651           PERFORM IMS-GU-WDK611-PATH                                     
011660           MOVE ART-IDFKNGRP TO W-IDFKNGRF                                
011670                                W-IDFKNGRT                                
011680           PERFORM IMS-GNP-WDT212                                         
011690           IF SEGMENT-FOUND                                               
011691             MOVE GFT-ADINPORT-FT TO W-ADINPORT                           
011692           ELSE                                                           
011693             MOVE W-ADINPORT-201   TO W-ADINPORT                          
011694           END-IF                                                         
011695         END-IF                                                           
011696*                                                                         
011698         IF CLAG-ADINPORT NOT = W-ADINPORT                                
011699           MOVE IN-IDARTNR TO UT-IDARTNR                                  
011700           MOVE W-ADINPORT TO UT-ADINPORT                                 
011701           PERFORM S11-WRITE-W6118E                                       
011702         END-IF                                                           
011703       END-IF                                                             
012210       PERFORM S01-READ-W6118D                                            
012300     END-PERFORM                                                          
012500                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013310     OPEN INPUT  W6118D                                                   
013410     OPEN OUTPUT W6118E                                                   
013600     ACCEPT TODAYS-DATE  FROM DATE                                        
013710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013900     .                                                                    
014000     EJECT                                                                
014100 Z-FINIT SECTION.                                                         
014201     CLOSE W6118D                                                         
014210           W6118E                                                         
014301     SKIP2                                                                
014302     MOVE 'S' TO POSTSUM-OPKOD                                            
014310     CALL POSTSUM USING POSTSUM-PARM                                      
014400     .                                                                    
014501     EJECT                                                                
014502 S01-READ-W6118D  SECTION.                                                
014503     READ W6118D INTO IN-AREA                                             
014504     AT END                                                               
014505        MOVE HIGH-VALUE TO IN-AREA                                        
014506        SET END-OF-W6118D TO TRUE                                         
014507                                                                          
014508     NOT AT END                                                           
014509        MOVE 'W6118D' TO POSTSUM-FDNAMN                                   
014510        MOVE 'W6118ED1' TO POSTSUM-DDNAMN2                                
014511*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
014512        MOVE SPACE     TO POSTSUM-TRANSTYP                                
014513        CALL POSTSUM USING POSTSUM-PARM                                   
014514     END-READ                                                             
014520     .                                                                    
014601     EJECT                                                                
014602 S11-WRITE-W6118E SECTION.                                                
014604     WRITE UT-RECORD FROM UT-AREA                                         
014606     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
014607     MOVE 'W6118E' TO POSTSUM-FDNAMN                                      
014608     MOVE 'W6118ED2' TO POSTSUM-DDNAMN2                                   
014609     CALL POSTSUM USING POSTSUM-PARM                                      
014610     .                                                                    
014800     EJECT                                                                
014900 S99-ABEND SECTION.                                                       
015102     MOVE 'S' TO POSTSUM-OPKOD                                            
015110     CALL POSTSUM USING POSTSUM-PARM                                      
015200     CALL ABEND USING RKOD-ABEND                                          
015300     .                                                                    
015400     EJECT                                                                
015500* --- IMS SECTIONS  ---                                                   
015600                                                                          
015701     EJECT                                                                
015702 IMS-GU-WDT201 SECTION.                                                   
015704     STRING 'WDT201  (BEFT     =' W-BEFT-X ')'                            
015705          DELIMITED BY SIZE INTO SSA1                                     
015706     MOVE '  GE' TO GOOD-STATUSCODES                                      
015707     CALL CBLTDLI USING GU WDT2-PCB DLI-IO-WDT201 SSA1                    
015708     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
015709     PERFORM IMS-STATUSCHECK                                              
015710     .                                                                    
015711     EJECT                                                                
015712 IMS-GNP-WDT211 SECTION.                                                  
015713**   STRING 'WDT201  (BEFT     =' W-BEFT-X ')'                            
015714**        DELIMITED BY SIZE INTO SSA1                                     
015715     STRING 'WDT211  (IDARTNR  =' W-IDARTNR-X ')'                         
015716          DELIMITED BY SIZE INTO SSA1                                     
015717     MOVE '  GE' TO GOOD-STATUSCODES                                      
015718     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT211 SSA1                   
015719     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
015720     PERFORM IMS-STATUSCHECK                                              
015730     .                                                                    
015731 IMS-GNP-WDT212 SECTION.                                                  
015732**   STRING 'WDT201  (BEFT     =' W-BEFT-X ')'                            
015733**        DELIMITED BY SIZE INTO SSA1                                     
015734     STRING 'WDT212  (IDFKNGRF<=' W-IDFKNGRF-X                            
015735                    '&IDFKNGRT>=' W-IDFKNGRT-X ')'                        
015736          DELIMITED BY SIZE INTO SSA1                                     
015737     MOVE '  GE' TO GOOD-STATUSCODES                                      
015738     CALL CBLTDLI USING GNP WDT2-PCB DLI-IO-WDT212 SSA1                   
015739     MOVE WDT2-STATUS-CODE TO STATUS-WS                                   
015740     PERFORM IMS-STATUSCHECK                                              
015741     .                                                                    
015742     EJECT                                                                
015743 IMS-GU-WDK611-PATH SECTION.                                              
015746     STRING 'WDK601  *D(IDARTNR  =' W-IDARTNR-X ')'                       
015747          DELIMITED BY SIZE INTO SSA1                                     
015748     MOVE 'WDK611'       TO SSA2                                          
015749     MOVE '  GE' TO GOOD-STATUSCODES                                      
015750     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601-11 SSA1 SSA2            
015751     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
015752     PERFORM IMS-STATUSCHECK                                              
015753     .                                                                    
015754     EJECT                                                                
015900 IMS-STATUSCHECK SECTION.                                                 
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GOOD-STATUS                                                   
016300       AT END                                                             
016400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016500           DELIMITED BY SIZE INTO ERROR-TEXT                              
016600         DISPLAY ERROR-TEXT                                               
016700         CALL FELLOG                                                      
016800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
